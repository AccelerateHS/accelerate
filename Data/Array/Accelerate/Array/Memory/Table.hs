{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Array.Memory.Table
-- Copyright   : [2008..2014] Manuel M T Chakravarty, Gabriele Keller
--               [2009..2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Accelerate backends often need to copy arrays to a remote memory before they
-- can be used in computation. This module provides an automated method for
-- doing so. Keeping track of arrays in a `MemoryTable` ensures that any memory
-- allocated for them will be freed when GHC's garbage collector collects the
-- host array.
--
module Data.Array.Accelerate.Array.Memory.Table (

  -- Tables for host/device memory associations
  MemoryTable, new, lookup, malloc, free, freeStable, insertUnmanaged, reclaim,

  StableArray, makeStableArray,

  -- Weak pointers of arrays
  makeWeakArrayData

) where

import Prelude                                                  hiding ( lookup )
import Data.Array.Storable.Internals                            ( StorableArray(..) )
import Data.Functor                                             ( (<$>) )
import Data.Maybe                                               ( isJust )
import Data.Proxy
import Data.Typeable                                            ( Typeable, gcast )
import Control.Monad                                            ( when )
import Control.Monad.IO.Class                                   ( MonadIO, liftIO )
import Control.Concurrent                                       ( yield )
import Control.Concurrent.MVar                                  ( MVar, newMVar, withMVar, modifyMVar_, mkWeakMVar )
import System.Mem                                               ( performGC )
import System.Mem.Weak                                          ( Weak, deRefWeak )
import Foreign.Storable                                         ( sizeOf )

import GHC.Exts                                                 ( Ptr(..) )
import GHC.ForeignPtr                                           ( ForeignPtr(..), ForeignPtrContents(..) )
import GHC.IORef                                                ( IORef(..) )
import GHC.STRef                                                ( STRef(..) )
import GHC.Base                                                 ( mkWeak#, mkWeakNoFinalizer#, IO(..) )
import GHC.Weak                                                 ( Weak(..) )

import qualified Data.HashTable.IO                              as HT

import Data.Array.Accelerate.Error                              ( internalError )
import Data.Array.Accelerate.Array.Data                         ( ArrayData, GArrayData(..),
                                                                  ArrayPtrs, ArrayElt, arrayElt, ArrayEltR(..),
                                                                  UniqueArray, storableFromUnique, getUniqueId )
import Data.Array.Accelerate.Array.Memory                       ( RemoteMemory, RemotePointer, PrimElt )
import Data.Array.Accelerate.Array.Memory.Nursery               ( Nursery(..) )
import qualified Data.Array.Accelerate.Array.Memory             as M
import qualified Data.Array.Accelerate.Array.Memory.Nursery     as N
import qualified Data.Array.Accelerate.Debug                    as D


-- We use an MVar to the hash table, so that several threads may safely access
-- it concurrently. This includes the finalisation threads that remove entries
-- from the table.
--
-- It is important that we can garbage collect old entries from the table when
-- the key is no longer reachable in the heap. Hence the value part of each
-- table entry is a (Weak val), where the stable name 'key' is the key for the
-- memo table, and the 'val' is the value of this table entry. When the key
-- becomes unreachable, a finaliser will fire and remove this entry from the
-- hash buckets, and further attempts to dereference the weak pointer will
-- return Nothing. References from 'val' to the key are ignored (see the
-- semantics of weak pointers in the documentation).
--
type HashTable key val  = HT.BasicHashTable key val
type MT p               = MVar ( HashTable StableArray (RemoteArray p) )
data MemoryTable p      = MemoryTable {-# UNPACK #-} !(MT p)
                                      {-# UNPACK #-} !(Weak (MT p))
                                      {-# UNPACK #-} !(Nursery p)
                                      (forall a. p a -> IO ())

-- | An untyped reference to an array, similar to a stablename.
--
type StableArray = Int

data RemoteArray p where
  RemoteArray :: Typeable e
              => {-# UNPACK #-} !(Weak ()) -- Keep track of host array liveness.
              -> p e                       -- The actual remote pointer
              -> Int                       -- The array size in bytes
              -> RemoteArray p


-- At what percentage of available memory should we consider it empty.
--
-- RCE: This is highly necessary with the CUDA backend as OSX's (and presumably
-- other OSes) UI doesn't handle GPU memory exhaustion very well (it crashes).
--
-- RCE: Experimentation should be done to come up with a better value.
threshold :: Float
threshold = 5.0

-- |Create a new memory table from host to remote arrays.
--
-- The function supplied should be the `free` for the remote pointers being
-- stored. This function will be called by the GC, which typically runs on a
-- different thread. Unlike the `free` in `RemoteMemory`, this function cannot
-- depend on any state.
--
new :: (RemoteMemory m, MonadIO m) => (forall a. RemotePointer m a -> IO ()) -> m (MemoryTable (RemotePointer m))
new free = do
  message "initialise memory table"
  tbl  <- liftIO $ HT.new
  ref  <- liftIO $ newMVar tbl
  nrs  <- N.new free
  weak <- liftIO $ mkWeakMVar ref (return ())
  return $! MemoryTable ref weak nrs free


-- | Look for the remote pointer corresponding to a given host-side array.
--
lookup :: (PrimElt a b) => MemoryTable p -> ArrayData a -> IO (Maybe (p b))
lookup (MemoryTable !ref _ _ _) !arr = do
  sa <- makeStableArray arr
  mw <- withMVar ref (`HT.lookup` sa)
  case mw of
    Nothing              -> trace ("lookup/not found: " ++ show sa) $ return Nothing
    Just (RemoteArray w p _) -> do
      mv <- deRefWeak w
      case mv of
        Just _ | Just p' <- gcast p -> trace ("lookup/found: " ++ show sa) $ return (Just p')
               | otherwise          -> $internalError "lookup" $ "type mismatch"

        -- Note: [Weak pointer weirdness]
        --
        -- After the lookup is successful, there might conceivably be no further
        -- references to 'arr'. If that is so, and a garbage collection
        -- intervenes, the weak pointer might get tombstoned before 'deRefWeak'
        -- gets to it. In that case we throw an error (below). However, because
        -- we have used 'arr' in the continuation, this ensures that 'arr' is
        -- reachable in the continuation of 'deRefWeak' and thus 'deRefWeak'
        -- always succeeds. This sort of weirdness, typical of the world of weak
        -- pointers, is why we can not reuse the stable name 'sa' computed
        -- above in the error message.
        --
        Nothing                    ->
          makeStableArray  arr >>= \x -> $internalError "lookup" $ "dead weak pair: " ++ show x


-- | Allocate a new device array to be associated with the given host-side array.
-- This may not always use the `malloc` provided by the `RemoteMemory` instance.
-- In order to reduce the number of raw allocations, previously allocated remote
-- arrays will be re-used. In the event that the remote memory is exhausted,
-- 'Nothing' is returned.
--
malloc :: forall a b m. (PrimElt a b, RemoteMemory m, MonadIO m)
       => MemoryTable (RemotePointer m)
       -> ArrayData a
       -> Int
       -> m (Maybe (RemotePointer m b))
malloc mt@(MemoryTable _ _ !nursery _) !ad !n = do
  -- Note: [Allocation sizes]
  --
  -- Instead of allocating the exact number of elements requested, we round up to
  -- a fixed chunk size; currently set at 128 elements. This means there is a
  -- greater chance the nursery will get a hit, and moreover that we can search
  -- the nursery for an exact size.
  --
  -- TLM: I believe the CUDA API allocates in chunks, of size 4MB.
  --
  chunk <- M.chunkSize
  let -- next highest multiple of f from x
      multiple x f      = (x + (f-1)) `div` f

      !n'               = chunk * multiple n chunk
      !bytes            = n' * sizeOf (undefined :: b)
  --
  message ("malloc: " ++ showBytes bytes)
  mp <-
      attempt "malloc/nursery" (liftIO $ fmap (M.castPtr (Proxy :: Proxy m)) <$> N.malloc bytes nursery)
    `orElse` attempt "malloc/new" (do
      left <- percentAvailable
      when (left < threshold) $ reclaim mt
      M.malloc n')
    `orElse` (do
      message "malloc/remote-malloc-failed (cleaning)"
      clean mt
      liftIO $ fmap (M.castPtr (Proxy :: Proxy m)) <$> N.malloc bytes nursery)
    `orElse` (do
      message "malloc/remote-malloc-failed (purging)"
      purge mt
      M.malloc n')
    `orElse` (do
      message "malloc/remote-malloc-failed (non-recoverable)"
      return Nothing)

  case mp of
    Nothing -> return Nothing
    Just p' -> do
      insert mt ad p' bytes
      return (Just p')

  where
    orElse :: m (Maybe x) -> m (Maybe x) -> m (Maybe x)
    orElse ra rb = do
      ma <- ra
      case ma of
        Nothing -> rb
        Just a  -> return (Just a)

    attempt :: String -> m (Maybe x) -> m (Maybe x)
    attempt msg next = do
      ma <- next
      case ma of
        Nothing -> return Nothing
        Just a  -> trace msg (return (Just a))



-- | Deallocate the device array associated with the given host-side array.
-- Typically this should only be called in very specific circumstances.
--
free :: (RemoteMemory m, PrimElt a b) => proxy m ->  MemoryTable (RemotePointer m) -> ArrayData a -> IO ()
free proxy mt !arr = do
  sa <- makeStableArray arr
  freeStable proxy mt sa

-- | Deallocate the device array associated with the given StableArray. This
-- is useful for other memory managers built on top of the memory table.
--
freeStable :: RemoteMemory m => proxy m -> MemoryTable (RemotePointer m) -> StableArray -> IO ()
freeStable proxy (MemoryTable !ref _ nrs _) !sa = withMVar ref $ \mt -> do
  mw <-  mt `HT.lookup` sa
  case mw of
    Nothing -> message ("free/not found: " ++ show sa)
    Just r  -> trace   ("free/evict: " ++ show sa) $ do
      freeRemote proxy nrs r
      mt `HT.delete` sa

freeRemote :: RemoteMemory m => proxy m -> Nursery (RemotePointer m) -> RemoteArray (RemotePointer m) -> IO ()
freeRemote proxy (Nursery !nrs _) (RemoteArray _ !p !bytes) = N.stash proxy bytes nrs p


-- Record an association between a host-side array and a new device memory area.
-- The device memory will be freed when the host array is garbage collected.
--
insert :: forall m a b. (PrimElt a b, RemoteMemory m, MonadIO m)
       => MemoryTable (RemotePointer m)
       -> ArrayData a
       -> RemotePointer m b
       -> Int
       -> m ()
insert mt@(MemoryTable !ref _ _ _) !arr !ptr !bytes = do
  key  <- makeStableArray  arr
  weak <- liftIO $ makeWeakArrayData arr () (Just $ finalizer (Proxy :: Proxy m) mt key)
  message      $ "insert: " ++ show key
  liftIO $ withMVar ref $ \tbl -> HT.insert tbl key (RemoteArray weak ptr bytes)


-- |Record an association between a host-side array and a remote memory area
-- that was not allocated by accelerate. The remote memory will NOT be re-used
-- once the host-side array is garbage collected.
--
-- This typically only has use for backends that provide an FFI.
--
insertUnmanaged :: (PrimElt a b, MonadIO m) => MemoryTable p -> ArrayData a -> p b -> m ()
insertUnmanaged (MemoryTable !ref !weak_ref _ _) !arr !ptr = do
  key  <- makeStableArray  arr
  weak <- liftIO $ makeWeakArrayData arr () (Just $ remoteFinalizer weak_ref key)
  message $ "insertUnmanaged: " ++ show key
  liftIO $ withMVar ref $ \tbl -> HT.insert tbl key (RemoteArray weak ptr 0)

percentAvailable :: (RemoteMemory m, MonadIO m) => m Float
percentAvailable = do
  left  <- M.availableMem
  total <- M.totalMem
  return (100.0 * fromIntegral left / fromIntegral total)


-- Removing entries
-- ----------------

-- |Initiate garbage collection and mark any arrays that no longer have host-side
-- equivalents as reusable.
--
clean :: forall m. (RemoteMemory m, MonadIO m) => MemoryTable (RemotePointer m) -> m ()
clean mt@(MemoryTable _ weak_ref nrs _) = management "clean" nrs . liftIO $ do
  -- Unforunately there is no real way to force a GC then wait for it to finsh.
  -- Calling performGC then yielding works moderately well in single-threaded
  -- cases, but tends to fall down otherwise. Either way, given that finalizers
  -- are often significantly delayed, it is worth our while traversing the table
  -- and explicitly freeing any dead entires.
  --
  performGC
  yield
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> return ()
    Just ref -> do
      rs <- withMVar ref $ HT.foldM removable []  -- collect arrays that can be removed
      mapM_ (freeStable (Proxy :: Proxy m) mt) rs -- remove them all
  where
    removable rs (sa, RemoteArray w _ _) = do
      alive <- isJust <$> deRefWeak w
      if alive then return rs else return (sa:rs)

-- |Call `free` on all arrays that are not currently associated with host-side
-- arrays.
--
purge :: (RemoteMemory m, MonadIO m) => MemoryTable (RemotePointer m) -> m ()
purge (MemoryTable _ _ nursery@(Nursery nrs _) free) = management "purge" nursery . liftIO $
  modifyMVar_ nrs (\(tbl,_) -> N.flush free tbl >> return (tbl, 0))

-- |Initiate garbage collection and `free` any remote arrays that no longer
-- have matching host-side equivalents.
--
reclaim :: forall m. (RemoteMemory m, MonadIO m) => MemoryTable (RemotePointer m) -> m ()
reclaim mt = clean mt >> purge mt

-- Because a finaliser might run at any time, we must reinstate the context in
-- which the array was allocated before attempting to release it.
--
-- Note also that finaliser threads will silently terminate if an exception is
-- raised. If the context, and thereby all allocated memory, was destroyed
-- externally before the thread had a chance to run, all we need do is update
-- the hash tables --- but we must do this first before failing to use a dead
-- context.
--
finalizer :: (RemoteMemory m, MonadIO m)
          => proxy m
          -> MemoryTable (RemotePointer m)
          -> StableArray
          -> IO ()
finalizer proxy (MemoryTable !ref _ (Nursery !nrs _) _) !key = do
  withMVar ref $ \mt -> do
    mr <- HT.lookup mt key
    case mr of
      Nothing -> message ("finalize/already-removed: " ++ show key)
      Just (RemoteArray _ ptr bytes) -> do
        N.stash proxy bytes nrs ptr
        HT.delete mt key

remoteFinalizer :: Weak (MT p) -> StableArray -> IO ()
remoteFinalizer !weak_ref !key = do
  mr <- deRefWeak weak_ref
  case mr of
    Nothing  -> message ("finalise/dead table: " ++ show key)
    Just ref -> trace   ("finalise: "            ++ show key) $ withMVar ref (`HT.delete` key)

-- Miscellaneous
-- -------------

-- | Make a new StableArray.
{-# INLINE makeStableArray #-}
makeStableArray :: (MonadIO m, Typeable a, Typeable e, ArrayPtrs a ~ Ptr e, ArrayElt a) => ArrayData a -> m StableArray
makeStableArray !ad = liftIO $ id arrayElt ad
  where
    id :: ArrayEltR e -> ArrayData e -> IO Int
    id ArrayEltRint     (AD_Int ua)     = getUniqueId ua
    id ArrayEltRint8    (AD_Int8 ua)    = getUniqueId ua
    id ArrayEltRint16   (AD_Int16 ua)   = getUniqueId ua
    id ArrayEltRint32   (AD_Int32 ua)   = getUniqueId ua
    id ArrayEltRint64   (AD_Int64 ua)   = getUniqueId ua
    id ArrayEltRword    (AD_Word ua)    = getUniqueId ua
    id ArrayEltRword8   (AD_Word8 ua)   = getUniqueId ua
    id ArrayEltRword16  (AD_Word16 ua)  = getUniqueId ua
    id ArrayEltRword32  (AD_Word32 ua)  = getUniqueId ua
    id ArrayEltRword64  (AD_Word64 ua)  = getUniqueId ua
    id ArrayEltRcshort  (AD_CShort ua)  = getUniqueId ua
    id ArrayEltRcushort (AD_CUShort ua) = getUniqueId ua
    id ArrayEltRcint    (AD_CInt ua)    = getUniqueId ua
    id ArrayEltRcuint   (AD_CUInt ua)   = getUniqueId ua
    id ArrayEltRclong   (AD_CLong ua)   = getUniqueId ua
    id ArrayEltRculong  (AD_CULong ua)  = getUniqueId ua
    id ArrayEltRcllong  (AD_CLLong ua)  = getUniqueId ua
    id ArrayEltRcullong (AD_CULLong ua) = getUniqueId ua
    id ArrayEltRfloat   (AD_Float ua)   = getUniqueId ua
    id ArrayEltRdouble  (AD_Double ua)  = getUniqueId ua
    id ArrayEltRcfloat  (AD_CFloat ua)  = getUniqueId ua
    id ArrayEltRcdouble (AD_CDouble ua) = getUniqueId ua
    id ArrayEltRbool    (AD_Bool ua)    = getUniqueId ua
    id ArrayEltRchar    (AD_Char ua)    = getUniqueId ua
    id ArrayEltRcchar   (AD_CChar ua)   = getUniqueId ua
    id ArrayEltRcschar  (AD_CSChar ua)  = getUniqueId ua
    id ArrayEltRcuchar  (AD_CUChar ua)  = getUniqueId ua
    id _                _               = error "I do have a cause, though. It is obscenity. I'm for it."

-- Weak arrays
-- ----------------------

-- |Make a weak pointer using an array as a key. Unlike the stanard `mkWeak`,
-- this guarantees finalisers won't fire early.
makeWeakArrayData :: forall a e c. (ArrayElt e, ArrayPtrs e ~ Ptr a) => ArrayData e -> c -> Maybe (IO ()) -> IO (Weak c)
makeWeakArrayData ad c f = mw arrayElt ad
  where
    mw :: ArrayEltR e -> ArrayData e -> IO (Weak c)
    mw ArrayEltRint     (AD_Int ua)     = mkWeak' ua c f
    mw ArrayEltRint8    (AD_Int8 ua)    = mkWeak' ua c f
    mw ArrayEltRint16   (AD_Int16 ua)   = mkWeak' ua c f
    mw ArrayEltRint32   (AD_Int32 ua)   = mkWeak' ua c f
    mw ArrayEltRint64   (AD_Int64 ua)   = mkWeak' ua c f
    mw ArrayEltRword    (AD_Word ua)    = mkWeak' ua c f
    mw ArrayEltRword8   (AD_Word8 ua)   = mkWeak' ua c f
    mw ArrayEltRword16  (AD_Word16 ua)  = mkWeak' ua c f
    mw ArrayEltRword32  (AD_Word32 ua)  = mkWeak' ua c f
    mw ArrayEltRword64  (AD_Word64 ua)  = mkWeak' ua c f
    mw ArrayEltRcshort  (AD_CShort ua)  = mkWeak' ua c f
    mw ArrayEltRcushort (AD_CUShort ua) = mkWeak' ua c f
    mw ArrayEltRcint    (AD_CInt ua)    = mkWeak' ua c f
    mw ArrayEltRcuint   (AD_CUInt ua)   = mkWeak' ua c f
    mw ArrayEltRclong   (AD_CLong ua)   = mkWeak' ua c f
    mw ArrayEltRculong  (AD_CULong ua)  = mkWeak' ua c f
    mw ArrayEltRcllong  (AD_CLLong ua)  = mkWeak' ua c f
    mw ArrayEltRcullong (AD_CULLong ua) = mkWeak' ua c f
    mw ArrayEltRfloat   (AD_Float ua)   = mkWeak' ua c f
    mw ArrayEltRdouble  (AD_Double ua)  = mkWeak' ua c f
    mw ArrayEltRcfloat  (AD_CFloat ua)  = mkWeak' ua c f
    mw ArrayEltRcdouble (AD_CDouble ua) = mkWeak' ua c f
    mw ArrayEltRbool    (AD_Bool ua)    = mkWeak' ua c f
    mw ArrayEltRchar    (AD_Char ua)    = mkWeak' ua c f
    mw ArrayEltRcchar   (AD_CChar ua)   = mkWeak' ua c f
    mw ArrayEltRcschar  (AD_CSChar ua)  = mkWeak' ua c f
    mw ArrayEltRcuchar  (AD_CUChar ua)  = mkWeak' ua c f
    mw _                _               = error "Base eight is just like base ten really — if you're missing two fingers."

    -- Note: [Weak Array pointers]
    --
    -- One of the unfortunate properties of GHC's weak pointers is that if a
    -- weak pointer is created with a non-primitive object as key, there is
    -- the possibility that the finalizer attached to the pointer may fire early.
    -- The reason for this is that the optimiser, at compile time, and the GC, at
    -- runtime, are free to create copies of the objects they are attached to.
    -- This is less than ideal if we want to properly track when arrays are no
    -- longer reachable.
    --
    -- The solution to this problem is to use a primitive object as a key for
    -- any weak pointers we create. However, the obvious choice of primitive,
    -- the `Addr#` that points to the pinned payload of the array, is not
    -- suitable. Being a pointer into non-GHC managed memory, the rts won't let
    -- us use it as a key. Instead we use the `MutVar#` contained within an
    -- IORef, itself part of the ForeignPtr contained within a StorableArray.
    -- This means that GHC is free to create as many copies of the container
    -- and any finalizers will not fire until all copies have been made
    -- unreachable
    --
    mkWeak' :: UniqueArray i a -> c -> Maybe (IO ()) -> IO (Weak c)
    mkWeak' (storableFromUnique -> StorableArray _ _ _ (ForeignPtr _ (MallocPtr _ (IORef (STRef r#))))) c (Just f)
      = IO $ \s ->
          case mkWeak# r# c f s of (# s1, w #) -> (# s1, Weak w #)
    mkWeak' (storableFromUnique -> StorableArray _ _ _ (ForeignPtr _ (MallocPtr _ (IORef (STRef r#))))) c Nothing
      = IO $ \s ->
          case mkWeakNoFinalizer# r# c s of (# s1, w #) -> (# s1, Weak w #)
    mkWeak' _                                                                     _ _
      = $internalError "makeWeakArrayData" "Internal representation of Storable array has changed"


-- Debug
-- -----

{-# INLINE showBytes #-}
showBytes :: Integral n => n -> String
showBytes x = D.showFFloatSIBase (Just 0) 1024 (fromIntegral x :: Double) "B"

{-# INLINE trace #-}
trace :: MonadIO m => String -> m a -> m a
trace msg next = message msg >> next

{-# INLINE message #-}
message :: MonadIO m => String -> m ()
message msg = liftIO $ D.traceIO D.dump_gc ("gc: " ++ msg)

{-# INLINE management #-}
management :: (RemoteMemory m, MonadIO m) => String -> Nursery p -> m a -> m a
management msg nrs next = do
  before     <- M.availableMem
  before_nrs <- liftIO $ N.size nrs
  total      <- M.totalMem
  r          <- next
  D.when D.dump_gc $ do
    after     <- M.availableMem
    after_nrs <- liftIO $ N.size nrs
    message $ msg ++ " (freed: "     ++ showBytes (after - before)
                  ++ ", stashed: "   ++ showBytes (before_nrs - after_nrs)
                  ++ ", remaining: " ++ showBytes after
                  ++ " of "          ++ showBytes total ++ ")"
  return r
