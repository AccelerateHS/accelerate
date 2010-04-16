{-# LANGUAGE GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, MagicHash, UnboxedTuples #-}

module Data.Array.Accelerate.CUDA.Data (
 
  ArrayElem(..)
) where

-- standard libraries

import Control.Monad.State (get, liftIO, put)
import Data.Map (adjust, delete, insert, lookup, notMember)
import Foreign (Ptr, nullPtr)
import Foreign.Ptr (ptrToWordPtr)
import Foreign.Storable (Storable)

import GHC.Int
import GHC.Ptr (Ptr(Ptr))

import qualified Data.Array.Accelerate.Array.Data as Acc
import qualified Data.Array.Accelerate.CUDA.Monad as CUDA
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Type
import Foreign.Ptr

import qualified Foreign.CUDA.Driver              as CUDA
import qualified Foreign.CUDA.Driver.Stream       as CUDA

class Acc.ArrayElem e => ArrayElem e where
  type DevicePtrs e
  type HostPtrs   e
  mallocArray      :: Acc.ArrayData e -> Int -> CUDA.CGIO ()
  pokeArray        :: Acc.ArrayData e -> Int -> CUDA.CGIO ()
  pokeArrayAsync   :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CUDA.CGIO ()
  peekArray        :: Acc.ArrayData e -> Int -> CUDA.CGIO ()
  peekArrayAsync   :: Acc.ArrayData e -> Int -> Maybe CUDA.Stream -> CUDA.CGIO ()
  free             :: Acc.ArrayData e -> CUDA.CGIO ()
  toFunParams      :: Acc.ArrayData e -> CUDA.CGIO [CUDA.FunParam]
  decUse           :: Acc.ArrayData e -> CUDA.CGIO Int

instance ArrayElem () where
  type DevicePtrs () = CUDA.DevicePtr ()
  type HostPtrs   () = CUDA.HostPtr   ()
  mallocArray     _ _   = return ()
  pokeArray       _ _   = return ()
  pokeArrayAsync  _ _ _ = return ()
  peekArray       _ _   = return ()
  peekArrayAsync  _ _ _ = return ()
  free            _     = return ()
  toFunParams     _     = return []
  decUse            _   = return 0

instance ArrayElem Int where
  type DevicePtrs Int = CUDA.DevicePtr Int
  type HostPtrs   Int = CUDA.HostPtr   Int
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Int)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Int8 where
  type DevicePtrs Int8 = CUDA.DevicePtr Int8
  type HostPtrs   Int8 = CUDA.HostPtr   Int8
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Int8)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Int16 where
  type DevicePtrs Int16 = CUDA.DevicePtr Int16
  type HostPtrs   Int16 = CUDA.HostPtr   Int16
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Int16)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Int32 where
  type DevicePtrs Int32 = CUDA.DevicePtr Int32
  type HostPtrs   Int32 = CUDA.HostPtr   Int32
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Int32)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Int64 where
  type DevicePtrs Int64 = CUDA.DevicePtr Int64
  type HostPtrs   Int64 = CUDA.HostPtr   Int64
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Int64)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Word where
  type DevicePtrs Word = CUDA.DevicePtr Word
  type HostPtrs   Word = CUDA.HostPtr   Word
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Word)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Word8 where
  type DevicePtrs Word8 = CUDA.DevicePtr Word8
  type HostPtrs   Word8 = CUDA.HostPtr   Word8
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Word8)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Word16 where
  type DevicePtrs Word16 = CUDA.DevicePtr Word16
  type HostPtrs   Word16 = CUDA.HostPtr   Word16
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Word16)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Word32 where
  type DevicePtrs Word32 = CUDA.DevicePtr Word32
  type HostPtrs   Word32 = CUDA.HostPtr   Word32
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Word32)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Word64 where
  type DevicePtrs Word64 = CUDA.DevicePtr Word64
  type HostPtrs   Word64 = CUDA.HostPtr   Word64
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Word64)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

-- FIXME:
-- CShort
-- CUShort
-- CInt
-- CUInt
-- CLong
-- CULong
-- CLLong
-- CULLong

instance ArrayElem Float where
  type DevicePtrs Float = CUDA.DevicePtr Float
  type HostPtrs   Float = CUDA.HostPtr   Float
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Float)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Double where
  type DevicePtrs Double = CUDA.DevicePtr Double
  type HostPtrs   Double = CUDA.HostPtr   Double
  mallocArray ad n = do
    currentState <- get
    dptr <- liftIO $ CUDA.mallocArray n :: CUDA.CGIO (DevicePtrs Double)
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
        val    = CUDA.MemoryEntry (CUDA.devPtrToWordPtr dptr) 0
    put $ currentState
      {CUDA.memMap = insert key val memMap}
  pokeArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArray n aptr dptr
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error
        "A pokeArray failed due to loss of memory allocation information."
  pokeArrayAsync ad n s= do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        if CUDA.numUse val == 0
          then do
            let hptr = CUDA.HostPtr aptr
                dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
                val' = val {CUDA.numUse = 1}
            liftIO $ CUDA.pokeArrayAsync n hptr dptr s
            put $ currentState
              { CUDA.dataTransferHtoD = CUDA.dataTransferHtoD currentState + 1
              , CUDA.memMap = insert key val' memMap}
          else do
            let val' = val {CUDA.numUse = CUDA.numUse val + 1}
            put $ currentState {CUDA.memMap = insert key val' memMap}
      Nothing -> error $
        "A pokeArrayAsync failed due to loss of memory allocation " ++
        "information."
  peekArray ad n = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArray n dptr aptr
      Nothing -> error
        "A peekArray failed due to loss of memory allocation information."
  peekArrayAsync ad n s = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let hptr = CUDA.HostPtr aptr
            dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.peekArrayAsync n dptr hptr s
      Nothing -> error $
        "A peekArrayAsync failed due to loss of memory allocation " ++
        "information."
  free ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        liftIO $ CUDA.free dptr
        put $ currentState {CUDA.memMap = delete key memMap}
      Nothing -> error
        "A free failed due to loss of memory allocation information."
  toFunParams ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let dptr = CUDA.wordPtrToDevPtr $ CUDA.devicePtr val
        return [CUDA.VArg dptr]
      Nothing -> error
        "A toFunParams failed due to loss of memory allocation information."
  decUse ad = do
    currentState <- get
    let aptr   = Acc.ptrsOfArrayData ad
        memMap = CUDA.memMap currentState
        key    = ptrToWordPtr aptr
    case Data.Map.lookup key memMap of
      Just val -> do
        let numUse = CUDA.numUse val
        if numUse == 0
          then
            return numUse
          else do
            let numUse' = numUse - 1
                val' = val {CUDA.numUse = numUse'}
            put $ currentState
              { CUDA.memMap = insert key val' memMap}
            return numUse'
      Nothing  ->
        return 0

instance ArrayElem Bool where
  type DevicePtrs Bool = CUDA.DevicePtr Word8
  type HostPtrs   Bool = CUDA.HostPtr   Word8

instance ArrayElem Char where
  type DevicePtrs Char = CUDA.DevicePtr Char
  type HostPtrs   Char = CUDA.HostPtr   Char
  -- ???unicode???

-- FIXME:
-- CChar
-- CSChar
-- CUChar

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
  type DevicePtrs (a, b) = (DevicePtrs a, DevicePtrs b)
  type HostPtrs   (a, b) = (HostPtrs   a, HostPtrs   b)
  mallocArray ad n = do
    mallocArray (Acc.fstArrayData ad) n
    mallocArray (Acc.sndArrayData ad) n
  pokeArray ad n = do
    pokeArray (Acc.fstArrayData ad) n
    pokeArray (Acc.sndArrayData ad) n
  pokeArrayAsync ad n s = do
    pokeArrayAsync (Acc.fstArrayData ad) n s
    pokeArrayAsync (Acc.sndArrayData ad) n s
  peekArray ad n = do
    peekArray (Acc.fstArrayData ad) n
    peekArray (Acc.sndArrayData ad) n
  peekArrayAsync ad n s = do
    peekArrayAsync (Acc.fstArrayData ad) n s
    peekArrayAsync (Acc.sndArrayData ad) n s
  free ad = do
    free (Acc.fstArrayData ad)
    free (Acc.sndArrayData ad)
  toFunParams ad = do
    paramsA <- toFunParams (Acc.fstArrayData ad)
    paramsB <- toFunParams (Acc.sndArrayData ad)
    return $ paramsA ++ paramsB
  decUse ad = do
    decUseA <- decUse (Acc.fstArrayData ad)
    decUseB <- decUse (Acc.sndArrayData ad)
    return $ max decUseA decUseB
