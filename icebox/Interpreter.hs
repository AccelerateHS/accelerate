-- |
-- Module      : Data.Array.Accelerate.Interpreter
-- Description : Reference backend (interpreted)
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--


-- | Stream a lazily read list of input arrays through the given program,
-- collecting results as we go
--
streamOut :: Arrays a => Sugar.Seq [a] -> [a]
streamOut seq = let seq' = convertSeqWith config seq
                 in evalDelayedSeq defaultSeqConfig seq'


toSeqOp :: forall slix sl dim co e proxy. (Elt slix, Shape sl, Shape dim, Elt e)
        => SliceIndex (EltRepr slix)
                      (EltRepr sl)
                      co
                      (EltRepr dim)
        -> proxy slix
        -> Array dim e
        -> [Array sl e]
toSeqOp sliceIndex _ arr = map (sliceOp sliceIndex arr :: slix -> Array sl e)
                               (enumSlices sliceIndex (shape arr))

-- Sequence evaluation
-- ---------------

-- Position in sequence.
--
type SeqPos = Int

-- Configuration for sequence evaluation.
--
data SeqConfig = SeqConfig
  { chunkSize :: Int -- Allocation limit for a sequence in
                     -- words. Actual runtime allocation should be the
                     -- maximum of this size and the size of the
                     -- largest element in the sequence.
  }

-- Default sequence evaluation configuration for testing purposes.
--
defaultSeqConfig :: SeqConfig
defaultSeqConfig = SeqConfig { chunkSize = 2 }

type Chunk a = Vector' a

-- The empty chunk. O(1).
emptyChunk :: Arrays a => Chunk a
emptyChunk = empty'

-- Number of arrays in chunk. O(1).
--
clen :: Arrays a => Chunk a -> Int
clen = length'

elemsPerChunk :: SeqConfig -> Int -> Int
elemsPerChunk conf n
  | n < 1 = chunkSize conf
  | otherwise =
    let (a,b) = chunkSize conf `quotRem` n
    in a + signum b

-- Drop a number of arrays from a chunk. O(1). Note: Require keeping a
-- scan of element sizes.
--
cdrop :: Arrays a => Int -> Chunk a -> Chunk a
cdrop = drop' dropOp (fst . offsetsOp)

-- Get all the shapes of a chunk of arrays. O(1).
--
chunkShapes :: Chunk (Array sh a) -> Vector sh
chunkShapes = shapes'

-- Get all the elements of a chunk of arrays. O(1).
--
chunkElems :: Chunk (Array sh a) -> Vector a
chunkElems = elements'

-- Convert a vector to a chunk of scalars.
--
vec2Chunk :: Elt e => Vector e -> Chunk (Scalar e)
vec2Chunk = vec2Vec'

-- Convert a list of arrays to a chunk.
--
fromListChunk :: Arrays a => [a] -> Vector' a
fromListChunk = fromList' concatOp

-- Convert a chunk to a list of arrays.
--
toListChunk :: Arrays a => Vector' a -> [a]
toListChunk = toList' fetchAllOp

-- fmap for Chunk. O(n).
--   TODO: Use vectorised function.
mapChunk :: (Arrays a, Arrays b)
         => (a -> b)
         -> Chunk a -> Chunk b
mapChunk f c = fromListChunk $ map f (toListChunk c)

-- zipWith for Chunk. O(n).
--  TODO: Use vectorised function.
zipWithChunk :: (Arrays a, Arrays b, Arrays c)
             => (a -> b -> c)
             -> Chunk a -> Chunk b -> Chunk c
zipWithChunk f c1 c2 = fromListChunk $ zipWith f (toListChunk c1) (toListChunk c2)

-- A window on a sequence.
--
data Window a = Window
  { chunk :: Chunk a   -- Current allocated chunk.
  , wpos  :: SeqPos    -- Position of the window on the sequence, given
                       -- in number of elements.
  }

-- The initial empty window.
--
window0 :: Arrays a => Window a
window0 = Window { chunk = emptyChunk, wpos = 0 }

-- Index the given window by the given index on the sequence.
--
(!#) :: Arrays a => Window a -> SeqPos -> Chunk a
w !# i
  | j <- i - wpos w
  , j >= 0
  = cdrop j (chunk w)
  --
  | otherwise
  = error $ "Window indexed before position. wpos = " ++ show (wpos w) ++ " i = " ++ show i

-- Move the give window by supplying the next chunk.
--
moveWin :: Arrays a => Window a -> Chunk a -> Window a
moveWin w c = w { chunk = c
                , wpos = wpos w + clen (chunk w)
                }

-- A cursor on a sequence.
--
data Cursor senv a = Cursor
  { ref  :: Idx senv a -- Reference to the sequence.
  , cpos :: SeqPos     -- Position of the cursor on the sequence,
                       -- given in number of elements.
  }

-- Initial cursor.
--
cursor0 :: Idx senv a -> Cursor senv a
cursor0 x = Cursor { ref = x, cpos = 0 }

-- Advance cursor by a relative amount.
--
moveCursor :: Int -> Cursor senv a -> Cursor senv a
moveCursor k c = c { cpos = cpos c + k }

-- Valuation for an environment of sequence windows.
--
data Val' senv where
  Empty' :: Val' ()
  Push'  :: Val' senv -> Window t -> Val' (senv, t)

-- Projection of a window from a window valuation using a de Bruijn
-- index.
--
prj' :: Idx senv t -> Val' senv -> Window t
prj' ZeroIdx       (Push' _   v) = v
prj' (SuccIdx idx) (Push' val _) = prj' idx val

-- Projection of a chunk from a window valuation using a sequence
-- cursor.
--
prjChunk :: Arrays a => Cursor senv a -> Val' senv -> Chunk a
prjChunk c senv = prj' (ref c) senv !# cpos c

-- An executable sequence.
--
data ExecSeq senv arrs where
  ExecP :: Arrays a => Window a -> ExecP senv a -> ExecSeq (senv, a) arrs -> ExecSeq senv  arrs
  ExecC :: Arrays a =>             ExecC senv a ->                           ExecSeq senv  a
  ExecR :: Arrays a =>             Cursor senv a ->                          ExecSeq senv  [a]

-- An executable producer.
--
data ExecP senv a where
  ExecStreamIn :: Int
               -> [a]
               -> ExecP senv a

  ExecMap :: Arrays a
          => (Chunk a -> Chunk b)
          -> Cursor senv a
          -> ExecP senv b

  ExecZipWith :: (Arrays a, Arrays b)
              => (Chunk a -> Chunk b -> Chunk c)
              -> Cursor senv a
              -> Cursor senv b
              -> ExecP senv c

  -- Stream scan skeleton.
  ExecScan :: Arrays a
           => (s -> Chunk a -> (Chunk r, s)) -- Chunk scanner.
           -> s                              -- Accumulator (internal state).
           -> Cursor senv a                  -- Input stream.
           -> ExecP senv r

-- An executable consumer.
--
data ExecC senv a where

  -- Stream reduction skeleton.
  ExecFold :: Arrays a
           => (s -> Chunk a -> s) -- Chunk consumer function.
           -> (s -> r)            -- Finalizer function.
           -> s                   -- Accumulator (internal state).
           -> Cursor senv a       -- Input stream.
           -> ExecC senv r

  ExecStuple :: IsAtuple a
             => Atuple (ExecC senv) (TupleRepr a)
             -> ExecC senv a

minCursor :: ExecSeq senv a -> SeqPos
minCursor s = travS s 0
  where
    travS :: ExecSeq senv a -> Int -> SeqPos
    travS s i =
      case s of
        ExecP _ p s' -> travP p i `min` travS s' (i+1)
        ExecC   c    -> travC c i
        ExecR   _    -> maxBound

    k :: Cursor senv a -> Int -> SeqPos
    k c i
      | i == idxToInt (ref c) = cpos c
      | otherwise             = maxBound

    travP :: ExecP senv a -> Int -> SeqPos
    travP p i =
      case p of
        ExecStreamIn _ _ -> maxBound
        ExecMap _ c -> k c i
        ExecZipWith _ c1 c2 -> k c1 i `min` k c2 i
        ExecScan _ _ c -> k c i

    travT :: Atuple (ExecC senv) t -> Int -> SeqPos
    travT NilAtup        _ = maxBound
    travT (SnocAtup t c) i = travT t i `min` travC c i

    travC :: ExecC senv a -> Int -> SeqPos
    travC c i =
      case c of
        ExecFold _ _ _ cu -> k cu i
        ExecStuple t      -> travT t i


evalDelayedSeq
    :: SeqConfig
    -> DelayedSeq arrs
    -> arrs
evalDelayedSeq cfg (DelayedSeq aenv s) | aenv' <- evalExtend aenv Empty
                                       = evalSeq cfg s aenv'

evalSeq :: forall aenv arrs.
            SeqConfig
         -> PreOpenSeq DelayedOpenAcc aenv () arrs
         -> Val aenv -> arrs
evalSeq conf s aenv = evalSeq' s
  where
    evalSeq' :: PreOpenSeq DelayedOpenAcc aenv senv arrs -> arrs
    evalSeq' (Producer _ s) = evalSeq' s
    evalSeq' (Consumer _)   = loop (initSeq aenv s)
    evalSeq' (Reify _)      = reify (initSeq aenv s)

    -- Initialize the producers and the accumulators of the consumers
    -- with the given array enviroment.
    initSeq :: forall senv arrs'.
                Val aenv
             -> PreOpenSeq DelayedOpenAcc aenv senv arrs'
             -> ExecSeq senv arrs'
    initSeq aenv s =
      case s of
        Producer   p s' -> ExecP window0 (initProducer p) (initSeq aenv s')
        Consumer   c    -> ExecC         (initConsumer c)
        Reify      ix   -> ExecR (cursor0 ix)

    -- Generate a list from the sequence.
    reify :: forall arrs. ExecSeq () [arrs]
          -> [arrs]
    reify s = case step s Empty' of
                (Just s', a) -> a ++ reify s'
                (Nothing, a) -> a

    -- Iterate the given sequence until it terminates.
    -- A sequence only terminates when one of the producers are exhausted.
    loop :: Arrays arrs
         => ExecSeq () arrs
         -> arrs
    loop s =
      case step' s of
        (Nothing, arrs) -> arrs
        (Just s', _)    -> loop s'

      where
        step' :: ExecSeq () arrs -> (Maybe (ExecSeq () arrs), arrs)
        step' s = step s Empty'

    -- One iteration of a sequence.
    step :: forall senv arrs'.
            ExecSeq senv arrs'
         -> Val' senv
         -> (Maybe (ExecSeq senv arrs'), arrs')
    step s senv =
      case s of
        ExecP w p s' ->
          let (c, mp')  = produce p senv
              finished  = 0 == clen (w !# minCursor s')
              w'        = if finished then moveWin w c else w
              (ms'', a) = step s' (senv `Push'` w')
          in case ms'' of
            Nothing  -> (Nothing, a)
            Just s'' | finished
                     , Just p' <- mp'
                     -> (Just (ExecP w' p' s''), a)
                     | not finished
                     -> (Just (ExecP w' p  s''), a)
                     | otherwise
                     -> (Nothing, a)
        ExecC   c    -> let (c', acc) = consume c senv
                        in (Just (ExecC c'), acc)
        ExecR ix     -> let c = prjChunk ix senv in (Just (ExecR (moveCursor (clen c) ix)), toListChunk c)

    evalA :: DelayedOpenAcc aenv a -> a
    evalA acc = evalOpenAcc acc aenv

    evalAF :: DelayedOpenAfun aenv f -> f
    evalAF f = evalOpenAfun f aenv

    evalE :: DelayedExp aenv t -> t
    evalE exp = evalExp exp aenv

    evalF :: DelayedFun aenv f -> f
    evalF fun = evalFun fun aenv

    initProducer :: forall a senv.
                    Producer DelayedOpenAcc aenv senv a
                 -> ExecP senv a
    initProducer p =
      case p of
        StreamIn arrs -> ExecStreamIn 1 arrs
        ToSeq sliceIndex slix (delayed -> Delayed sh ix _) ->
          let n   = R.size (R.sliceShape sliceIndex (fromElt sh))
              k   = elemsPerChunk conf n
          in ExecStreamIn k (toSeqOp sliceIndex slix (fromFunction sh ix))
        MapSeq     f x       -> ExecMap     (mapChunk (evalAF f)) (cursor0 x)
        ChunkedMapSeq f x    -> ExecMap     (evalAF f) (cursor0 x)
        ZipWithSeq f x y     -> ExecZipWith (zipWithChunk (evalAF f)) (cursor0 x) (cursor0 y)
        ScanSeq    f e x     -> ExecScan scanner (evalE e) (cursor0 x)
          where
            scanner a c =
              let v0 = chunkElems c
                  (v1, a') = scanl'Op (evalF f) a (delayArray v0)
              in (vec2Chunk v1, fromScalar a')

    initConsumer :: forall a senv.
                    Consumer DelayedOpenAcc aenv senv a
                 -> ExecC senv a
    initConsumer c =
      case c of
        FoldSeq f e x ->
          let f' = evalF f
              a0 = fromFunction (Z :. chunkSize conf) (const (evalE e))
              consumer v c = zipWith'Op f' (delayArray v) (delayArray (chunkElems c))
              finalizer = fold1Op f' . delayArray
          in ExecFold consumer finalizer a0 (cursor0 x)
        FoldSeqFlatten f acc x ->
          let f' = evalAF f
              a0 = evalA acc
              consumer a c = f' a (chunkShapes c) (chunkElems c)
          in ExecFold consumer id a0 (cursor0 x)
        Stuple t ->
          let initTup :: Atuple (Consumer DelayedOpenAcc aenv senv) t -> Atuple (ExecC senv) t
              initTup NilAtup        = NilAtup
              initTup (SnocAtup t c) = SnocAtup (initTup t) (initConsumer c)
          in ExecStuple (initTup t)

    delayed :: DelayedOpenAcc aenv (Array sh e) -> Delayed (Array sh e)
    delayed AST.Manifest{}  = $internalError "evalOpenAcc" "expected delayed array"
    delayed AST.Delayed{..} = Delayed (evalExp extentD aenv)
                                      (evalFun indexD aenv)
                                      (evalFun linearIndexD aenv)

produce :: Arrays a => ExecP senv a -> Val' senv -> (Chunk a, Maybe (ExecP senv a))
produce p senv =
  case p of
    ExecStreamIn k xs ->
      let (xs', xs'') = (take k xs, drop k xs)
          c           = fromListChunk xs'
          mp          = if null xs''
                        then Nothing
                        else Just (ExecStreamIn k xs'')
      in (c, mp)
    ExecMap f x ->
      let c = prjChunk x senv
      in (f c, Just $ ExecMap f (moveCursor (clen c) x))
    ExecZipWith f x y ->
      let c1 = prjChunk x senv
          c2 = prjChunk y senv
          k = clen c1 `min` clen c2
      in (f c1 c2, Just $ ExecZipWith f (moveCursor k x) (moveCursor k y))
    ExecScan scanner a x ->
      let c = prjChunk x senv
          (c', a') = scanner a c
          k = clen c
      in (c', Just $ ExecScan scanner a' (moveCursor k x))

consume :: forall senv a. ExecC senv a -> Val' senv -> (ExecC senv a, a)
consume c senv =
  case c of
    ExecFold f g acc x ->
      let c    = prjChunk x senv
          acc' = f acc c
      -- Even though we call g here, lazy evaluation should guarantee it is
      -- only ever called once.
      in (ExecFold f g acc' (moveCursor (clen c) x), g acc')
    ExecStuple t ->
      let consT :: Atuple (ExecC senv) t -> (Atuple (ExecC senv) t, t)
          consT NilAtup        = (NilAtup, ())
          consT (SnocAtup t c) | (c', acc) <- consume c senv
                               , (t', acc') <- consT t
                               = (SnocAtup t' c', (acc', acc))
          (t', acc) = consT t
      in (ExecStuple t', toAtuple acc)

evalExtend :: Extend DelayedOpenAcc aenv aenv' -> Val aenv -> Val aenv'
evalExtend BaseEnv aenv = aenv
evalExtend (PushEnv ext1 ext2) aenv | aenv' <- evalExtend ext1 aenv
                                    = Push aenv' (evalOpenAcc ext2 aenv')

delayArray :: Array sh e -> Delayed (Array sh e)
delayArray arr@(Array _ adata) = Delayed (shape arr) (arr!) (toElt . unsafeIndexArrayData adata)

fromScalar :: Scalar a -> a
fromScalar = (!Z)

concatOp :: forall e. Elt e => [Vector e] -> Vector e
concatOp = concatVectors

fetchAllOp :: (Shape sh, Elt e) => Segments sh -> Vector e -> [Array sh e]
fetchAllOp segs elts
  | (offsets, n) <- offsetsOp segs
  , (n ! Z) <= size (shape elts)
  = [fetch (segs ! (Z :. i)) (offsets ! (Z :. i)) | i <- [0 .. size (shape segs) - 1]]
  | otherwise = error $ "illegal argument to fetchAllOp"
  where
    fetch sh offset = fromFunction sh (\ ix -> elts ! (Z :. ((toIndex sh ix) + offset)))

dropOp :: Elt e => Int -> Vector e -> Vector e
dropOp i v   -- TODO
             --  * Implement using C-style pointer-plus.
             --    ; dropOp is used often (from prjChunk),
             --      so it ought to be efficient O(1).
  | n <- size (shape v)
  , i <= n
  , i >= 0
  = fromFunction (Z :. n - i) (\ (Z :. j) -> v ! (Z :. i + j))
  | otherwise = error $ "illegal argument to drop"

offsetsOp :: Shape sh => Segments sh -> (Vector Int, Scalar Int)
offsetsOp segs = scanl'Op (+) 0 $ delayArray (mapOp size (delayArray segs))

