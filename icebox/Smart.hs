
module Data.Array.Accelerate.Smart


data PreSeq acc seq exp arrs where
  -- Convert the given Haskell-list of arrays to a sequence.
  StreamIn :: Arrays a
           => [a]
           -> PreSeq acc seq exp [a]

  -- Convert the given array to a sequence.
  -- Example:
  -- slix = Z :. All :. Split :. All :. All :. Split
  --              ^       ^       ^      ^      ^
  --              |        \     /      /       |
  --              |         \___/______/_______ Iteration space.
  --              |            /      /
  --           Element________/______/
  --            shape.
  --
  ToSeq :: ( Elt e
           , Slice slix
           , Division slsix
           , DivisionSlice slsix ~ slix
           , Typeable (FullShape slix)
           , Typeable (SliceShape slix)
           )
        => slsix
        -> acc (Array (FullShape slix) e)
        -> PreSeq acc seq exp [Array (SliceShape slix) e]

  -- Apply the given the given function to all elements of the given sequence.
  MapSeq :: (Arrays a, Arrays b)
         => (Acc a -> acc b)
         -> seq [a]
         -> PreSeq acc seq exp [b]

  -- Apply a given binary function pairwise to all elements of the given sequences.
  -- The length of the result is the length of the shorter of the two argument
  -- arrays.
  ZipWithSeq :: (Arrays a, Arrays b, Arrays c)
             => (Acc a -> Acc b -> acc c)
             -> seq [a]
             -> seq [b]
             -> PreSeq acc seq exp [c]

  -- ScanSeq (+) a0 x. Scan a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  ScanSeq :: Elt a
          => (Exp a -> Exp a -> exp a)
          -> exp a
          -> seq [Scalar a]
          -> PreSeq acc seq exp [Scalar a]

  -- FoldSeq (+) a0 x. Fold a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  FoldSeq :: Elt a
          => (Exp a -> Exp a -> exp a)
          -> exp a
          -> seq [Scalar a]
          -> PreSeq acc seq exp (Scalar a)

  -- FoldSeqFlatten f a0 x. A specialized version of FoldSeqAct
  -- where reduction with the companion operator corresponds to
  -- flattening. f must be semi-associative, with vecotor append (++)
  -- as the companion operator:
  --
  --   Forall b s1 a2 sh2 a2.
  --     f (f b sh1 a1) sh2 a2 = f b (sh1 ++ sh2) (a1 ++ a2).
  --
  -- It is common to ignore the shape vectors, yielding the usual
  -- semi-associativity law:
  --
  --   f b a _ = b + a,
  --
  -- for some (+) satisfying:
  --
  --   Forall b a1 a2. (b + a1) + a2 = b + (a1 ++ a2).
  --
  FoldSeqFlatten :: (Arrays a, Shape sh, Elt e)
                 => (Acc a -> Acc (Vector sh) -> Acc (Vector e) -> acc a)
                 -> acc a
                 -> seq [Array sh e]
                 -> PreSeq acc seq exp a

  -- Tuple up the results of a sequence computation. Note that the Arrays
  -- constraint requires that the elements of the tuple are Arrays, not
  -- streams ([]).
  Stuple :: (Arrays arrs, IsAtuple arrs)
         => Atuple (seq) (TupleRepr arrs)
         -> PreSeq acc seq exp arrs

-- |Array-valued sequence computations
--
newtype Seq a = Seq (PreSeq Acc Seq Exp a)

deriving instance Typeable Seq


showPreSeqOp :: PreSeq acc seq exp arrs -> String
showPreSeqOp StreamIn{}       = "StreamIn"
showPreSeqOp ToSeq{}          = "ToSeq"
showPreSeqOp MapSeq{}         = "MapSeq"
showPreSeqOp ZipWithSeq{}     = "ZipWithSeq"
showPreSeqOp ScanSeq{}        = "ScanSeq"
showPreSeqOp FoldSeq{}        = "FoldSeq"
showPreSeqOp FoldSeqFlatten{} = "FoldSeqFlatten"
showPreSeqOp Stuple{}         = "Stuple"

