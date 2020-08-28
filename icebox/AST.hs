
module Data.Array.Accelerate.AST

data PreOpenAcc (acc :: Type -> Type -> Type) aenv a where
  -- A sequence of operations.
  Collect     :: Arrays arrs
              => PreOpenSeq acc aenv () arrs
              -> PreOpenAcc acc aenv arrs


-- |Closed sequence computation
--
type Seq = PreOpenSeq OpenAcc () ()

data PreOpenSeq acc aenv senv arrs where
  Producer :: Arrays a
           => Producer acc aenv senv a
           -> PreOpenSeq acc aenv (senv, a) arrs
           -> PreOpenSeq acc aenv senv arrs

  Consumer :: Arrays arrs
           => Consumer acc aenv senv arrs
           -> PreOpenSeq acc aenv senv arrs

  Reify    :: Arrays arrs
           => Idx senv arrs
           -> PreOpenSeq acc aenv senv [arrs]

data Producer acc aenv senv a where
  -- Convert the given Haskell-list of arrays to a sequence.
  StreamIn :: Arrays a
           => [a]
           -> Producer acc aenv senv a

  -- Convert the given array to a sequence.
  ToSeq :: (Elt slix, Shape sl, Shape sh, Elt e)
           => SliceIndex  (EltRepr slix)
                          (EltRepr sl)
                          co
                          (EltRepr sh)
           -> proxy slix
           -> acc aenv (Array sh e)
           -> Producer acc aenv senv (Array sl e)

  -- Apply the given the given function to all elements of the given
  -- sequence.
  MapSeq :: (Arrays a, Arrays b)
         => PreOpenAfun acc aenv (a -> b)
         -> Idx senv a
         -> Producer acc aenv senv b

  -- Apply the given the given function to all elements of the given
  -- sequence.
  ChunkedMapSeq :: (Arrays a, Arrays b)
                => PreOpenAfun acc aenv (Vector' a -> Vector' b)
                -> Idx senv a
                -> Producer acc aenv senv b

  -- Apply a given binary function pairwise to all elements of the
  -- given sequences.
  ZipWithSeq :: (Arrays a, Arrays b, Arrays c)
             => PreOpenAfun acc aenv (a -> b -> c)
             -> Idx senv a
             -> Idx senv b
             -> Producer acc aenv senv c

  -- ScanSeq (+) a0 x. Scan a sequence x by combining each element
  -- using the given binary operation (+). (+) must be associative:
  --
  --   Forall a b c. (a + b) + c = a + (b + c),
  --
  -- and a0 must be the identity element for (+):
  --
  --   Forall a. a0 + a = a = a + a0.
  --
  ScanSeq :: Elt e
          => PreFun acc aenv (e -> e -> e)
          -> PreExp acc aenv e
          -> Idx senv (Scalar e)
          -> Producer acc aenv senv (Scalar e)

data Consumer acc aenv senv a where

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
          => PreFun acc aenv (a -> a -> a)
          -> PreExp acc aenv a
          -> Idx senv (Scalar a)
          -> Consumer acc aenv senv (Scalar a)

  -- FoldSeqFlatten f a0 x. A specialized version of FoldSeqAct where
  -- reduction with the companion operator corresponds to
  -- flattening. f must be semi-associative, with vecotor append (++)
  -- as the companion operator:
  --
  --   Forall b sh1 a1 sh2 a2.
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
                 => PreOpenAfun acc aenv (a -> Vector sh -> Vector e -> a)
                 -> acc aenv a
                 -> Idx senv (Array sh e)
                 -> Consumer acc aenv senv a

  Stuple :: (Arrays a, IsAtuple a)
         => Atuple (Consumer acc aenv senv) (TupleRepr a)
         -> Consumer acc aenv senv a


rnfPreOpenSeq :: forall acc aenv senv t. NFDataAcc acc -> PreOpenSeq acc aenv senv t -> ()
rnfPreOpenSeq rnfA topSeq =
  let
      rnfS :: PreOpenSeq acc aenv' senv' t' -> ()
      rnfS = rnfPreOpenSeq rnfA

      rnfP :: Producer acc aenv' senv' t' -> ()
      rnfP = rnfSeqProducer rnfA

      rnfC :: Consumer acc aenv' senv' t' -> ()
      rnfC = rnfSeqConsumer rnfA
  in
  case topSeq of
    Producer p s              -> rnfP p `seq` rnfS s
    Consumer c                -> rnfC c
    Reify ix                  -> rnfIdx ix

rnfSeqProducer :: forall acc aenv senv t. NFDataAcc acc -> Producer acc aenv senv t -> ()
rnfSeqProducer rnfA topSeq =
  let
      rnfArrs :: forall a. Arrays a => [a] -> ()
      rnfArrs []     = ()
      rnfArrs (a:as) = rnfArrays (arrays @a) (fromArr a) `seq` rnfArrs as

      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfF :: OpenFun env' aenv' t' -> ()
      rnfF = rnfOpenFun rnfA

      rnfE :: OpenExp env' aenv' t' -> ()
      rnfE = rnfOpenExp rnfA
  in
  case topSeq of
    StreamIn as               -> rnfArrs as
    ToSeq slice _ a           -> rnfSliceIndex slice `seq` rnfA a
    MapSeq f ix               -> rnfAF f `seq` rnfIdx ix
    ChunkedMapSeq f ix        -> rnfAF f `seq` rnfIdx ix
    ZipWithSeq f ix1 ix2      -> rnfAF f `seq` rnfIdx ix1 `seq` rnfIdx ix2
    ScanSeq f z ix            -> rnfF f `seq` rnfE z `seq` rnfIdx ix

rnfSeqConsumer :: forall acc aenv senv t. NFDataAcc acc -> Consumer acc aenv senv t -> ()
rnfSeqConsumer rnfA topSeq =
  let
      rnfAF :: PreOpenAfun acc aenv' t' -> ()
      rnfAF = rnfPreOpenAfun rnfA

      rnfF :: OpenFun env' aenv' t' -> ()
      rnfF = rnfOpenFun rnfA

      rnfE :: OpenExp env' aenv' t' -> ()
      rnfE = rnfOpenExp rnfA
  in
  case topSeq of
    FoldSeq f z ix            -> rnfF f `seq` rnfE z `seq` rnfIdx ix
    FoldSeqFlatten f a ix     -> rnfAF f `seq` rnfA a `seq` rnfIdx ix
    Stuple stup               -> rnfStuple rnfA stup

rnfStuple :: NFDataAcc acc -> Atuple (Consumer acc aenv senv) t -> ()
rnfStuple _    NilAtup          = ()
rnfStuple rnfA (SnocAtup tup c) = rnfStuple rnfA tup `seq` rnfSeqConsumer rnfA c

