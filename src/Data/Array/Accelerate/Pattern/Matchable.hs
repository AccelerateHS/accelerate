{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoStarIsType         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}


module Data.Array.Accelerate.Pattern.Matchable where

import           Data.Array.Accelerate.Smart as Smart
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import           Generics.SOP as SOP
import Data.Type.Equality
import Data.Array.Accelerate.Representation.POS as POS
import Data.Array.Accelerate.Representation.Tag
import Unsafe.Coerce
import qualified Data.Array.Accelerate.AST as AST
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Representation.Type
-- import Data.Array.Accelerate.Pretty


class Matchable a where
  type SOPCode a :: [[Type]]
  type SOPCode a = Code a

  type Choices' a :: Nat
  type Choices' a = Choices a

  build ::
    ( KnownNat n
    ) => Proxy n
    -> NP Exp (Index (SOPCode a) n)
    -> Exp a
  default build ::
    ( KnownNat n
    , POSable a
    ) => Proxy n
    -> NP Exp (Index (SOPCode a) n)
    -> Exp a

  build n _ = case sameNat (emptyChoices @a) (Proxy :: Proxy 1) of
    -- no tag
    Just Refl -> undefined
    -- tagged
    Nothing -> undefined

  match :: ( KnownNat n
    ) => Proxy n
    -> Exp a
    -> Maybe (NP Exp (Index (SOPCode a) n))

buildTag :: SOP.All POSable xs => NP Exp xs -> Exp TAG
buildTag SOP.Nil = constant 0 -- exp of 0 :: Finite 1
buildTag (((Exp x) :: (Exp x)) :* (xs :: xs)) = case sameNat (emptyChoices @x) (Proxy :: Proxy 1) of
  -- x doesn't contain a tag, skip
  Just Refl
    -> buildTag xs
  -- x contains a tag, build an Exp to calculate the product
  Nothing
    | Refl :: (EltR x :~: (TAG, _r)) <- unsafeCoerce Refl
    -- TODO: this is incorrect, we need the size of the TAG here (return to Finite?)
    -> mkMul (Exp (SmartExp (Prj PairIdxLeft x))) (buildTag xs)

-- flattenProduct :: Product a -> FlattenProduct a
-- flattenProduct Nil = ()
-- flattenProduct (Cons x xs) = (SumScalarType x, flattenProduct xs)

-- buildFields :: forall n a . (POSable a, Elt a) => Proxy n -> NP SmartExp (Index (SOPCode a) n) -> SmartExp (FlattenProduct (Fields a))
-- buildFields _ a = case emptyFields @a of
--   PTNil -> case constant () of { Exp se -> se }
--   PTCons st pt -> case a of
--     -- SOP.Nil -> SmartExp (Pair (someFunction st) undefined)
--     (x :* xs) -> SmartExp (Pair undefined undefined)

buildFields' :: Proxy n -> ProductType (Fields a) -> NP SmartExp (Index (SOPCode a) n) -> SmartExp (FlattenProduct (Fields a))
buildFields' _ PTNil         _         = SmartExp Smart.Nil
buildFields' n (PTCons x xs) SOP.Nil   = undefined -- SmartExp $ Pair _ (buildFields' n xs SOP.Nil)
buildFields' _ (PTCons x xs) (y :* ys) = undefined

someFunction :: SumType x -> SmartExp (ScalarType (Sum x))
someFunction = undefined

newtype SEFPF a = SEFPF (SmartExp (FlattenProduct (Fields a)))

-- mapBuildField :: (All POSable xs, All Elt xs) => NP SmartExp xs -> NP SEFPF xs
-- mapBuildField SOP.Nil = SOP.Nil
-- mapBuildField ((x :: SmartExp x) :* xs) = SEFPF (buildField @x x) :* mapBuildField xs


buildField :: forall a . (POSable a, Elt a, EltR a ~ POStoEltR (Choices a) (Fields a)) => SmartExp (EltR a) -> SmartExp (FlattenProduct (Fields a))
buildField (SmartExp a) = case sameNat (emptyChoices @a) (Proxy :: Proxy 1) of
  Just Refl ->
    case emptyFields @a of
      -- singleton types
      PTCons (STSucc _ STZero) PTNil
        | Refl :: (POStoEltR (Choices a) (Fields a) :~: a) <- unsafeCoerce Refl
        -> SmartExp $ Pair (SmartExp (undefined a)) (SmartExp Smart.Nil)
      -- tagless types
      _ | Refl :: (POStoEltR (Choices a) (Fields a) :~: FlattenProduct (Fields a)) <- unsafeCoerce Refl
        -> SmartExp a
  -- tagged types
  Nothing
    -- We know that this is true because Choices a is not equal to 1
    | Refl :: (POStoEltR (Choices a) (Fields a) :~: (_x, FlattenProduct (Fields a))) <- unsafeCoerce Refl
    -> SmartExp (Prj PairIdxRight (SmartExp a))


type family Index (xs :: [[Type]]) (y :: Nat) :: [Type] where
  Index (x ': xs) 0 = x
  Index (x ': xs) n = Index xs (n - 1)

type family ListToCons (xs :: [Type]) :: Type where
  ListToCons '[] = ()
  ListToCons (x ': xs) = (x, ListToCons xs)

-- copied from POSable library
type family Products (xs :: [Nat]) :: Nat where
  Products '[] = 1
  Products (x ': xs) = x * Products xs

-- idem
type family MapChoices (xs :: [Type]) :: [Nat] where
  MapChoices '[] = '[]
  MapChoices (x ': xs) = Choices x ': MapChoices xs

-- idem
type family Concat (xss :: [[x]]) :: [x] where
  Concat '[] = '[]
  Concat (xs ': xss) = xs ++ Concat xss

-- idem
type family MapFields (xs :: [Type]) :: [[[Type]]] where
  MapFields '[] = '[]
  MapFields (x ': xs) = Fields x ': MapFields xs
  
type family MapFlattenProduct (xs :: [[[Type]]]) :: [Type] where
  MapFlattenProduct '[] = '[]
  MapFlattenProduct (x ': xs) = FlattenProduct x ': MapFlattenProduct xs

type family ConcatT (xss :: [x]) :: x where
  ConcatT '[] = ()
  ConcatT (x ': xs) = (x, ConcatT xs)

-- type family RealConcatT (xss :: [[Type]]) :: Type where
--   ConcatT '[] = ()
--   ConcatT ('[] ': xs) = RealConcatT xs
--   ConcatT ((x ': xs) ': ys) = (x, RealConcatT xs ys)
  
type family ConcatAST (x :: Type) (y :: Type) :: Type where
  ConcatAST xs () = xs
  ConcatAST () ys = ys
  ConcatAST (x, xs) ys = (x, ConcatAST xs ys)

type family ConcatASTs (xs :: [Type]) :: Type where
  ConcatASTs '[] = ()
  ConcatASTs (x ': xs) = ConcatAST x (ConcatASTs xs)

instance Matchable Bool where
  type Choices' Bool = 2

  build n _ = Exp (SmartExp (Pair (undefined (fromInteger $ natVal n)) (SmartExp Smart.Nil)))

  match n (Exp e) = case sameNat n (Proxy :: Proxy 0) of
    Just Refl ->
      case e of
        SmartExp (Match (TagRtag 0 TagRunit) _x) -> Just SOP.Nil

        SmartExp Match {} -> Nothing

        _ -> error "Embedded pattern synonym used outside 'match' context."
    Nothing ->
      case sameNat n (Proxy :: Proxy 1) of
        Just Refl ->
          case e of
            SmartExp (Match (TagRtag 1 TagRunit) _x) -> Just SOP.Nil

            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"

makeTag :: TAG -> SmartExp TAG
makeTag x = SmartExp (Const (SingleScalarType (NumSingleType (IntegralNumType TypeTAG))) x)

tagType :: TupR ScalarType TAG
tagType = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeTAG)))


instance Matchable (Maybe Int) where
  type Choices' (Maybe Int) = 2

  build n x = case sameNat n (Proxy :: Proxy 0) of
    Just Refl ->
      Exp (
        SmartExp (
          Pair
            (makeTag 0)
            (SmartExp (
              Pair
                (SmartExp (
                  Const
                    (scalarType @(SumScalar (Undef, (Int, ()))))
                    (PickScalar POS.Undef)
                ))
                (SmartExp Smart.Nil)
            ))
          )
        )
    Nothing -> case sameNat n (Proxy :: Proxy 1) of
      Just Refl | (Exp x' :* SOP.Nil) <- x -> Exp (
          SmartExp (
            Pair
              (makeTag 1)
              (SmartExp (
                Pair
                  (SmartExp
                    (Union
                      scalarTypeUndefLeft
                      (SmartExp
                        (LiftUnion x')
                      )
                    )
                  )
                  (SmartExp Smart.Nil)
              ))
          )
        )
      Nothing -> error "Impossible type encountered"

  match n (Exp e) = case sameNat n (Proxy :: Proxy 0) of
    Just Refl ->
      case e of
        SmartExp (Match (TagRtag 0 (TagRpair _ TagRunit)) _x)
          -> Just SOP.Nil

        SmartExp Match {} -> Nothing

        _ -> error "Embedded pattern synonym used outside 'match' context."
    Nothing -> -- matchJust
      case sameNat n (Proxy :: Proxy 1) of
        Just Refl ->
          case e of
            SmartExp (Match (TagRtag 1 _) x)
              -> Just (
                  (mkExp $ PrjUnion $ SmartExp $ Union (unConcatSumScalarType (SumScalarType $ SuccScalarType (SingleScalarType UndefSingleType) ZeroScalarType)) (SmartExp $ Prj PairIdxLeft (SmartExp $ Prj PairIdxRight x)))
                  :* SOP.Nil)
            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"

unConcatSumScalarType :: ScalarType (SumScalar a) -> ScalarType (SumScalar (Concat' a b)) -> ScalarType (SumScalar b)
unConcatSumScalarType (SumScalarType ZeroScalarType) xs = xs
unConcatSumScalarType (SumScalarType (SuccScalarType a as)) (SumScalarType (SuccScalarType x xs)) = unConcatSumScalarType (SumScalarType as) (SumScalarType xs)

instance (POSable (Either a b), POSable a, POSable b, Elt a) => Matchable (Either a b) where
  type Choices' (Either a b) = OuterChoices (Either a b)

  build n x
    | Refl :: (EltR (Either a b) :~: (TAG, FlattenProduct (Fields (Either a b)))) <- unsafeCoerce Refl -- this should be easily provable, I'm just lazy
    = case sameNat n (Proxy :: Proxy 0) of
      Just Refl
        -> Exp (
            SmartExp (
              Pair
                (unExp $ buildTAG x)
                undefined --(understandConcatPlease @a @b (mergeLeft @a @b (getSingleElem x)))
              )
            )
      where
        tag = undefined --foldl 1 (*) (mapChoices x)
        test = natVal (Proxy :: Proxy (Choices a))
  --   Nothing -> case sameNat n (Proxy :: Proxy 1) of
  --     Just Refl | (Exp x' :* SOP.Nil) <- x -> Exp (
  --         SmartExp (
  --           Pair
  --             (makeTag 1)
  --             (SmartExp (
  --               Pair
  --                 (SmartExp
  --                   (Union
  --                     (Right (
  --                       SmartExp
  --                         (Union
  --                           (Left x')
  --                         )
  --                     ))
  --                   )
  --                 )
  --                 (SmartExp Smart.Nil)
  --             ))
  --         )
  --       )
  --     Nothing -> error "Impossible type encountered"

  -- match n (Exp e) = case sameNat n (Proxy :: Proxy 0) of
  --   Just Refl ->
  --     case e of
  --       SmartExp (Match (TagRtag 0 (TagRpair _ TagRunit)) _x)
  --         -> Just SOP.Nil

  --       SmartExp Match {} -> Nothing

  --       _ -> error "Embedded pattern synonym used outside 'match' context."
  --   Nothing -> -- matchJust
  --     case sameNat n (Proxy :: Proxy 1) of
  --       Just Refl ->
  --         case e of
  --           SmartExp (Match (TagRtag 1 _) x)
  --             -> Just
  --                 (Exp
  --                   (SmartExp
  --                     (PrjUnion
  --                       UnionIdxLeft
  --                       (SmartExp
  --                         (PrjUnion
  --                           UnionIdxRight
  --                           (SmartExp
  --                             (Prj
  --                               PairIdxLeft
  --                               (SmartExp (Prj PairIdxRight x))
  --                             ))
  --                         ))
  --                     )) :* SOP.Nil)
  --           SmartExp Match {} -> Nothing

  --           _ -> error "Embedded pattern synonym used outside 'match' context."

  --       Nothing ->
  --         error "Impossible type encountered"

-- weirdConvert :: forall x . Elt x => TypeR (EltR x)
-- weirdConvert = eltR @x

-- getSingleElem :: NP Exp '[a] -> Exp a
-- getSingleElem (x :* SOP.Nil) = x

-- understandConcatPlease :: SmartExp (FlattenProduct (Merge (Fields a) (Fields b))) -> SmartExp (FlattenProduct (Merge (Fields a ++ '[]) (Fields b ++ '[])))
-- understandConcatPlease = unsafeCoerce

-- mergeLeft :: forall a b . (POSable a, Elt a) => Exp a -> SmartExp (FlattenProduct (Merge (Fields a) (Fields b)))
-- mergeLeft (Exp a) = case buildFields1 @a a of
--   a' -> case weirdConvert2 @a (eltR @a) of
--     a3 -> undefined

-- mergeLeft' :: forall a b . TypeR (EltR a -> ProductType b -> SmartExp (FlattenProduct a) -> SmartExp (Merge' (FlattenProduct a) (FlattenProduct b))
-- mergeLeft' PTNil PTNil a = a
-- mergeLeft' PTNil (PTCons gb (gbs :: (ProductType (Fields b')))) a = SmartExp $ Pair (fromSumType gb) (mergeLeft' @a @b' PTNil gbs a)


-- buildFields :: (All POSable xs) => NP SmartExp xs -> SmartExp (ConcatT (MapFlattenProduct (MapFields xs)))
-- buildFields SOP.Nil = ()
-- buildFields (x :* xs) = SmartExp $ Pair
--   where
--     fieldsx = buildFields1 x

-- mergeLeft :: forall a b . TypeR a -> TypeR b -> SmartExp a -> SmartExp (Merge' a b)
-- mergeLeft TupRunit TupRunit          a = unExp $ constant ()
-- mergeLeft TupRunit (TupRpair (TupRsingle (SumScalarType x)) gbs) a
--   = SmartExp $ Pair
--       (makeUndefLeft x)
--       (mergeLeft TupRunit gbs a)
-- mergeLeft (TupRpair (TupRsingle (SumScalarType x)) gas) TupRunit a
--   = SmartExp $ Pair
--       (mergeSumUndefRight x (SmartExp $ Prj PairIdxLeft a))
--       (mergeLeft gas TupRunit (SmartExp $ Prj PairIdxRight a))
-- mergeLeft (TupRpair (TupRsingle (SumScalarType ga)) gas) (TupRpair (TupRsingle (SumScalarType gb)) gbs) a
--   = SmartExp $ Pair
--       (SmartExp $ Union _ (SmartExp $ Prj PairIdxLeft a))
--       (mergeLeft gas gbs (SmartExp $ Prj PairIdxRight a))

makeUndefLeft :: SumScalarType x -> SmartExp (SumScalar (Undef, x))
makeUndefLeft x = SmartExp $ Const (SumScalarType (SuccScalarType (SingleScalarType UndefSingleType) x)) (PickScalar POS.Undef)

mergeSumLeft :: forall a b . SumScalarType a -> SumScalarType b -> SmartExp (SumScalar a) -> SmartExp (SumScalar (Concat' a b))
mergeSumLeft ls rs x = SmartExp $ Union (const $ scalarSumConcat ls rs) x

scalarSumConcat:: SumScalarType xs -> SumScalarType ys -> ScalarType (SumScalar (Concat' xs ys))
scalarSumConcat ZeroScalarType rs = SumScalarType rs
scalarSumConcat (SuccScalarType l ls) rs = SumScalarType $ SuccScalarType l ls'
  where
    SumScalarType ls' = scalarSumConcat ls rs

merge :: forall a b . TypeR a -> TypeR b -> SmartExp a -> SmartExp b -> SmartExp (Merge' a b)
merge TupRunit TupRunit          a b = unExp $ constant ()
merge TupRunit (TupRpair (TupRsingle (SumScalarType x)) gbs) a b
  = SmartExp $ Pair
      (mergeSumUndefLeft x (SmartExp $ Prj PairIdxLeft b))
      (merge TupRunit gbs a (SmartExp $ Prj PairIdxRight b))
merge (TupRpair (TupRsingle (SumScalarType x)) gas) TupRunit a b
  = SmartExp $ Pair
      (mergeSumUndefRight x (SmartExp $ Prj PairIdxLeft a))
      (merge gas TupRunit (SmartExp $ Prj PairIdxRight a) b)
merge (TupRpair (TupRsingle (SumScalarType ga)) gas) (TupRpair (TupRsingle (SumScalarType gb)) gbs) a b
  = SmartExp $ Pair
      (undefined) -- mergeSum
      (merge gas gbs (SmartExp $ Prj PairIdxRight a) (SmartExp $ Prj PairIdxRight b))


mergeSumUndefRight :: SumScalarType x -> SmartExp (SumScalar x) -> SmartExp (SumScalar (Concat' x (Undef, ())))
mergeSumUndefRight ZeroScalarType a = SmartExp $ Const (SumScalarType (SuccScalarType (SingleScalarType UndefSingleType) ZeroScalarType)) (PickScalar POS.Undef)
mergeSumUndefRight (SuccScalarType x xs) a = SmartExp $ Union scalarTypeUndefRight a

mergeSumUndefLeft :: SumScalarType x -> SmartExp (SumScalar x) -> SmartExp (SumScalar (Undef, x))
mergeSumUndefLeft ZeroScalarType a = SmartExp $ Const (SumScalarType (SuccScalarType (SingleScalarType UndefSingleType) ZeroScalarType)) (PickScalar POS.Undef)
mergeSumUndefLeft (SuccScalarType x xs) a = SmartExp $ Union scalarTypeUndefLeft a

scalarTypeUndefLeft :: ScalarType (SumScalar a) -> ScalarType (SumScalar (Undef, a))
scalarTypeUndefLeft (SumScalarType x) = SumScalarType (SuccScalarType (scalarType @Undef) x)

scalarTypeUndefRight :: ScalarType (SumScalar a) -> ScalarType (SumScalar (Concat' a (Undef, ())))
scalarTypeUndefRight (SumScalarType ZeroScalarType) = SumScalarType (SuccScalarType (scalarType @Undef) ZeroScalarType)
scalarTypeUndefRight (SumScalarType (SuccScalarType x xs))
  = SumScalarType (SuccScalarType x xs')
  where
    (SumScalarType xs') = scalarTypeUndefRight (SumScalarType xs)

-- mergeSum :: SumScalar

class AllSumScalar (xs :: Type) where

instance AllSumScalar () where

instance (x' ~ SumScalar x, IsSumScalar x', AllSumScalar xs) => AllSumScalar (x, xs) where

class All' (c :: k -> Constraint) (xs :: Type) where

instance All' c () where

instance (c x, All' c xs) => All' c (x, xs) where
      


-- ZipWith Concat
-- like POSable.Merge, but lifted to tuple lists
type family Merge' (a :: Type) (b :: Type) = (r :: Type) where
  Merge' () () = ()
  Merge' () (SumScalar b, bs) = (SumScalar (Undef, b), Merge' () bs)
  Merge' (SumScalar a, as) () = (SumScalar (Concat' a (Undef, ())), Merge' as ())
  Merge' (SumScalar a, as) (SumScalar b, bs) = (SumScalar (Concat' a b), Merge' as bs)

type family Concat' (a :: Type) (b :: Type) = (r :: Type) where
  Concat' () ys = ys
  Concat' (x, xs) ys = (x, Concat' xs ys)

-- fromSumType :: SumType x -> SmartExp (SumScalar (Undef, FlattenSum x))
-- fromSumType x = SmartExp $ Union (_) $ SmartExp (LiftUnion (SmartExp (Const (SingleScalarType UndefSingleType) POS.Undef)))

buildFields1 :: forall x . (POSable x) => SmartExp (EltR x) -> SmartExp (FlattenProduct (Fields x))
buildFields1 x = case eltRType @x of
  SingletonType -> SmartExp $ Pair (SmartExp $ LiftUnion x) (SmartExp Smart.Nil)
  TaglessType -> x
  TaggedType -> SmartExp $ Prj PairIdxRight x

weirdConvert2 :: forall x . (Elt x, POSable x) => TypeR (EltR x) -> TypeR (FlattenProduct (Fields x))
weirdConvert2 x = case eltRType @x of
  SingletonType -> case x of
    TupRsingle x' -> TupRpair (TupRsingle (SumScalarType (SuccScalarType x' ZeroScalarType))) TupRunit
  TaglessType -> x
  TaggedType -> case x of
    TupRpair _ x' -> x'

-- guidedAppend :: forall x y . TypeR (FlattenProduct (Fields x)) -> SmartExp (FlattenProduct (Fields x)) -> SmartExp (FlattenProduct (Fields y)) -> SmartExp (FlattenProduct (Fields x ++ Fields y))
-- guidedAppend  TupRunit        x y | Refl :: (FlattenProduct (Fields y) :~: FlattenProduct (Fields x ++ Fields y)) <- unsafeCoerce Refl = y
-- guidedAppend (TupRsingle g)   x y = SmartExp (Pair x y)
-- guidedAppend (TupRpair g1 g2) x y = undefined

buildFields2 :: forall x y . (POSable x) => SmartExp (EltR x) -> SmartExp (FlattenProduct (Fields y)) -> SmartExp (FlattenProduct (Fields x ++ Fields y))
buildFields2 x y = case eltRType @x of
  SingletonType -> SmartExp $ Pair (SmartExp $ LiftUnion x) y
  TaglessType -> buildFields3 @x @y x y
  TaggedType -> buildFields3 @x @y (SmartExp $ Prj PairIdxRight x) y


buildFields3 :: forall x y . (POSable x) => SmartExp (FlattenProduct (Fields x)) -> SmartExp (FlattenProduct (Fields y)) -> SmartExp (FlattenProduct (Fields x ++ Fields y))
buildFields3 = buildFields4 @x @y (emptyFields @x)

buildFields4 :: forall x y . ProductType (Fields x) -> SmartExp (FlattenProduct (Fields x)) -> SmartExp (FlattenProduct (Fields y)) -> SmartExp (FlattenProduct (Fields x ++ Fields y))
buildFields4 PTNil x y = y
buildFields4 (PTCons g gs) x y = undefined
  where
    x' :: SmartExp (SumScalar (FlattenSum (Head (Fields x))))
    x' = SmartExp $ Prj PairIdxLeft x
    xs' :: SmartExp (FlattenProduct (Tail (Fields x)))
    xs' = SmartExp $ Prj PairIdxRight x
    f :: SmartExp (FlattenProduct (Fields x ++ Fields y))
    f = SmartExp (Pair x' xy)
    xy :: SmartExp (FlattenProduct (Tail (Fields x) ++ Fields y))
    xy = undefined

type family Head (xs :: [x]) :: x where
  Head (x ': xs) = x

type family Tail (xs :: [x]) :: [x] where
  Tail (x ': xs) = xs
-- concatFields :: forall x y. (POSable x) => SmartExp (FlattenProduct (Fields x)) -> SmartExp (FlattenProduct (Fields y)) -> SmartExp (FlattenProduct (Fields x ++ Fields y))
-- concatFields x y = case emptyFields @x of
--   PTNil
--     -> y
--   (PTCons x' (xs' :: (ProductType xs)))
--     -> SmartExp $ Pair (SmartExp $ Prj PairIdxLeft x) (f @xs @y (SmartExp $ Prj PairIdxRight x) y)
--   where
--     -- rec' :: SmartExp (FlattenProduct )
--     -- rec' x y = concatFields (SmartExp $ Prj PairIdxRight x) y
--     f :: SmartExp (FlattenProduct ys) -> SmartExp (FlattenProduct (Fields z)) -> SmartExp (FlattenProduct (ys ++ Fields z))
--     f xs y' = undefined
  
-- concatFields' :: forall xs ys . SmartExp (FlattenProduct xs) -> SmartExp (FlattenProduct ys) -> SmartExp (FlattenProduct (xs ++ ys))
-- concatFields' = undefined

-- convertASTtoNP :: forall x . POSable x => SmartExp (FlattenProduct (Fields x)) -> NP SmartExp (MapFlattenSum (Fields x))
-- convertASTtoNP = convertASTtoNP' @(Fields x) (emptyFields @x)

-- convertASTtoNP' :: ProductType x -> SmartExp (FlattenProduct x) -> NP SmartExp (MapFlattenSum x)
-- convertASTtoNP' PTNil _         = SOP.Nil
-- convertASTtoNP' (PTCons _ xs) y = SmartExp (Prj PairIdxLeft y) :* convertASTtoNP' xs (SmartExp $ Prj PairIdxRight y)

-- nPtoAST :: NP SmartExp xs -> SmartExp (ConcatASTs xs)
-- nPtoAST SOP.Nil = SmartExp Smart.Nil
-- nPtoAST (x :* xs) = SmartExp $ Pair x (nPtoAST xs)

-- concatAST :: forall x y . (Elt x) => SmartExp x -> SmartExp y -> SmartExp (ConcatAST x y)
-- concatAST x y | TupRunit <- eltR @x = y
-- concatAST x y | (TupRpair _ _) <- eltR @x = SmartExp $ Pair (SmartExp $ Prj PairIdxLeft x) (concatAST (SmartExp (Prj PairIdxRight x)) y)

type family MapFlattenSum (x :: [[Type]]) :: [Type] where
  MapFlattenSum '[] = '[]
  MapFlattenSum (x ': xs) = SumScalar (FlattenSum x) ': MapFlattenSum xs

-- like combineProducts, but lifted to the AST
buildTAG :: (All POSable xs) => NP Exp xs -> Exp TAG
buildTAG SOP.Nil = Exp $ makeTag 0
buildTAG (x :* xs) = combineProduct x (buildTAG xs)

-- like Finite.combineProduct, but lifted to the AST
-- basically `tag x + tag y * natVal x`
combineProduct :: forall x. (POSable x) => Exp x -> Exp TAG -> Exp TAG
combineProduct x y = case sameNat (Proxy :: Proxy (Choices x)) (Proxy :: Proxy 1) of
  -- untagged type: `tag x = 0`, `natVal x = 1`
  Just Refl -> y
  -- tagged type
  Nothing
    | Refl :: (EltR x :~: (TAG, FlattenProduct (Fields x))) <- unsafeCoerce Refl
    -> mkAdd (mkExp $ Prj PairIdxLeft (unExp x)) (mkMul y (constant (fromInteger $ natVal (Proxy :: Proxy (Choices x)))))
