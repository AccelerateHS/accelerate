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
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}


module Data.Array.Accelerate.Pattern.Matchable where

import           Data.Array.Accelerate.Smart as Smart
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import           Generics.SOP as SOP
import Data.Type.Equality
import Data.Array.Accelerate.Representation.POS as POS (Undef(..))
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

  -- type Choices' a :: Nat
  -- type Choices' a = Choices a

  build ::
    ( KnownNat n
    ) => Proxy n
    -> NP Exp (Index (SOPCode a) n)
    -> Exp a
  default build ::
    ( KnownNat n
    , Elt a
    ) => Proxy n
    -> NP Exp (Index (SOPCode a) n)
    -> Exp a

  build n _ = case sameNat (Proxy :: Proxy (EltChoices a)) (Proxy :: Proxy 1) of
    -- no tag
    Just Refl -> undefined
    -- tagged
    Nothing -> undefined

  match :: ( KnownNat n
    ) => Proxy n
    -> Exp a
    -> Maybe (NP Exp (Index (SOPCode a) n))

buildTag :: SOP.All Elt xs => NP Exp xs -> Exp TAG
buildTag SOP.Nil = constant 0 -- exp of 0 :: Finite 1
buildTag (((Exp x) :: (Exp x)) :* (xs :: xs)) = case sameNat (Proxy :: Proxy (EltChoices x)) (Proxy :: Proxy 1) of
  -- x doesn't contain a tag, skip
  Just Refl
    -> buildTag xs
  -- x contains a tag, build an Exp to calculate the product
  Nothing
    | Refl :: (EltR x :~: (TAG, _r)) <- unsafeCoerce Refl
    -- TODO: this is incorrect, we need the size of the TAG here (return to Finite?)
    -> mkMul (Exp (SmartExp (Prj PairIdxLeft x))) (buildTag xs)



type family Index (xs :: [[Type]]) (y :: Nat) :: [Type] where
  Index (x ': xs) 0 = x
  Index (x ': xs) n = Index xs (n - 1)

type family ListToCons (xs :: [Type]) :: Type where
  ListToCons '[] = ()
  ListToCons (x ': xs) = (x, ListToCons xs)

-- copied from Elt library
type family Products (xs :: [Nat]) :: Nat where
  Products '[] = 1
  Products (x ': xs) = x * Products xs

-- idem
-- type family MapChoices (xs :: [Type]) :: [Nat] where
--   MapChoices '[] = '[]
--   MapChoices (x ': xs) = Choices x ': MapChoices xs

-- idem
-- type family Concat (xss :: [[x]]) :: [x] where
--   Concat '[] = '[]
--   Concat (xs ': xss) = xs ++ Concat xss

  
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
  -- type Choices' Bool = 2

  build n _ = Exp (SmartExp (Pair (undefined (fromInteger $ natVal n)) (SmartExp Smart.Nil)))

  match n (Exp e) = case sameNat n (Proxy :: Proxy 0) of
    Just Refl ->
      case e of
        SmartExp (Match (0,1) _x) -> Just SOP.Nil

        SmartExp Match {} -> Nothing

        _ -> error "Embedded pattern synonym used outside 'match' context."
    Nothing ->
      case sameNat n (Proxy :: Proxy 1) of
        Just Refl ->
          case e of
            SmartExp (Match (1,2) _x) -> Just SOP.Nil

            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"

makeTag :: TAG -> SmartExp TAG
makeTag x = SmartExp (Const (SingleScalarType (NumSingleType (IntegralNumType TypeTAG))) x)

tagType :: TupR ScalarType TAG
tagType = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeTAG)))


instance Matchable (Maybe Int) where
  -- type Choices' (Maybe Int) = 2

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
                    (scalarType @(UnionScalar (Undef, (Int, ()))))
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
        SmartExp (Match (0,1) _x)
          -> Just SOP.Nil

        SmartExp Match {} -> Nothing

        _ -> error "Embedded pattern synonym used outside 'match' context."
    Nothing -> -- matchJust
      case sameNat n (Proxy :: Proxy 1) of
        Just Refl ->
          case e of
            SmartExp (Match (1,2) x)
              -> Just (
                  (mkExp $ PrjUnion $ SmartExp $ Union (unConcatSumScalarType (UnionScalarType $ SuccScalarType (UndefSingleType) ZeroScalarType)) (SmartExp $ Prj PairIdxLeft (SmartExp $ Prj PairIdxRight x)))
                  :* SOP.Nil)
            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"

unConcatSumScalarType :: ScalarType (UnionScalar a) -> ScalarType (UnionScalar (Concat' a b)) -> ScalarType (UnionScalar b)
unConcatSumScalarType (UnionScalarType ZeroScalarType) xs = xs
unConcatSumScalarType (UnionScalarType (SuccScalarType a as)) (UnionScalarType (SuccScalarType x xs)) = unConcatSumScalarType (UnionScalarType as) (UnionScalarType xs)



instance (Elt (Either a b), Elt a, Elt b, Elt a) => Matchable (Either a b) where
  -- type Choices' (Either a b) = OuterChoices (Either a b)

  build n x
    | Refl :: (EltR (Either a b) :~: (TAG, y)) <- unsafeCoerce Refl -- this should be provable, I'm just lazy
    = case sameNat n (Proxy :: Proxy 0) of
      Just Refl
        -> Exp (
            SmartExp (
              Pair
                (unExp $ buildTAG x)
                _
              )
            )
      where
        tag = undefined --foldl 1 (*) (mapChoices x)
        test = natVal (Proxy :: Proxy (EltChoices a))
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

-- buildFields :: (All Elt xs) => NP SmartExp xs -> SmartExp (ConcatT (MapFlattenProduct (MapFields xs)))
-- buildFields SOP.Nil = ()
-- buildFields (x :* xs) = SmartExp $ Pair
--   where
--     fieldsx = buildFields1 x

mergeLeft :: forall a b . TypeR a -> TypeR b -> SmartExp a -> SmartExp (Merge' a b)
mergeLeft TupRunit TupRunit          a = unExp $ constant ()
mergeLeft TupRunit (TupRpair (TupRsingle (UnionScalarType x)) gbs) a
  = SmartExp $ Pair
      (makeUndefLeft x)
      (mergeLeft TupRunit gbs a)
mergeLeft (TupRpair (TupRsingle (UnionScalarType x)) gas) TupRunit a
  = SmartExp $ Pair
      (mergeSumUndefRight x (SmartExp $ Prj PairIdxLeft a))
      (mergeLeft gas TupRunit (SmartExp $ Prj PairIdxRight a))
mergeLeft (TupRpair (TupRsingle (UnionScalarType (ga :: (UnionScalarType ga)))) gas) (TupRpair (TupRsingle (UnionScalarType (gb :: (UnionScalarType gb)))) gbs) a
  = SmartExp $ Pair
      (SmartExp $ Union (\y -> scalarSumConcat' @ga @gb y (UnionScalarType gb)) (SmartExp $ Prj PairIdxLeft a)) -- (scalarSumConcat' @ga @gb (UnionScalarType ga))
      (mergeLeft gas gbs (SmartExp $ Prj PairIdxRight a))

makeUndefLeft :: UnionScalarType x -> SmartExp (UnionScalar (Undef, x))
makeUndefLeft x = SmartExp $ Const (UnionScalarType (SuccScalarType (UndefSingleType) x)) (PickScalar POS.Undef)

mergeSumLeft :: forall a b . UnionScalarType a -> UnionScalarType b -> SmartExp (UnionScalar a) -> SmartExp (UnionScalar (Concat' a b))
mergeSumLeft ls rs x = SmartExp $ Union (const $ scalarSumConcat ls rs) x


scalarSumConcat':: ScalarType (UnionScalar xs) -> ScalarType (UnionScalar ys) -> ScalarType (UnionScalar (Concat' xs ys))
scalarSumConcat' (UnionScalarType ls) (UnionScalarType rs) = scalarSumConcat ls rs

scalarSumConcat:: UnionScalarType xs -> UnionScalarType ys -> ScalarType (UnionScalar (Concat' xs ys))
scalarSumConcat ZeroScalarType rs = UnionScalarType rs
scalarSumConcat (SuccScalarType l ls) rs = UnionScalarType $ SuccScalarType l ls'
  where
    UnionScalarType ls' = scalarSumConcat ls rs

merge :: forall a b . TypeR a -> TypeR b -> SmartExp a -> SmartExp b -> SmartExp (Merge' a b)
merge TupRunit TupRunit          a b = unExp $ constant ()
merge TupRunit (TupRpair (TupRsingle (UnionScalarType x)) gbs) a b
  = SmartExp $ Pair
      (mergeSumUndefLeft x (SmartExp $ Prj PairIdxLeft b))
      (merge TupRunit gbs a (SmartExp $ Prj PairIdxRight b))
merge (TupRpair (TupRsingle (UnionScalarType x)) gas) TupRunit a b
  = SmartExp $ Pair
      (mergeSumUndefRight x (SmartExp $ Prj PairIdxLeft a))
      (merge gas TupRunit (SmartExp $ Prj PairIdxRight a) b)
merge (TupRpair (TupRsingle (UnionScalarType ga)) gas) (TupRpair (TupRsingle (UnionScalarType gb)) gbs) a b
  = SmartExp $ Pair
      (undefined) -- mergeSum
      (merge gas gbs (SmartExp $ Prj PairIdxRight a) (SmartExp $ Prj PairIdxRight b))


mergeSumUndefRight :: UnionScalarType x -> SmartExp (UnionScalar x) -> SmartExp (UnionScalar (Concat' x (Undef, ())))
mergeSumUndefRight ZeroScalarType a = SmartExp $ Const (UnionScalarType (SuccScalarType (UndefSingleType) ZeroScalarType)) (PickScalar POS.Undef)
mergeSumUndefRight (SuccScalarType x xs) a = SmartExp $ Union scalarTypeUndefRight a

mergeSumUndefLeft :: UnionScalarType x -> SmartExp (UnionScalar x) -> SmartExp (UnionScalar (Undef, x))
mergeSumUndefLeft ZeroScalarType a = SmartExp $ Const (UnionScalarType (SuccScalarType (UndefSingleType) ZeroScalarType)) (PickScalar POS.Undef)
mergeSumUndefLeft (SuccScalarType x xs) a = SmartExp $ Union scalarTypeUndefLeft a

scalarTypeUndefLeft :: ScalarType (UnionScalar a) -> ScalarType (UnionScalar (Undef, a))
scalarTypeUndefLeft (UnionScalarType x) = UnionScalarType (SuccScalarType (singleType @Undef) x)

scalarTypeUndefRight :: ScalarType (UnionScalar a) -> ScalarType (UnionScalar (Concat' a (Undef, ())))
scalarTypeUndefRight (UnionScalarType ZeroScalarType) = UnionScalarType (SuccScalarType (singleType @Undef) ZeroScalarType)
scalarTypeUndefRight (UnionScalarType (SuccScalarType x xs))
  = UnionScalarType (SuccScalarType x xs')
  where
    (UnionScalarType xs') = scalarTypeUndefRight (UnionScalarType xs)

-- mergeSum :: UnionScalar

class AllUnionScalar (xs :: Type) where

instance AllUnionScalar () where

instance (x' ~ UnionScalar x, IsUnionScalar x', AllUnionScalar xs) => AllUnionScalar (x, xs) where

class All' (c :: k -> Constraint) (xs :: Type) where

instance All' c () where

instance (c x, All' c xs) => All' c (x, xs) where
      


-- ZipWith Concat
-- like POSable.Merge, but lifted to tuple lists
type family Merge' (a :: Type) (b :: Type) = (r :: Type) where
  Merge' () () = ()
  Merge' () (UnionScalar b, bs) = (UnionScalar (Undef, b), Merge' () bs)
  Merge' (UnionScalar a, as) () = (UnionScalar (Concat' a (Undef, ())), Merge' as ())
  Merge' (UnionScalar a, as) (UnionScalar b, bs) = (UnionScalar (Concat' a b), Merge' as bs)

type family Concat' (a :: Type) (b :: Type) = (r :: Type) where
  Concat' () ys = ys
  Concat' (x, xs) ys = (x, Concat' xs ys)


type family Head (xs :: [x]) :: x where
  Head (x ': xs) = x

type family Tail (xs :: [x]) :: [x] where
  Tail (x ': xs) = xs

type family MapFlattenSum (x :: [[Type]]) :: [Type] where
  MapFlattenSum '[] = '[]
  MapFlattenSum (x ': xs) = UnionScalar (FlattenSum x) ': MapFlattenSum xs

-- like combineProducts, but lifted to the AST
buildTAG :: (All Elt xs) => NP Exp xs -> Exp TAG
buildTAG SOP.Nil = Exp $ makeTag 0
buildTAG (x :* xs) = combineProduct x (buildTAG xs)

-- like Finite.combineProduct, but lifted to the AST
-- basically `tag x + tag y * natVal x`
combineProduct :: forall x. (Elt x) => Exp x -> Exp TAG -> Exp TAG
combineProduct x y = case sameNat (Proxy :: Proxy (EltChoices x)) (Proxy :: Proxy 1) of
  -- untagged type: `tag x = 0`, `natVal x = 1`
  Just Refl -> y
  -- tagged type
  Nothing
    | Refl :: (EltR x :~: (TAG, y)) <- unsafeCoerce Refl
    -> mkAdd (mkExp $ Prj PairIdxLeft (unExp x)) (mkMul y (constant (fromInteger $ natVal (Proxy :: Proxy (EltChoices x)))))
