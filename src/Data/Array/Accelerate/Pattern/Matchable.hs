{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}

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

  -- type Choices' a :: Nat
  -- type Choices' a = Choices a

  build ::
    ( KnownNat n
    ) => Proxy n
    -> NP Exp (SOPCode a !! n)
    -> Exp a
  default build ::
    ( KnownNat n
    , Elt a
    ) => Proxy n
    -> NP Exp (SOPCode a !! n)
    -> Exp a

  build n _ = case sameNat (Proxy :: Proxy (EltChoices a)) (Proxy :: Proxy 1) of
    -- no tag
    Just Refl -> undefined
    -- tagged
    Nothing -> undefined

  match :: ( KnownNat n
    ) => Proxy n
    -> Exp a
    -> Maybe (NP Exp (SOPCode a !! n))

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



type family (!!) (xs :: [[Type]]) (y :: Nat) :: [Type] where
  (x ': xs) !! 0 = x
  (x ': xs) !! n = xs !! (n - 1)

infixl 9 !!

instance Matchable Bool where
  -- type Choices' Bool = 2

  build n _ = Exp (SmartExp (Pair (unExp $ constant @TAG (fromInteger $ natVal n)) (SmartExp Smart.Nil)))

  match n (Exp e) = case sameNat n (Proxy :: Proxy 0) of
    Just Refl ->
      case e of
        SmartExp (Match n _x) | n == 0 -> Just SOP.Nil

        SmartExp Match {} -> Nothing

        _ -> error "Embedded pattern synonym used outside 'match' context."
    Nothing ->
      case sameNat n (Proxy :: Proxy 1) of
        Just Refl ->
          case e of
            SmartExp (Match n _x) | n == 1 -> Just SOP.Nil

            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"

makeTag :: TAG -> SmartExp TAG
makeTag x = SmartExp (Const (SingleScalarType (NumSingleType (IntegralNumType TypeTAG))) x)

tagType :: TupR ScalarType TAG
tagType = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeTAG)))


instance Matchable (Maybe Int) where
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
        SmartExp (Match m _x)
          | m == 0
          -> Just SOP.Nil

        SmartExp Match {} -> Nothing

        _ -> error "Embedded pattern synonym used outside 'match' context."
    Nothing -> -- matchJust
      case sameNat n (Proxy :: Proxy 1) of
        Just Refl ->
          case e of
            SmartExp (Match m x)
              | m == 1
              -> Just (
                  mkExp (PrjUnion $ SmartExp $ Union (prjLeft (prjRight x)))
                  :* SOP.Nil)
            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"


instance (POSable (Maybe a), POSable a) => Matchable (Maybe a) where
  build n fs = case sameNat (Proxy @(Choices a)) (Proxy @0) of
    -- a has 0 valid choices (which means we cannot create a Just of this type)
    -- we ignore the implementation for now, because this is not really useful
    Just Refl -> undefined
    -- a has at least 1 choice.
    -- this means that Maybe a always has a tag
    Nothing | Refl :: (EltR (Maybe a) :~: (TAG, FlattenProduct (Fields (Maybe a)))) <- unsafeCoerce Refl
      -> case sameNat n (Proxy :: Proxy 0) of
        -- Produce a Nothing
        Just Refl -> Exp (SmartExp (Pair (unExp $ buildTAG fs) (makeLeft @() @a (SmartExp Smart.Nil))))
        Nothing
          | Exp x :* SOP.Nil <- fs
          -> case sameNat n (Proxy :: Proxy 1) of
            -- Add 1 to the tag because we have skipped 1 choice: Nothing
            Just Refl -> Exp (SmartExp (Pair (unExp $ mkAdd @TAG (constant 1) (buildTAG fs)) (makeRight @() @a (unTag @a x))))
            Nothing -> error $ "Impossible situation requested: Maybe has 2 constructors, constructor " ++ show (natVal n) ++ "is out of bound"
        Nothing -> error "Impossible situation requested: Just a expects a single value, got 0 or more then 1"

  match n (Exp e) = case sameNat (Proxy @(Choices a)) (Proxy @0) of
    -- a has 0 valid choices (which means we cannot create a Just of this type)
    -- we ignore the implementation for now, because this is not really useful
    Just Refl -> undefined
    -- a has at least 1 choice.
    -- this means that Maybe a always has a tag
    Nothing | Refl :: (EltR (Maybe a) :~: (TAG, FlattenProduct (Fields (Maybe a)))) <- unsafeCoerce Refl
      -> case sameNat n (Proxy :: Proxy 0) of
      Just Refl ->
        case e of
          SmartExp (Match m _x)
            | m >= 0
            , m < 1
            -> Just SOP.Nil

          SmartExp Match {} -> Nothing

          _ -> error "Embedded pattern synonym used outside 'match' context."
      Nothing -> -- matchJust
        case sameNat n (Proxy :: Proxy 1) of
          Just Refl ->
            case e of
              SmartExp (Match m x)
                | m >= 1
                , m < fromInteger (natVal $ Proxy @(Choices a))
                -- remove one from the tag as we are not in left anymore
                -- the `tag` function will apply the new tag if necessary
                -> Just (Exp (tag @a (unExp $ mkMin @TAG (constant 1) (Exp $ prjLeft x)) (splitRight @() @a $ prjRight x)) :* SOP.Nil)
              SmartExp Match {} -> Nothing

              _ -> error "Embedded pattern synonym used outside 'match' context."

          Nothing ->
            error "Impossible type encountered"

splitLeft :: forall a b . (POSable a, POSable b) => SmartExp (FlattenProduct (Merge (Fields a ++ '[]) (Fields b ++ '[]))) -> SmartExp (FlattenProduct (Fields a))
splitLeft x = splitLeft' x (emptyFields @a) (emptyFields @b)

splitLeft' :: forall a b . SmartExp (FlattenProduct (Merge (a ++ '[]) (b ++ '[]))) -> ProductType a -> ProductType b -> SmartExp (FlattenProduct a)
splitLeft' _ PTNil _ = SmartExp Smart.Nil
splitLeft' x (PTCons _ ls) PTNil = SmartExp $ Pair (SmartExp $ Union (prjLeft x)) (splitLeft' (prjRight x) ls PTNil)
splitLeft' x (PTCons _ ls) (PTCons _ rs) = SmartExp $ Pair (SmartExp $ Union (prjLeft x)) (splitLeft' (prjRight x) ls rs)

splitRight :: forall a b . (POSable a, POSable b) => SmartExp (FlattenProduct (Merge (Fields a ++ '[]) (Fields b ++ '[]))) -> SmartExp (FlattenProduct (Fields b))
splitRight x = splitRight' x (emptyFields @a) (emptyFields @b)

splitRight' :: forall a b . SmartExp (FlattenProduct (Merge (a ++ '[]) (b ++ '[]))) -> ProductType a -> ProductType b -> SmartExp (FlattenProduct b)
splitRight' _ _ PTNil = SmartExp Smart.Nil
splitRight' x PTNil (PTCons _ rs) = SmartExp $ Pair (SmartExp $ Union (prjLeft x)) (splitRight' (prjRight x) PTNil rs)
splitRight' x (PTCons _ ls) (PTCons _ rs) = SmartExp $ Pair (SmartExp $ Union (prjLeft x)) (splitRight' (prjRight x) ls rs)

makeLeft :: forall a b . (POSable a, POSable b) => SmartExp (FlattenProduct (Fields a)) -> SmartExp (FlattenProduct (Merge (Fields a ++ '[]) (Fields b ++ '[])))
makeLeft x = makeLeft' x (emptyFields @a) (emptyFields @b)

makeLeft' :: forall a b . SmartExp (FlattenProduct a) -> ProductType a -> ProductType b -> SmartExp (FlattenProduct (Merge (a ++ '[]) (b ++ '[])))
makeLeft' _ PTNil PTNil = SmartExp Smart.Nil
makeLeft' x PTNil (PTCons _ rs) = SmartExp (Pair (SmartExp (Union (SmartExp (LiftUnion (SmartExp (Const (SingleScalarType UndefSingleType) POS.Undef)))))) (makeLeft' x PTNil rs))
makeLeft' x (PTCons _ ls) PTNil = SmartExp (Pair (SmartExp (Union (prjLeft x))) (makeLeft' (prjRight x) ls PTNil))
makeLeft' x (PTCons _ ls) (PTCons _ rs) = SmartExp (Pair (SmartExp (Union (prjLeft x))) (makeLeft' (prjRight x) ls rs))

prjLeft :: SmartExp (x, xs) -> SmartExp x
prjLeft = SmartExp . Prj PairIdxLeft

prjRight :: SmartExp (x, xs) -> SmartExp xs
prjRight = SmartExp . Prj PairIdxRight

makeRight :: forall a b . (POSable a, POSable b) => SmartExp (FlattenProduct (Fields b)) -> SmartExp (FlattenProduct (Merge (Fields a ++ '[]) (Fields b ++ '[])))
makeRight x = makeRight' x (emptyFields @a) (emptyFields @b)

makeRight' :: forall a b . SmartExp (FlattenProduct b) -> ProductType a -> ProductType b -> SmartExp (FlattenProduct (Merge (a ++ '[]) (b ++ '[])))
makeRight' _ PTNil PTNil = SmartExp Smart.Nil
makeRight' x PTNil (PTCons _ rs) = SmartExp (Pair (SmartExp (Union (prjLeft x))) (makeRight' (prjRight x) PTNil rs))
makeRight' x (PTCons _ ls) PTNil = SmartExp (Pair (SmartExp (Union (SmartExp (LiftUnion (SmartExp (Const (SingleScalarType UndefSingleType) POS.Undef)))))) (makeRight' x ls PTNil))
makeRight' x (PTCons _ ls) (PTCons _ rs) = SmartExp (Pair (SmartExp (Union (prjLeft x))) (makeRight' (prjRight x) ls rs))

unTag :: forall x . (POSable x) => SmartExp (EltR x) -> SmartExp (FlattenProduct (Fields x))
unTag x = case eltRType @x of
  SingletonType -> SmartExp (Pair (SmartExp (LiftUnion x)) (SmartExp Smart.Nil))
  TaglessType -> x
  TaggedType -> prjRight x

tag :: forall x . (POSable x) => SmartExp TAG -> SmartExp (FlattenProduct (Fields x)) -> SmartExp (EltR x)
tag t x = case eltRType @x of
  SingletonType -> SmartExp $ PrjUnion $ prjLeft x
  TaglessType -> x
  TaggedType -> SmartExp $ Pair t x

instance (POSable (Either a b), POSable a, POSable b) => Matchable (Either a b) where
  -- type Choices' (Either a b) = OuterChoices (Either a b)

  build n fs
    -- this is only not true if either left or right has a tag of type Finite 0
    -- types with tags of Finite 0 have no constructors, and are quite useless
    | Refl :: (EltR (Either a b) :~: (TAG, FlattenProduct (Fields (Either a b)))) <- unsafeCoerce Refl
    = case sameNat n (Proxy :: Proxy 0) of
      -- we have chosen constructor 0 (Left)
      Just Refl -> case emptyFields @a of
        -- Left has no fields
        PTNil ->  Exp (SmartExp (Pair (unExp $ buildTAG fs) (undefPairs @(Fields b) (emptyFields @b))))
        -- Left has fields
        PTCons st pt -> Exp (SmartExp (Pair (unExp $ buildTAG fs) undefined))
      Nothing ->
        case sameNat n (Proxy :: Proxy 1) of
          -- we have chosen constructor 1 (Right)
          Just Refl -> case emptyFields @a of
            PTNil -> case fs of
              x :* SOP.Nil -> case eltRType @b of  -- disambiguate between tagless and tagged b's
                SingletonType -> Exp (SmartExp (Pair (unExp $ buildTAG fs) (SmartExp (Pair (SmartExp (Union (SmartExp (LiftUnion (unExp x))))) (SmartExp Smart.Nil)))))
                TaglessType -> Exp (SmartExp (Pair (unExp $ buildTAG fs) (mergePairs @(Fields b) (emptyFields @b) (unExp x))))
                TaggedType -> Exp (SmartExp (Pair (unExp $ buildTAG fs) (mergePairs @(Fields b) (emptyFields @b) (SmartExp (Prj PairIdxRight (unExp x))))))
            (PTCons x xs) -> Exp (SmartExp (Pair (unExp $ buildTAG fs) undefined))
          Nothing -> error "Index out of bounds"

undefPairs :: forall xs . ProductType xs -> SmartExp (FlattenProduct (Merge '[] (xs ++ '[])))
undefPairs PTNil = SmartExp Smart.Nil
undefPairs (PTCons x xs) = SmartExp (Pair (SmartExp (Union (SmartExp (LiftUnion (unExp $ constant POS.Undef))))) (undefPairs xs))

mergePairs :: forall xs . ProductType xs -> SmartExp (FlattenProduct xs) -> SmartExp (FlattenProduct (Merge '[] (xs ++ '[])))
mergePairs PTNil _ = SmartExp Smart.Nil
mergePairs (PTCons x xs) y = SmartExp (Pair (SmartExp (Union (SmartExp (Prj PairIdxLeft y)))) (mergePairs xs (SmartExp (Prj PairIdxRight y))))

-- like combineProducts, but lifted to the AST
buildTAG :: (All POSable xs) => NP Exp xs -> Exp TAG
buildTAG SOP.Nil = Exp $ makeTag 0
buildTAG (x :* xs) = combineProduct x (buildTAG xs)

-- like Finite.combineProduct, but lifted to the AST
-- basically `tag x + tag y * natVal x`
combineProduct :: forall x. (POSable x) => Exp x -> Exp TAG -> Exp TAG
combineProduct x y = case sameNat (emptyChoices @x) (Proxy :: Proxy 1) of
  -- untagged type: `tag x = 0`, `natVal x = 1`
  Just Refl -> y
  -- tagged type
  Nothing
    | Refl :: (EltR x :~: (TAG, y)) <- unsafeCoerce Refl
    -> mkAdd (mkExp $ Prj PairIdxLeft (unExp x)) (mkMul y (constant (fromInteger $ natVal (emptyChoices @x))))
