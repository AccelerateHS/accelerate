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

buildFields :: forall n a . (POSable a, Elt a) => Proxy n -> NP SmartExp (Index (SOPCode a) n) -> SmartExp (FlattenProduct (Fields a))
buildFields _ a = case emptyFields @a of
  PTNil -> case constant () of { Exp se -> se }
  PTCons st pt -> case a of
    -- SOP.Nil -> SmartExp (Pair (someFunction st) undefined)
    (x :* xs) -> SmartExp (Pair undefined undefined)

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
makeTag x = undefined -- SmartExp (Const (TupRsingle (tagType x)))

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
                  (Const
                    (scalarType @(SumScalar (Undef, (Int, ()))))
                    (PickScalar POS.Undef)
                  )
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
                      (Right (
                        SmartExp
                         (Union
                            (Left x')
                         )
                      ))
                    )
                  )
                  (SmartExp Smart.Nil)
              ))
          )
        )
      Nothing -> error "Impossible type encountered"

  match n exp@(Exp e) = case sameNat n (Proxy :: Proxy 0) of
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
              -> Just
                  (Exp
                    (SmartExp
                      (PrjUnion
                        UnionIdxLeft
                        (SmartExp
                          (PrjUnion
                            UnionIdxRight
                            (SmartExp
                              (Prj
                                PairIdxLeft
                                (SmartExp (Prj PairIdxRight x))
                              ))
                          ))
                      )) :* SOP.Nil)
            SmartExp Match {} -> Nothing

            _ -> error "Embedded pattern synonym used outside 'match' context."

        Nothing ->
          error "Impossible type encountered"
