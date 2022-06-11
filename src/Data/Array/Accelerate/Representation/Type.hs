{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Representation.Type
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Representation.Type
  where

import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Formatting
import Language.Haskell.TH.Extra

import GHC.TypeLits


-- | Both arrays (Acc) and expressions (Exp) are represented as nested
-- pairs consisting of:
--
--   * unit (void)
--
--   * pairs: representing compound values (i.e. tuples) where each component
--     will be stored in a separate array.
--
--   * single array / scalar types
--     in case of expressions: values which go in registers. These may be single value
--     types such as int and float, or SIMD vectors of single value types such
--     as <4 * float>. We do not allow vectors-of-vectors.
--
data TupR s a where
  TupRunit   ::                         TupR s ()
  TupRsingle :: s a                  -> TupR s a
  TupRpair   :: TupR s a -> TupR s b -> TupR s (a, b)

instance Show (TupR ScalarType a) where
  show TupRunit       = "()"
  show (TupRsingle t) = show t
  show (TupRpair a b) = "(" ++ show a ++ "," ++ show b ++ ")"

formatTypeR :: Format r (TypeR a -> r)
formatTypeR = later $ \case
  TupRunit     -> "()"
  TupRsingle t -> bformat formatScalarType t
  TupRpair a b -> bformat (parenthesised (formatTypeR % "," % formatTypeR)) a b

type TypeR = TupR ScalarType

rnfTupR :: (forall b. s b -> ()) -> TupR s a -> ()
rnfTupR _ TupRunit       = ()
rnfTupR f (TupRsingle s) = f s
rnfTupR f (TupRpair a b) = rnfTupR f a `seq` rnfTupR f b

rnfTypeR :: TypeR t -> ()
rnfTypeR = rnfTupR rnfScalarType

liftTupR :: (forall b. s b -> CodeQ (s b)) -> TupR s a -> CodeQ (TupR s a)
liftTupR _ TupRunit       = [|| TupRunit ||]
liftTupR f (TupRsingle s) = [|| TupRsingle $$(f s) ||]
liftTupR f (TupRpair a b) = [|| TupRpair $$(liftTupR f a) $$(liftTupR f b) ||]

liftTypeR :: TypeR t -> CodeQ (TypeR t)
liftTypeR TupRunit         = [|| TupRunit ||]
liftTypeR (TupRsingle t)   = [|| TupRsingle $$(liftScalarType t) ||]
liftTypeR (TupRpair ta tb) = [|| TupRpair $$(liftTypeR ta) $$(liftTypeR tb) ||]

liftTypeQ :: TypeR t -> TypeQ
liftTypeQ = tuple
  where
    tuple :: TypeR t -> TypeQ
    tuple TupRunit         = [t| () |]
    tuple (TupRpair t1 t2) = [t| ($(tuple t1), $(tuple t2)) |]
    tuple (TupRsingle t)   = scalar t

    scalar :: ScalarType t -> TypeQ
    scalar (NumScalarType t) = num t
    scalar (BitScalarType t) = bit t

    bit :: BitType t -> TypeQ
    bit TypeBit      = [t| Bit |]
    bit (TypeMask n) = [t| Vec $(litT (numTyLit (natVal' n))) Bit |]

    num :: NumType t -> TypeQ
    num (IntegralNumType t) = integral t
    num (FloatingNumType t) = floating t

    integral :: IntegralType t -> TypeQ
    integral = \case
      SingleIntegralType t   -> [t| $(single t) |]
      VectorIntegralType n t -> [t| Vec $(litT (numTyLit (natVal' n))) $(single t) |]
      where
        single :: SingleIntegralType t -> TypeQ
        single TypeInt8    = [t| Int8 |]
        single TypeInt16   = [t| Int16 |]
        single TypeInt32   = [t| Int32 |]
        single TypeInt64   = [t| Int64 |]
        single TypeInt128  = [t| Int128 |]
        single TypeWord8   = [t| Word8 |]
        single TypeWord16  = [t| Word16 |]
        single TypeWord32  = [t| Word32 |]
        single TypeWord64  = [t| Word64 |]
        single TypeWord128 = [t| Word128 |]

    floating :: FloatingType t -> TypeQ
    floating = \case
      SingleFloatingType t   -> [t| $(single t) |]
      VectorFloatingType n t -> [t| Vec $(litT (numTyLit (natVal' n))) $(single t) |]
      where
        single :: SingleFloatingType t -> TypeQ
        single TypeFloat16  = [t| Half |]
        single TypeFloat32  = [t| Float |]
        single TypeFloat64  = [t| Double |]
        single TypeFloat128 = [t| Float128 |]


runQ $
  let
      mkT :: Int -> Q Dec
      mkT n =
        let xs  = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            ts  = map varT xs
            rhs = foldl (\a b -> [t| ($a, $b) |]) [t| () |] ts
         in
         tySynD (mkName ("Tup" ++ show n)) (map plainTV xs) rhs
  in
  mapM mkT [2..16]

