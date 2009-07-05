{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}

-- |Embedded array processing language: smart expression constructors
--
--  Copyright (c) [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--  This modules defines the the user-visible embedded language using more
--  convenient higher-order abstract syntax (instead of de Bruijn indices).
--  Moreover, it defines smart constructors to construct programs.

module Data.Array.Accelerate.Smart (

  -- * HOAS AST
  Exp(..), convertExp, convertFun1, convertFun2,

  -- * Class of element types in array computations
  Elem,

  -- * Constructors for literals
  mkVal, mkNumVal,

  -- * Constructors for constants
  mkMinBound, mkMaxBound, mkPi,

  -- * Constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkIDiv, mkMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq,
  mkEq, mkNEq, mkMax, mkMin, mkLAnd, mkLOr, mkLNot,

) where

-- standard library
import Data.Maybe
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST hiding (Exp, OpenExp(..))
import qualified Data.Array.Accelerate.AST as AST
import Data.Array.Accelerate.Pretty
import Data.Array.Accelerate.Typeable


-- |HOAS AST
-- -

-- HOAS expressions mirror the constructors of `AST.OpenExp', but with the
-- `Tag' constructor instead of variables in the form of de Bruijn indices.
--
data Exp t where
  -- Tag used during the conversion to de Bruijn indices
  Tag         :: Typeable1 (Idx env) => TupleType t -> Idx env t -> Exp t

  -- All the same constructors as `AST.OpenExp'
  Const       :: TupleType t -> t           -> Exp t
  Pair        :: Exp s -> Exp t             -> Exp (s, t)
  Fst         :: Exp (s, t)                 -> Exp s
  Snd         :: Exp (s, t)                 -> Exp t
  Cond        :: Exp Bool -> Exp t -> Exp t -> Exp t
  PrimConst   :: PrimConst t                -> Exp t
  PrimApp     :: PrimFun (a -> r) -> Exp a  -> Exp r
  IndexScalar :: Arr dim t -> Exp dim       -> Exp t
  Shape       :: Arr dim e                  -> Exp dim


-- |Conversion from HOAS to de Bruijn AST
-- -

-- |Convert an open expression
--
convertExp :: Typeable env => Exp t -> AST.OpenExp env t
convertExp (Tag ty idx)      = AST.Var ty (fromJust (cast1 idx))
                                  -- can't go wrong unless the library is wrong!
convertExp (Const ty v)      = AST.Const ty v
convertExp (Pair e1 e2)      = AST.Pair (convertExp e1) (convertExp e2)
convertExp (Fst e)           = AST.Fst (convertExp e)
convertExp (Snd e)           = AST.Snd (convertExp e)
convertExp (Cond e1 e2 e3)   = AST.Cond (convertExp e1) (convertExp e2) 
                                        (convertExp e3)
convertExp (PrimConst c)     = AST.PrimConst c
convertExp (PrimApp p e)     = AST.PrimApp p (convertExp e)
convertExp (IndexScalar a e) = AST.IndexScalar a (convertExp e)
convertExp (Shape a)         = AST.Shape a

-- |Convert a unary functions
--
convertFun1 :: forall a b. IsTuple a 
            => (Exp a -> Exp b) -> AST.Fun (a -> b)
convertFun1 f = Lam (Body openF)
  where
    a     = Tag tupleType (ZeroIdx :: Idx ((), a) a)
    openF = convertExp (f a)

-- |Convert a binary functions
--
convertFun2 :: forall a b c. (IsTuple a, IsTuple b) 
            => (Exp a -> Exp b -> Exp c) -> AST.Fun (a -> b -> c)
convertFun2 f = Lam (Lam (Body openF))
  where
    a     = Tag tupleType (SuccIdx ZeroIdx :: Idx (((), a), b) a)
    b     = Tag tupleType (ZeroIdx         :: Idx (((), a), b) b)
    openF = convertExp (f a b)

instance Show (Exp t) where
  show e = show (convertExp e :: AST.OpenExp () t)


-- |Processed data types: tuples of scalars
-- -

class Elem a where
  type ElemRepr a :: *
  elemType :: {-dummy-} a -> TupleType (ElemRepr a)
  fromElem :: a -> ElemRepr a
  toElem   :: ElemRepr a -> a

instance Elem () where
  type ElemRepr () = ()
  elemType _ = UnitTuple
  fromElem = id
  toElem   = id

{-
instance Elem Int where
  type ElemRepr Int = ((), Int)
  elemType _ = UnitTuple `PairTuple` SingleTuple scalarType
  fromElem v = ((), v)
  toElem ((), v) = v
 -}

instance Elem Int where
  type ElemRepr Int = Int
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int8 where
  type ElemRepr Int8 = Int8
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int16 where
  type ElemRepr Int16 = Int16
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int32 where
  type ElemRepr Int32 = Int32
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Int64 where
  type ElemRepr Int64 = Int64
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word where
  type ElemRepr Word = Word
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word8 where
  type ElemRepr Word8 = Word8
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word16 where
  type ElemRepr Word16 = Word16
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word32 where
  type ElemRepr Word32 = Word32
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Word64 where
  type ElemRepr Word64 = Word64
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CShort where
  type ElemRepr CShort = CShort
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CUShort where
  type ElemRepr CUShort = CUShort
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CInt where
  type ElemRepr CInt = CInt
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CUInt where
  type ElemRepr CUInt = CUInt
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CLong where
  type ElemRepr CLong = CLong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CULong where
  type ElemRepr CULong = CULong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CLLong where
  type ElemRepr CLLong = CLLong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CULLong where
  type ElemRepr CULLong = CULLong
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Float where
  type ElemRepr Float = Float
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Double where
  type ElemRepr Double = Double
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CFloat where
  type ElemRepr CFloat = CFloat
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CDouble where
  type ElemRepr CDouble = CDouble
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Bool where
  type ElemRepr Bool = Bool
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem Char where
  type ElemRepr Char = Char
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CChar where
  type ElemRepr CChar = CChar
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CSChar where
  type ElemRepr CSChar = CSChar
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance Elem CUChar where
  type ElemRepr CUChar = CUChar
  elemType _ = SingleTuple scalarType
  fromElem = id
  toElem   = id

instance (Elem a, Elem b) => Elem (a, b) where
  type ElemRepr (a, b) = (ElemRepr a, ElemRepr b)
  elemType (_::(a, b)) 
    = PairTuple (elemType (undefined :: a)) (elemType (undefined :: b))
  fromElem (a, b) = (fromElem a, fromElem b)
  toElem   (a, b) = (toElem a, toElem b)

instance (Elem a, Elem b, Elem c) => Elem (a, b, c) where
  type ElemRepr (a, b, c) = (ElemRepr (a, b), ElemRepr c)
  elemType (_::(a, b, c)) 
    = PairTuple (elemType (undefined :: (a, b))) (elemType (undefined :: c))
  fromElem (a, b, c) = (fromElem (a, b), fromElem c)
  toElem   (ab, c) = let (a, b) = toElem ab in (a, b, toElem c)
  
instance (Elem a, Elem b, Elem c, Elem d) => Elem (a, b, c, d) where
  type ElemRepr (a, b, c, d) = (ElemRepr (a, b, c), ElemRepr d)
  elemType (_::(a, b, c, d)) 
    = PairTuple (elemType (undefined :: (a, b, c))) (elemType (undefined :: d))
  fromElem (a, b, c, d) = (fromElem (a, b, c), fromElem d)
  toElem   (abc, d) = let (a, b, c) = toElem abc in (a, b, c, toElem d)

instance (Elem a, Elem b, Elem c, Elem d, Elem e) => Elem (a, b, c, d, e) where
  type ElemRepr (a, b, c, d, e) = (ElemRepr (a, b, c, d), ElemRepr e)
  elemType (_::(a, b, c, d, e)) 
    = PairTuple (elemType (undefined :: (a, b, c, d))) 
                (elemType (undefined :: e))
  fromElem (a, b, c, d, e) = (fromElem (a, b, c, d), fromElem e)
  toElem   (abcd, e) = let (a, b, c, d) = toElem abcd in (a, b, c, d, toElem e)


-- |Smart constructors to construct HOAS AST expressions
-- -

-- |Smart constructor for literals
-- -

mkVal :: Elem t => t -> Exp t
mkVal = Const (elemType undefined)

mkNumVal :: IsNum t => t -> Exp t
mkNumVal = Const (UnitTuple (NumScalarType numType))

-- |Smart constructor for constants
-- -

mkMinBound :: IsBounded t => Exp t
mkMinBound = PrimConst (PrimMinBound boundedType)

mkMaxBound :: IsBounded t => Exp t
mkMaxBound = PrimConst (PrimMaxBound boundedType)

mkPi :: IsFloating r => Exp r
mkPi = PrimConst (PrimPi floatingType)

-- |Smart constructors for primitive applications
-- -

-- Operators from Num

mkAdd :: IsNum t => Exp t -> Exp t -> Exp t
mkAdd x y = PrimAdd numType `PrimApp` (x `Pair` y)

mkSub :: IsNum t => Exp t -> Exp t -> Exp t
mkSub x y = PrimSub numType `PrimApp` (x `Pair` y)

mkMul :: IsNum t => Exp t -> Exp t -> Exp t
mkMul x y = PrimMul numType `PrimApp` (x `Pair` y)

mkNeg :: IsNum t => Exp t -> Exp t
mkNeg x = PrimNeg numType `PrimApp` x

mkAbs :: IsNum t => Exp t -> Exp t
mkAbs x = PrimAbs numType `PrimApp` x

mkSig :: IsNum t => Exp t -> Exp t
mkSig x = PrimSig numType `PrimApp` x

-- Operators from Integral & Bits

mkQuot :: IsIntegral t => Exp t -> Exp t -> Exp t
mkQuot x y = PrimQuot integralType `PrimApp` (x `Pair` y)

mkRem :: IsIntegral t => Exp t -> Exp t -> Exp t
mkRem x y = PrimRem integralType `PrimApp` (x `Pair` y)

mkIDiv :: IsIntegral t => Exp t -> Exp t -> Exp t
mkIDiv x y = PrimIDiv integralType `PrimApp` (x `Pair` y)

mkMod :: IsIntegral t => Exp t -> Exp t -> Exp t
mkMod x y = PrimMod integralType `PrimApp` (x `Pair` y)

mkBAnd :: IsIntegral t => Exp t -> Exp t -> Exp t
mkBAnd x y = PrimBAnd integralType `PrimApp` (x `Pair` y)

mkBOr :: IsIntegral t => Exp t -> Exp t -> Exp t
mkBOr x y = PrimBOr integralType `PrimApp` (x `Pair` y)

mkBXor :: IsIntegral t => Exp t -> Exp t -> Exp t
mkBXor x y = PrimBXor integralType `PrimApp` (x `Pair` y)

mkBNot :: IsIntegral t => Exp t -> Exp t
mkBNot x = PrimBNot integralType `PrimApp` x
  -- FIXME: add shifts

-- Operators from Fractional, Floating, RealFrac & RealFloat

mkFDiv :: IsFloating t => Exp t -> Exp t -> Exp t
mkFDiv x y = PrimFDiv floatingType `PrimApp` (x `Pair` y)

mkRecip :: IsFloating t => Exp t -> Exp t
mkRecip x = PrimRecip floatingType `PrimApp` x
  -- FIXME: add operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: IsScalar t => Exp t -> Exp t -> Exp Bool
mkLt x y = PrimLt scalarType `PrimApp` (x `Pair` y)

mkGt :: IsScalar t => Exp t -> Exp t -> Exp Bool
mkGt x y = PrimGt scalarType `PrimApp` (x `Pair` y)

mkLtEq :: IsScalar t => Exp t -> Exp t -> Exp Bool
mkLtEq x y = PrimLtEq scalarType `PrimApp` (x `Pair` y)

mkGtEq :: IsScalar t => Exp t -> Exp t -> Exp Bool
mkGtEq x y = PrimGtEq scalarType `PrimApp` (x `Pair` y)

mkEq :: IsScalar t => Exp t -> Exp t -> Exp Bool
mkEq x y = PrimEq scalarType `PrimApp` (x `Pair` y)

mkNEq :: IsScalar t => Exp t -> Exp t -> Exp Bool
mkNEq x y = PrimLt scalarType `PrimApp` (x `Pair` y)

mkMax :: IsScalar t => Exp t -> Exp t -> Exp t
mkMax x y = PrimMax scalarType `PrimApp` (x `Pair` y)

mkMin :: IsScalar t => Exp t -> Exp t -> Exp t
mkMin x y = PrimMin scalarType `PrimApp` (x `Pair` y)

-- Logical operators

mkLAnd :: Exp Bool -> Exp Bool -> Exp Bool
mkLAnd x y = PrimLAnd `PrimApp` (x `Pair` y)

mkLOr :: Exp Bool -> Exp Bool -> Exp Bool
mkLOr x y = PrimLOr `PrimApp` (x `Pair` y)

mkLNot :: Exp Bool -> Exp Bool
mkLNot x = PrimLNot `PrimApp` x

-- FIXME: Character conversions

-- FIXME: Numeric conversions
