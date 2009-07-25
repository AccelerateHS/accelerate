{-# LANGUAGE GADTs, TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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

  -- * Array processing computation monad
  AP, runAP, wrapComp, wrapComp2,

  -- * HOAS AST
  Arr(..), Scalar, Vector, Exp(..), 

  -- * Conversions
  convertArray, convertArr, convertExp, convertFun1, convertFun2, 

  -- * Smart constructors for array operations
  mkIndex, mkReplicate, {- mkZip, -}

  -- * Smart constructors for literals
  exp,

  -- * Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi,

  -- * Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkIDiv, mkMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq,
  mkEq, mkNEq, mkMax, mkMin, mkLAnd, mkLOr, mkLNot,

) where

-- avoid clashes with Prelude functions
import Prelude hiding (exp)

-- standard library
import Control.Monad.State
import Data.Maybe
import Data.Typeable

-- friends
import Data.Array.Accelerate.Array.Representation hiding (Array(..))
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.AST hiding (Exp, OpenExp(..), Arr(..), Scalar)
import Data.Array.Accelerate.Pretty
import qualified Data.Array.Accelerate.AST                  as AST
import qualified Data.Array.Accelerate.Array.Representation as AST


-- |Conversion of surface to internal types
-- ----------------------------------------

-- Conversion of type representations
--
{-
convertIntegralType :: IntegralType a -> IntegralType (ElemRepr a)
convertIntegralType ty@(TypeInt _)     = ty
convertIntegralType ty@(TypeInt8 _)    = ty
convertIntegralType ty@(TypeInt16 _)   = ty
convertIntegralType ty@(TypeInt32 _)   = ty
convertIntegralType ty@(TypeInt64 _)   = ty
convertIntegralType ty@(TypeWord _)    = ty
convertIntegralType ty@(TypeWord8 _)   = ty
convertIntegralType ty@(TypeWord16 _)  = ty
convertIntegralType ty@(TypeWord32 _)  = ty
convertIntegralType ty@(TypeWord64 _)  = ty
convertIntegralType ty@(TypeCShort _)  = ty
convertIntegralType ty@(TypeCUShort _) = ty
convertIntegralType ty@(TypeCInt _)    = ty
convertIntegralType ty@(TypeCUInt _)   = ty
convertIntegralType ty@(TypeCLong _)   = ty
convertIntegralType ty@(TypeCULong _)  = ty
convertIntegralType ty@(TypeCLLong _)  = ty
convertIntegralType ty@(TypeCULLong _) = ty

convertFloatingType :: FloatingType a -> FloatingType (ElemRepr a)
convertFloatingType ty@(TypeFloat _)   = ty
convertFloatingType ty@(TypeDouble _)  = ty
convertFloatingType ty@(TypeCFloat _)  = ty
convertFloatingType ty@(TypeCDouble _) = ty

convertNonNumType :: NonNumType a -> NonNumType (ElemRepr a)
convertNonNumType ty@(TypeBool   _) = ty
convertNonNumType ty@(TypeChar   _) = ty
convertNonNumType ty@(TypeCChar  _) = ty
convertNonNumType ty@(TypeCSChar _) = ty
convertNonNumType ty@(TypeCUChar _) = ty

convertNumType :: NumType a -> NumType (ElemRepr a)
convertNumType (IntegralNumType ty) = IntegralNumType $ convertIntegralType ty
convertNumType (FloatingNumType ty) = FloatingNumType $ convertFloatingType ty

convertBoundedType :: BoundedType a -> BoundedType (ElemRepr a)
convertBoundedType (IntegralBoundedType ty) 
  = IntegralBoundedType (convertIntegralType ty)
convertBoundedType (NonNumBoundedType ty) 
  = NonNumBoundedType (convertNonNumType ty)

convertScalarType :: ScalarType a -> ScalarType (ElemRepr a)
convertScalarType (NumScalarType ty)    = NumScalarType $ convertNumType ty
convertScalarType (NonNumScalarType ty) 
  = NonNumScalarType $ convertNonNumType ty
-}
{-
-- |Conversion of slice indices
--
convertSlice :: forall sl. SliceIx sl
             => sl -> SliceIndex (ToShapeRepr (Slice    sl))
                                 (ToShapeRepr (CoSlice  sl))
                                 (ToShapeRepr (SliceDim sl))
convertSlice = cvt . toShapeRepr
  where
    cvt :: ToShapeRepr sl -> SliceIndex (ToShapeRepr (Slice    sl))
                                        (ToShapeRepr (CoSlice  sl))
                                        (ToShapeRepr (SliceDim sl))
    cvt () = SliceNil
-}

-- |HOAS AST
-- ---------

-- |Array representation for the surface language
--
data Arr dim e where
  Arr :: (Ix dim, Elem e) => String -> Arr dim e

-- |Scalars of the surface language
--
type Scalar a = Arr DIM0 a

-- |Scalars of the surface language
--
type Vector a = Arr DIM1 a

-- HOAS expressions mirror the constructors of `AST.OpenExp', but with the
-- `Tag' constructor instead of variables in the form of de Bruijn indices.
-- Moreover, HOAS expression use n-tuples and the type class 'Elem' to
-- constrain element types, whereas `AST.OpenExp' uses nested pairs and the 
-- class 'IsTuple'.
--
data Exp t where
    -- Needed for conversion to de Bruijn form
  Tag         :: Elem t
              => Int                        -> Exp t
                 -- environment size at defining occurrence

    -- All the same constructors as `AST.OpenExp'
  Const       :: Elem t 
              => t                          -> Exp t
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

-- A layout of an environment an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t 
              => Layout env env' -> Idx env t -> Layout env (env', t)

-- Project the nth index out of an environment layout
--
prjIdx :: Typeable t => Int -> Layout env env' -> Idx env t
prjIdx 0 (PushLayout _ ix) = fromJust (gcast ix)
                               -- can't go wrong unless the library is wrong!
prjIdx n (PushLayout l _)  = prjIdx (n - 1) l
prjIdx _ EmptyLayout       
  = error "Data.Array.Accelerate.Smart.prjIdx: internal error"

-- |Convert an open expression with the given environment layout
--
convertOpenExp :: forall t env. 
                  Layout env env -> Exp t -> AST.OpenExp env (ElemRepr t)
convertOpenExp lyt = cvt
  where
    cvt :: forall t'. Exp t' -> AST.OpenExp env (ElemRepr t')
    cvt (Tag i)           = AST.Var (elemType (undefined::t')) (prjIdx i lyt)
    cvt (Const v)         = AST.Const (elemType (undefined::t')) (fromElem v)
-- FIXME:
--    cvt (Pair e1 e2)      = AST.Pair (cvt e1) (cvt e2)
--    cvt (Fst e)           = AST.Fst (cvt e)
--    cvt (Snd e)           = AST.Snd (cvt e)
    cvt (Cond e1 e2 e3)   = AST.Cond (cvt e1) (cvt e2) (cvt e3)
--    cvt (PrimConst c)     = AST.PrimConst (convertPrimConst c)
    cvt (PrimConst c)     = AST.PrimConst c
--    cvt (PrimApp p e)     = AST.PrimApp (convertPrimFun p) (cvt e)
    cvt (IndexScalar a e) = AST.IndexScalar (convertArr a) (cvt e)
    cvt (Shape a)         = AST.Shape (convertArr a)

-- |Convert a closed expression
--
convertExp :: Exp t -> AST.Exp (ElemRepr t)
convertExp = convertOpenExp EmptyLayout

{-
-- |Convert a primitive constant
--
convertPrimConst :: PrimConst a -> PrimConst (ElemRepr a)
convertPrimConst (PrimMinBound ty) = PrimMinBound $ convertBoundedType ty
convertPrimConst (PrimMaxBound ty) = PrimMinBound $ convertBoundedType ty
convertPrimConst (PrimPi ty)       = PrimPi $ convertFloatingType ty
-}

{-
-- |Convert a primitive operation
--
convertPrimFun :: PrimFun (a -> b) -> PrimFun (ElemRepr a -> ElemRepr b)
convertPrimFun (PrimAdd ty)   = PrimAdd (convertNumType ty)
convertPrimFun (PrimSub ty)   = PrimSub (convertNumType ty)
convertPrimFun (PrimMul ty)   = PrimMul (convertNumType ty)
convertPrimFun (PrimNeg ty)   = PrimNeg (convertNumType ty)
convertPrimFun (PrimAbs ty)   = PrimAbs (convertNumType ty)
convertPrimFun (PrimSig ty)   = PrimSig (convertNumType ty)
convertPrimFun (PrimQuot ty)  = PrimQuot (convertIntegralType ty)
convertPrimFun (PrimRem ty)   = PrimRem (convertIntegralType ty)
convertPrimFun (PrimIDiv ty)  = PrimIDiv (convertIntegralType ty)
convertPrimFun (PrimMod ty)   = PrimMod (convertIntegralType ty)
convertPrimFun (PrimBAnd ty)  = PrimBAnd (convertIntegralType ty)
convertPrimFun (PrimBOr ty)   = PrimBOr (convertIntegralType ty)
convertPrimFun (PrimBXor ty)  = PrimBXor (convertIntegralType ty)
convertPrimFun (PrimBNot ty)  = PrimBNot (convertIntegralType ty)
convertPrimFun (PrimFDiv ty)  = PrimFDiv (convertFloatingType ty)
convertPrimFun (PrimRecip ty) = PrimRecip (convertFloatingType ty)
convertPrimFun (PrimLt ty)    = PrimLt (convertScalarType ty)
convertPrimFun (PrimGt ty)    = PrimGt (convertScalarType ty)
convertPrimFun (PrimLtEq ty)  = PrimLtEq (convertScalarType ty)
convertPrimFun (PrimGtEq ty)  = PrimGtEq (convertScalarType ty)
convertPrimFun (PrimEq ty)    = PrimEq (convertScalarType ty)
convertPrimFun (PrimNEq ty)   = PrimNEq (convertScalarType ty)
convertPrimFun (PrimMax ty)   = PrimMax (convertScalarType ty)
convertPrimFun (PrimMin ty)   = PrimMin (convertScalarType ty)
convertPrimFun PrimLAnd       = PrimLAnd
convertPrimFun PrimLOr        = PrimLOr
convertPrimFun PrimLNot       = PrimLNot
convertPrimFun PrimOrd        = PrimOrd
convertPrimFun PrimChr        = PrimChr
convertPrimFun PrimRoundFloatInt = PrimRoundFloatInt
-}
-- |Convert surface array representation to the internal one
--
convertArray :: forall dim e. 
                Array dim e -> AST.Array (ElemRepr dim) (ElemRepr e)
convertArray (Array {arrayShape = shape, arrayId = id, arrayPtr = ptr})
  = AST.Array {
      AST.arrayShape = fromElem shape, 
      AST.arrayElemType = elemType (undefined::e), 
      AST.arrayId = id, 
      AST.arrayPtr = ptr
    }

-- |Convert surface AP array representation to the internal one
--
convertArr :: forall dim e. Arr dim e -> AST.Arr (ElemRepr dim) (ElemRepr e)
convertArr (Arr idStr) = AST.Arr (elemType (undefined :: e)) idStr

-- |Convert a unary functions
--
convertFun1 :: forall a b. Elem a
            => (Exp a -> Exp b) -> AST.Fun (ElemRepr a -> ElemRepr b)
convertFun1 f = Lam (Body openF)
  where
    a     = Tag 0
    lyt   = EmptyLayout 
            `PushLayout` 
            (ZeroIdx :: Idx ((), ElemRepr a) (ElemRepr a))
    openF = convertOpenExp lyt (f a)

-- |Convert a binary functions
--
convertFun2 :: forall a b c. (Elem a, Elem b) 
            => (Exp a -> Exp b -> Exp c) 
            -> AST.Fun (ElemRepr a -> ElemRepr b -> ElemRepr c)
convertFun2 f = Lam (Lam (Body openF))
  where
    a     = Tag 1
    b     = Tag 0
    lyt   = EmptyLayout 
            `PushLayout`
            (SuccIdx ZeroIdx :: Idx (((), ElemRepr a), ElemRepr b) (ElemRepr a))
            `PushLayout`
            (ZeroIdx         :: Idx (((), ElemRepr a), ElemRepr b) (ElemRepr b))
    openF = convertOpenExp lyt (f a b)

instance Show (Exp t) where
  show e = show (convertExp e :: AST.Exp (ElemRepr t))


-- |Monad of collective operations
-- -------------------------------

-- |Array processing computations as a state transformer
--
type AP a = State APstate a

data APstate = APstate 
               { comps :: Comps        -- the program so far (reversed list)
               , sym   :: Int          -- next unique variable name
               }

unComps :: APstate -> [CompBinding]
unComps s = case comps s of Comps cs -> cs

initialAPstate :: APstate
initialAPstate = APstate (Comps []) 0

runAP :: AP a -> Comps
runAP = reverseComps . comps . flip execState initialAPstate
  where
    reverseComps (Comps cs) = Comps (reverse cs)

-- Obtain a unique variable name; it's unique in the AP computation
--
genSym :: AP String
genSym 
  = do
      n <- gets sym
      modify $ \s -> s {sym = succ (sym s)}
      return $ "a" ++ show n

-- Obtain a unique array identifier at a given element type; it's unique in
-- the AP computation 
--
genArr :: (Ix dim, Elem e) => AP (Arr dim e)
genArr
  = do
      name <- genSym
      return $ Arr name

-- Add a collective operation to the list in the monad state
--
pushComp :: CompBinding -> AP ()
pushComp comp = modify $ \s -> s {comps = Comps $ comp : unComps s}

wrapComp :: (Ix dim, Elem e)
         => Comp (AST.Arr (ElemRepr dim) (ElemRepr e)) -> AP (Arr dim e)
wrapComp comp
  = do
      arr <- genArr
      pushComp $ convertArr arr `CompBinding` comp
      return arr

wrapComp2 :: (Ix dim1, Ix dim2, Elem e1, Elem e2) 
          => Comp (AST.Arr (ElemRepr dim1) (ElemRepr e1), 
                   AST.Arr (ElemRepr dim2) (ElemRepr e2))
          -> AP (Arr dim1 e1, Arr dim2 e2)
wrapComp2 comp
  = do
      arr1 <- genArr
      arr2 <- genArr
      pushComp $ (convertArr arr1, convertArr arr2) `CompBinding` comp
      return (arr1, arr2)


-- |Smart constructors to construct representation AST forms
-- ---------------------------------------------------------

mkIndex :: forall slix e. (SliceIx slix, Elem e) 
        => slix {- dummy to fix the type variable -}
        -> e    {- dummy to fix the type variable -}
        -> AST.Arr (ElemRepr (SliceDim slix)) (ElemRepr e) 
        -> AST.Exp (ElemRepr slix)
        -> Comp (AST.Arr (ElemRepr (Slice slix)) (ElemRepr e))
mkIndex slix _ arr e 
  = Index (convertSliceIndex slix (sliceIndex (undefined::slix))) arr e

mkReplicate :: forall slix e. (SliceIx slix, Elem e) 
        => slix {- dummy to fix the type variable -}
        -> e    {- dummy to fix the type variable -}
        -> AST.Exp (ElemRepr slix)
        -> AST.Arr (ElemRepr (Slice slix)) (ElemRepr e) 
        -> Comp (AST.Arr (ElemRepr (SliceDim slix)) (ElemRepr e))
mkReplicate slix _ e arr 
  = Replicate (convertSliceIndex slix (sliceIndex (undefined::slix))) e arr

{-
mkZip :: (Ix dim, Elem a, Elem b) 
      => dim {- dummy to fix the type variable -}
      -> a   {- dummy to fix the type variable -}
      -> b   {- dummy to fix the type variable -}
      -> AST.Arr (ElemRepr dim) (ElemRepr a) 
      -> AST.Arr (ElemRepr dim) (ElemRepr' b)
      -> Comp (AST.Arr (ElemRepr dim) (ElemRepr (a, b)))
mkZip _ _ _ arr1 arr2 = Zip arr1 arr2
 -}


-- |Smart constructors to construct HOAS AST expressions
-- -----------------------------------------------------

-- |Smart constructor for literals
-- -

exp :: Elem t => t -> Exp t
exp v = Const v

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
