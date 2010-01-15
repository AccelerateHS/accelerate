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
--  This modules defines the AST of the user-visible embedded language using
--  more convenient higher-order abstract syntax (instead of de Bruijn
--  indices). Moreover, it defines smart constructors to construct programs.

module Data.Array.Accelerate.Smart (

  -- * HOAS AST
  Acc(..), Exp(..), 
  
  -- * HOAS -> de Bruijn conversion
  convertAcc, convertClosedExp,
  
  -- * Smart constructors for unpairing
  unpair,

  -- * Smart constructors for literals
  constant,
  
  -- * Smart constructors and destructors for tuples
  tup2, tup3, tup4, tup5, untup2, untup3, untup4, untup5,

  -- * Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi, 
  mkSin, mkCos, mkTan,
  mkAsin, mkAcos, mkAtan,
  mkAsinh, mkAcosh, mkAtanh,
  mkExpFloating, mkSqrt, mkLog,
  mkFPow, mkLogBase,

  -- * Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkIDiv, mkMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq,
  mkEq, mkNEq, mkMax, mkMin, mkLAnd, mkLOr, mkLNot, mkBoolToInt

) where

-- standard library
import Data.Maybe
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Tuple hiding    (Tuple)
import qualified Data.Array.Accelerate.Tuple as Tuple
import Data.Array.Accelerate.AST hiding (OpenAcc(..), Acc, OpenExp(..), Exp)
import qualified Data.Array.Accelerate.AST                  as AST
import Data.Array.Accelerate.Pretty ()


-- Monadic array computations
-- --------------------------

-- |Array-valued collective computations
--
data Acc a where
  
  FstArray    :: (Elem e1, Elem e2)
              => Acc (Array dim1 e1, Array dim2 e2)
              -> Acc (Array dim1 e1)
  SndArray    :: (Elem e1, Elem e2)
              => Acc (Array dim1 e1, Array dim2 e2)
              -> Acc (Array dim2 e2)

  Use         :: Array dim e -> Acc (Array dim e)
  Unit        :: Elem e
              => Exp e 
              -> Acc (Scalar e)
  Reshape     :: Ix dim
              => Exp dim
              -> Acc (Array dim' e)
              -> Acc (Array dim e)
  Replicate   :: (SliceIx slix, Elem e)
              => Exp slix
              -> Acc (Array (Slice slix)    e)
              -> Acc (Array (SliceDim slix) e)
  Index       :: (SliceIx slix, Elem e)
              => Acc (Array (SliceDim slix) e)
              -> Exp slix
              -> Acc (Array (Slice slix) e)
  Map         :: (Elem e, Elem e')
              => (Exp e -> Exp e') 
              -> Acc (Array dim e)
              -> Acc (Array dim e')
  ZipWith     :: (Elem e1, Elem e2, Elem e3)
              => (Exp e1 -> Exp e2 -> Exp e3) 
              -> Acc (Array dim e1)
              -> Acc (Array dim e2)
              -> Acc (Array dim e3)
  Fold        :: Elem e
              => (Exp e -> Exp e -> Exp e)
              -> Exp e
              -> Acc (Array dim e)
              -> Acc (Scalar e)
  FoldSeg     :: Elem e
              => (Exp e -> Exp e -> Exp e)
              -> Exp e
              -> Acc (Vector e)
              -> Acc Segments
              -> Acc (Vector e)
  Scan        :: Elem e
              => (Exp e -> Exp e -> Exp e)
              -> Exp e
              -> Acc (Vector e)
              -> Acc (Vector e, Scalar e)
  Permute     :: (Ix dim, Ix dim', Elem e)
              => (Exp e -> Exp e -> Exp e)
              -> Acc (Array dim' e)
              -> (Exp dim -> Exp dim')
              -> Acc (Array dim e)
              -> Acc (Array dim' e)
  Backpermute :: (Ix dim, Ix dim', Elem e)
              => Exp dim'
              -> (Exp dim' -> Exp dim)
              -> Acc (Array dim e)
              -> Acc (Array dim' e)


-- |Conversion from HOAS to de Bruijn computation AST
-- -

-- |Convert an array expression with given array environment layout
--
convertOpenAcc :: Layout aenv aenv 
               -> Acc a 
               -> AST.OpenAcc aenv a
convertOpenAcc alyt (FstArray acc)
  = AST.Let2 (convertOpenAcc alyt acc) (AST.Avar (AST.SuccIdx AST.ZeroIdx))
convertOpenAcc alyt (SndArray acc)
  = AST.Let2 (convertOpenAcc alyt acc) (AST.Avar AST.ZeroIdx)
convertOpenAcc _    (Use array)     = AST.Use array
convertOpenAcc alyt (Unit e)        = AST.Unit (convertExp alyt e)
convertOpenAcc alyt (Reshape e acc) 
  = AST.Reshape (convertExp alyt e) (convertOpenAcc alyt acc)
convertOpenAcc alyt (Replicate ix acc)
  = mkReplicate (convertExp alyt ix) (convertOpenAcc alyt acc)
convertOpenAcc alyt (Index acc ix)
  = mkIndex (convertOpenAcc alyt acc) (convertExp alyt ix)
convertOpenAcc alyt (Map f acc) 
  = AST.Map (convertFun1 alyt f) (convertOpenAcc alyt acc)
convertOpenAcc alyt (ZipWith f acc1 acc2) 
  = AST.ZipWith (convertFun2 alyt f) 
                (convertOpenAcc alyt acc1)
                (convertOpenAcc alyt acc2)
convertOpenAcc alyt (Fold f e acc) 
  = AST.Fold (convertFun2 alyt f) (convertExp alyt e) (convertOpenAcc alyt acc)
convertOpenAcc alyt (FoldSeg f e acc1 acc2) 
  = AST.FoldSeg (convertFun2 alyt f) (convertExp alyt e) 
                (convertOpenAcc alyt acc1) (convertOpenAcc alyt acc2)
convertOpenAcc alyt (Scan f e acc) 
  = AST.Scan (convertFun2 alyt f) (convertExp alyt e) (convertOpenAcc alyt acc)
convertOpenAcc alyt (Permute f dftAcc perm acc) 
  = AST.Permute (convertFun2 alyt f) 
                (convertOpenAcc alyt dftAcc)
                (convertFun1 alyt perm) 
                (convertOpenAcc alyt acc)
convertOpenAcc alyt (Backpermute newDim perm acc) 
  = AST.Backpermute (convertExp alyt newDim)
                    (convertFun1 alyt perm)
                    (convertOpenAcc alyt acc)

-- |Convert a closed array expression
--
convertAcc :: Acc a -> AST.Acc a
convertAcc = convertOpenAcc EmptyLayout


-- Embedded expressions of the surface language
-- --------------------------------------------

-- HOAS expressions mirror the constructors of `AST.OpenExp', but with the
-- `Tag' constructor instead of variables in the form of de Bruijn indices.
-- Moreover, HOAS expression use n-tuples and the type class 'Elem' to
-- constrain element types, whereas `AST.OpenExp' uses nested pairs and the 
-- GADT 'TupleType'.
--

-- |Scalar expressions used to parametrise collective array operations
--
data Exp t where
    -- Needed for conversion to de Bruijn form
  Tag         :: Elem t
              => Int                          -> Exp t
                 -- environment size at defining occurrence

    -- All the same constructors as 'AST.Exp'
  Const       :: Elem t 
              => t                             -> Exp t

  Tuple       :: (Elem t, IsTuple t)
              => Tuple.Tuple Exp (TupleRepr t) -> Exp t
  Prj         :: (Elem t, IsTuple t)
              => TupleIdx (TupleRepr t) e     
              -> Exp t                         -> Exp e              
  Cond        :: Exp Bool -> Exp t -> Exp t    -> Exp t
  PrimConst   :: Elem t                       
              => PrimConst t                   -> Exp t
  PrimApp     :: (Elem a, Elem r)             
              => PrimFun (a -> r) -> Exp a     -> Exp r
  IndexScalar :: Acc (Array dim t) -> Exp dim  -> Exp t
  Shape       :: Elem dim
              => Acc (Array dim e)             -> Exp dim


-- |Conversion from HOAS to de Bruijn expression AST
-- -

-- A layout of an environment an entry for each entry of the environment.
-- Each entry in the layout holds the deBruijn index that refers to the
-- corresponding entry in the environment.
--
data Layout env env' where
  EmptyLayout :: Layout env ()
  PushLayout  :: Typeable t 
              => Layout env env' -> Idx env t -> Layout env (env', t)

-- Project the nth index out of an environment layout.
--
prjIdx :: Typeable t => Int -> Layout env env' -> Idx env t
prjIdx 0 (PushLayout _ ix) = fromJust (gcast ix)
                               -- can't go wrong unless the library is wrong!
prjIdx n (PushLayout l _)  = prjIdx (n - 1) l
prjIdx _ EmptyLayout       
  = error "Data.Array.Accelerate.Smart.prjIdx: internal error"

-- |Convert an open expression with given environment layouts.
--
convertOpenExp :: forall t env aenv. 
                  Layout env  env       -- scalar environment
               -> Layout aenv aenv      -- array environment
               -> Exp t                 -- expression to be converted
               -> AST.OpenExp env aenv t
convertOpenExp lyt alyt = cvt
  where
    cvt :: Exp t' -> AST.OpenExp env aenv t'
    cvt (Tag i)             = AST.Var (prjIdx i lyt)
    cvt (Const v)           = AST.Const (fromElem v)
    cvt (Tuple tup)         = AST.Tuple (convertTuple lyt alyt tup)
    cvt (Prj idx e)         = AST.Prj idx (cvt e)
    cvt (Cond e1 e2 e3)     = AST.Cond (cvt e1) (cvt e2) (cvt e3)
    cvt (PrimConst c)       = AST.PrimConst c
    cvt (PrimApp p e)       = AST.PrimApp p (cvt e)
    cvt (IndexScalar a e)   = AST.IndexScalar (convertOpenAcc alyt a) (cvt e)
    cvt (Shape a)           = AST.Shape (convertOpenAcc alyt a)

-- |Convert a tuple expression
--
convertTuple :: Layout env env 
             -> Layout aenv aenv 
             -> Tuple.Tuple Exp t 
             -> Tuple.Tuple (AST.OpenExp env aenv) t
convertTuple _lyt _alyt NilTup           = NilTup
convertTuple lyt  alyt  (es `SnocTup` e) 
  = convertTuple lyt alyt es `SnocTup` convertOpenExp lyt alyt e

-- |Convert an expression closed wrt to scalar variables
--
convertExp :: Layout aenv aenv      -- array environment
           -> Exp t                 -- expression to be converted
           -> AST.Exp aenv t
convertExp alyt = convertOpenExp EmptyLayout alyt

-- |Convert a closed expression
--
convertClosedExp :: Exp t -> AST.Exp () t
convertClosedExp = convertExp EmptyLayout

-- |Convert a unary functions
--
convertFun1 :: forall a b aenv. Elem a
            => Layout aenv aenv 
            -> (Exp a -> Exp b) 
            -> AST.Fun aenv (a -> b)
convertFun1 alyt f = Lam (Body openF)
  where
    a     = Tag 0
    lyt   = EmptyLayout 
            `PushLayout` 
            (ZeroIdx :: Idx ((), ElemRepr a) (ElemRepr a))
    openF = convertOpenExp lyt alyt (f a)

-- |Convert a binary functions
--
convertFun2 :: forall a b c aenv. (Elem a, Elem b) 
            => Layout aenv aenv 
            -> (Exp a -> Exp b -> Exp c) 
            -> AST.Fun aenv (a -> b -> c)
convertFun2 alyt f = Lam (Lam (Body openF))
  where
    a     = Tag 1
    b     = Tag 0
    lyt   = EmptyLayout 
            `PushLayout`
            (SuccIdx ZeroIdx :: Idx (((), ElemRepr a), ElemRepr b) (ElemRepr a))
            `PushLayout`
            (ZeroIdx         :: Idx (((), ElemRepr a), ElemRepr b) (ElemRepr b))
    openF = convertOpenExp lyt alyt (f a b)


-- Pretty printing
--

instance Show (Acc as) where
  show = show . convertAcc
  
instance Show (Exp a) where
  show = show . convertClosedExp


-- |Smart constructors to construct representation AST forms
-- ---------------------------------------------------------

mkIndex :: forall slix e aenv. (SliceIx slix, Elem e) 
        => AST.OpenAcc aenv (Array (SliceDim slix) e)
        -> AST.Exp     aenv slix
        -> AST.OpenAcc aenv (Array (Slice slix) e)
mkIndex arr e 
  = AST.Index (convertSliceIndex slix (sliceIndex slix)) arr e
  where
    slix = undefined :: slix

mkReplicate :: forall slix e aenv. (SliceIx slix, Elem e) 
        => AST.Exp     aenv slix
        -> AST.OpenAcc aenv (Array (Slice slix) e)
        -> AST.OpenAcc aenv (Array (SliceDim slix) e)
mkReplicate e arr
  = AST.Replicate (convertSliceIndex slix (sliceIndex slix)) e arr
  where
    slix = undefined :: slix


-- |Smart constructors to construct HOAS AST expressions
-- -----------------------------------------------------

-- Pushes the 'Acc' constructor through a pair
--
unpair :: (Ix dim1, Ix dim2, Elem e1, Elem e2)
       => Acc (Array dim1 e1, Array dim2 e2) 
       -> (Acc (Array dim1 e1), Acc (Array dim2 e2))
unpair acc = (FstArray acc, SndArray acc)


-- Smart constructor for literals
-- 

-- |Constant scalar expression
--
constant :: Elem t => t -> Exp t
constant = Const

-- Smart constructor and destructors for tuples
--

tup2 :: (Elem a, Elem b) => (Exp a, Exp b) -> Exp (a, b)
tup2 (x1, x2) = Tuple (NilTup `SnocTup` x1 `SnocTup` x2)

tup3 :: (Elem a, Elem b, Elem c) => (Exp a, Exp b, Exp c) -> Exp (a, b, c)
tup3 (x1, x2, x3) = Tuple (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3)

tup4 :: (Elem a, Elem b, Elem c, Elem d) 
     => (Exp a, Exp b, Exp c, Exp d) -> Exp (a, b, c, d)
tup4 (x1, x2, x3, x4) 
  = Tuple (NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4)

tup5 :: (Elem a, Elem b, Elem c, Elem d, Elem e) 
     => (Exp a, Exp b, Exp c, Exp d, Exp e) -> Exp (a, b, c, d, e)
tup5 (x1, x2, x3, x4, x5)
  = Tuple $
      NilTup `SnocTup` x1 `SnocTup` x2 `SnocTup` x3 `SnocTup` x4 `SnocTup` x5

untup2 :: (Elem a, Elem b) => Exp (a, b) -> (Exp a, Exp b)
untup2 e = ((SuccTupIdx ZeroTupIdx) `Prj` e, ZeroTupIdx `Prj` e)

untup3 :: (Elem a, Elem b, Elem c) => Exp (a, b, c) -> (Exp a, Exp b, Exp c)
untup3 e = (SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e, 
            SuccTupIdx ZeroTupIdx `Prj` e, 
            ZeroTupIdx `Prj` e)

untup4 :: (Elem a, Elem b, Elem c, Elem d) 
       => Exp (a, b, c, d) -> (Exp a, Exp b, Exp c, Exp d)
untup4 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e, 
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e, 
            SuccTupIdx ZeroTupIdx `Prj` e, 
            ZeroTupIdx `Prj` e)

untup5 :: (Elem a, Elem b, Elem c, Elem d, Elem e) 
       => Exp (a, b, c, d, e) -> (Exp a, Exp b, Exp c, Exp d, Exp e)
untup5 e = (SuccTupIdx (SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx))) 
            `Prj` e, 
            SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` e, 
            SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` e, 
            SuccTupIdx ZeroTupIdx `Prj` e, 
            ZeroTupIdx `Prj` e)

-- Smart constructor for constants
-- 

mkMinBound :: (Elem t, IsBounded t) => Exp t
mkMinBound = PrimConst (PrimMinBound boundedType)

mkMaxBound :: (Elem t, IsBounded t) => Exp t
mkMaxBound = PrimConst (PrimMaxBound boundedType)

mkPi :: (Elem r, IsFloating r) => Exp r
mkPi = PrimConst (PrimPi floatingType)

mkSin :: (Elem t, IsFloating t) => Exp t -> Exp t
mkSin x = PrimSin floatingType `PrimApp` x

mkCos :: (Elem t, IsFloating t) => Exp t -> Exp t
mkCos x = PrimCos floatingType `PrimApp` x

mkTan :: (Elem t, IsFloating t) => Exp t -> Exp t
mkTan x = PrimTan floatingType `PrimApp` x

mkAsin :: (Elem t, IsFloating t) => Exp t -> Exp t
mkAsin x = PrimAsin floatingType `PrimApp` x

mkAcos :: (Elem t, IsFloating t) => Exp t -> Exp t
mkAcos x = PrimAcos floatingType `PrimApp` x

mkAtan :: (Elem t, IsFloating t) => Exp t -> Exp t
mkAtan x = PrimAtan floatingType `PrimApp` x

mkAsinh :: (Elem t, IsFloating t) => Exp t -> Exp t
mkAsinh x = PrimAsinh floatingType `PrimApp` x

mkAcosh :: (Elem t, IsFloating t) => Exp t -> Exp t
mkAcosh x = PrimAcosh floatingType `PrimApp` x

mkAtanh :: (Elem t, IsFloating t) => Exp t -> Exp t
mkAtanh x = PrimAtanh floatingType `PrimApp` x

mkExpFloating :: (Elem t, IsFloating t) => Exp t -> Exp t
mkExpFloating x = PrimExpFloating floatingType `PrimApp` x

mkSqrt :: (Elem t, IsFloating t) => Exp t -> Exp t
mkSqrt x = PrimSqrt floatingType `PrimApp` x

mkLog :: (Elem t, IsFloating t) => Exp t -> Exp t
mkLog x = PrimLog floatingType `PrimApp` x

mkFPow :: (Elem t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFPow x y = PrimFPow floatingType `PrimApp` tup2 (x, y)

mkLogBase :: (Elem t, IsFloating t) => Exp t -> Exp t -> Exp t
mkLogBase x y = PrimLogBase floatingType `PrimApp` tup2 (x, y)

-- Smart constructors for primitive applications
-- 

-- Operators from Num

mkAdd :: (Elem t, IsNum t) => Exp t -> Exp t -> Exp t
mkAdd x y = PrimAdd numType `PrimApp` tup2 (x, y)

mkSub :: (Elem t, IsNum t) => Exp t -> Exp t -> Exp t
mkSub x y = PrimSub numType `PrimApp` tup2 (x, y)

mkMul :: (Elem t, IsNum t) => Exp t -> Exp t -> Exp t
mkMul x y = PrimMul numType `PrimApp` tup2 (x, y)

mkNeg :: (Elem t, IsNum t) => Exp t -> Exp t
mkNeg x = PrimNeg numType `PrimApp` x

mkAbs :: (Elem t, IsNum t) => Exp t -> Exp t
mkAbs x = PrimAbs numType `PrimApp` x

mkSig :: (Elem t, IsNum t) => Exp t -> Exp t
mkSig x = PrimSig numType `PrimApp` x

-- Operators from Integral & Bits

mkQuot :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkQuot x y = PrimQuot integralType `PrimApp` tup2 (x, y)

mkRem :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkRem x y = PrimRem integralType `PrimApp` tup2 (x, y)

mkIDiv :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkIDiv x y = PrimIDiv integralType `PrimApp` tup2 (x, y)

mkMod :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkMod x y = PrimMod integralType `PrimApp` tup2 (x, y)

mkBAnd :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBAnd x y = PrimBAnd integralType `PrimApp` tup2 (x, y)

mkBOr :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBOr x y = PrimBOr integralType `PrimApp` tup2 (x, y)

mkBXor :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBXor x y = PrimBXor integralType `PrimApp` tup2 (x, y)

mkBNot :: (Elem t, IsIntegral t) => Exp t -> Exp t
mkBNot x = PrimBNot integralType `PrimApp` x
  -- FIXME: add shifts

-- Operators from Fractional, Floating, RealFrac & RealFloat

mkFDiv :: (Elem t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFDiv x y = PrimFDiv floatingType `PrimApp` tup2 (x, y)

mkRecip :: (Elem t, IsFloating t) => Exp t -> Exp t
mkRecip x = PrimRecip floatingType `PrimApp` x

  -- FIXME: add operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLt x y = PrimLt scalarType `PrimApp` tup2 (x, y)

mkGt :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGt x y = PrimGt scalarType `PrimApp` tup2 (x, y)

mkLtEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLtEq x y = PrimLtEq scalarType `PrimApp` tup2 (x, y)

mkGtEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGtEq x y = PrimGtEq scalarType `PrimApp` tup2 (x, y)

mkEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkEq x y = PrimEq scalarType `PrimApp` tup2 (x, y)

mkNEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkNEq x y = PrimLt scalarType `PrimApp` tup2 (x, y)

mkMax :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMax x y = PrimMax scalarType `PrimApp` tup2 (x, y)

mkMin :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMin x y = PrimMin scalarType `PrimApp` tup2 (x, y)

-- Logical operators

mkLAnd :: Exp Bool -> Exp Bool -> Exp Bool
mkLAnd x y = PrimLAnd `PrimApp` tup2 (x, y)

mkLOr :: Exp Bool -> Exp Bool -> Exp Bool
mkLOr x y = PrimLOr `PrimApp` tup2 (x, y)

mkLNot :: Exp Bool -> Exp Bool
mkLNot x = PrimLNot `PrimApp` x

-- FIXME: Character conversions

-- FIXME: Numeric conversions

-- FIXME: Other conversions

mkBoolToInt :: Exp Bool -> Exp Int
mkBoolToInt b = PrimBoolToInt `PrimApp` b

