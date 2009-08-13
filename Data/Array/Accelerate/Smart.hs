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

  -- * Smart constructors for literals
  constant,

  -- * Smart constructors for constants
  mkMinBound, mkMaxBound, mkPi,

  -- * Smart constructors for primitive functions
  mkAdd, mkSub, mkMul, mkNeg, mkAbs, mkSig, mkQuot, mkRem, mkIDiv, mkMod,
  mkBAnd, mkBOr, mkBXor, mkBNot, mkFDiv, mkRecip, mkLt, mkGt, mkLtEq, mkGtEq,
  mkEq, mkNEq, mkMax, mkMin, mkLAnd, mkLOr, mkLNot,

) where

-- standard library
import Data.Maybe
import Data.Typeable

-- friends
import Data.Array.Accelerate.Type
{-
import Data.Array.Accelerate.Array.Representation hiding (
  Array(..), Scalar, Vector)
-}
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST hiding (OpenAcc(..), Acc, OpenExp(..), Exp)
import qualified Data.Array.Accelerate.AST                  as AST
import qualified Data.Array.Accelerate.Array.Representation as AST
import Data.Array.Accelerate.Pretty


-- Monadic array computations
-- --------------------------

data Acc a where

  Use         :: Array dim e -> Acc (Array dim e)
  Unit        :: Elem e
              => Exp e 
              -> Acc (Scalar e)
  Reshape     :: Ix dim
              => Exp dim
              -> Acc (Array dim' e)
              -> Acc (Array dim e)
  Replicate   :: (SliceIx slix, Elem e)
              => slix {- dummy to fix the type variable -}
              -> e    {- dummy to fix the type variable -}
              -> Exp slix
              -> Acc (Array (Slice slix)    e)
              -> Acc (Array (SliceDim slix) e)
  Index       :: (SliceIx slix, Elem e)
              => slix {- dummy to fix the type variable -}
              -> e    {- dummy to fix the type variable -}
              -> Acc (Array (SliceDim slix) e)
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
  Filter      :: Elem e
              => (Exp e -> Exp Bool) 
              -> Acc (Vector e)
              -> Acc (Vector e)
  Fold        :: Elem e
              => (Exp e -> Exp e -> Exp e)
              -> Exp e
              -> Acc (Array dim e)
              -> Acc (Scalar e)
  Scan        :: Elem e
              => (Exp e -> Exp e -> Exp e)
              -> Exp e
              -> Acc (Vector e)
              -> Acc (Scalar e, Vector e)
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
               -> AST.OpenAcc aenv (ArraysRepr a)
convertOpenAcc alyt (Use array)     = AST.Use (fromArray array)
convertOpenAcc alyt (Unit e)        = AST.Unit (convertExp alyt e)
convertOpenAcc alyt (Reshape e acc) 
  = AST.Reshape (convertExp alyt e) (convertOpenAcc alyt acc)
convertOpenAcc alyt (Replicate slixType eType ix acc)
  = mkReplicate slixType eType 
                (convertExp alyt ix) (convertOpenAcc alyt acc)
convertOpenAcc alyt (Index slixType eType acc ix)
  = mkIndex slixType eType (convertOpenAcc alyt acc) (convertExp alyt ix)
convertOpenAcc alyt (Map f acc) 
  = AST.Map (convertFun1 alyt f) (convertOpenAcc alyt acc)
convertOpenAcc alyt (ZipWith f acc1 acc2) 
  = AST.ZipWith (convertFun2 alyt f) 
                (convertOpenAcc alyt acc1)
                (convertOpenAcc alyt acc2)
convertOpenAcc alyt (Filter p acc) 
  = AST.Filter (convertFun1 alyt p) (convertOpenAcc alyt acc)
convertOpenAcc alyt (Fold f e acc) 
  = AST.Fold (convertFun2 alyt f) (convertExp alyt e) (convertOpenAcc alyt acc)
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
convertAcc :: Acc a -> AST.Acc (ArraysRepr a)
convertAcc = convertOpenAcc EmptyLayout


-- Embedded expressions of the surface language
-- --------------------------------------------

-- HOAS expressions mirror the constructors of `AST.OpenExp', but with the
-- `Tag' constructor instead of variables in the form of de Bruijn indices.
-- Moreover, HOAS expression use n-tuples and the type class 'Elem' to
-- constrain element types, whereas `AST.OpenExp' uses nested pairs and the 
-- GADT 'TupleType'.
--
data Exp t where
    -- Needed for conversion to de Bruijn form
  Tag         :: Elem t
              => Int                        -> Exp t
                 -- environment size at defining occurrence

    -- All the same constructors as 'AST.Exp'
  Const       :: Elem t 
              => t                            -> Exp t
  Pair        :: (Elem s, Elem t)             
              => Exp s -> Exp t               -> Exp (s, t)
  Fst         :: (Elem s, Elem t)             
              => Exp (s, t)                   -> Exp s
  Snd         :: (Elem s, Elem t)             
              => Exp (s, t)                   -> Exp t
  Cond        :: Exp Bool -> Exp t -> Exp t   -> Exp t
  PrimConst   :: Elem t                       
              => PrimConst t                  -> Exp t
  PrimApp     :: (Elem a, Elem r)             
              => PrimFun (a -> r) -> Exp a    -> Exp r
  IndexScalar :: Acc (Array dim t) -> Exp dim -> Exp t
  Shape       :: Acc (Array dim e)            -> Exp dim


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

-- Project the nth index out of an environment layout
--
prjIdx :: Typeable t => Int -> Layout env env' -> Idx env t
prjIdx 0 (PushLayout _ ix) = fromJust (gcast ix)
                               -- can't go wrong unless the library is wrong!
prjIdx n (PushLayout l _)  = prjIdx (n - 1) l
prjIdx _ EmptyLayout       
  = error "Data.Array.Accelerate.Smart.prjIdx: internal error"

-- |Convert an open expression with given environment layouts
--
convertOpenExp :: forall t env aenv. 
                  Layout env  env       -- scalar environment
               -> Layout aenv aenv      -- array environment
               -> Exp t                 -- expression to be converted
               -> AST.OpenExp env aenv (ElemRepr t)
convertOpenExp lyt alyt = cvt
  where
    cvt :: forall t'. Exp t' -> AST.OpenExp env aenv (ElemRepr t')
    cvt (Tag i)             = AST.Var (prjIdx i lyt)
    cvt (Const v)           = AST.Const (fromElem v)
    cvt (Pair (e1::Exp t1) 
              (e2::Exp t2)) = AST.Pair (undefined::t1)
                                       (undefined::t2)
                                       (cvt e1) (cvt e2)
    cvt (Fst (e::Exp (t', t2)))             
                            = AST.Fst (undefined::t') (undefined::t2) (cvt e)
    cvt (Snd (e::Exp (t1, t')))             
                            = AST.Snd (undefined::t1) (undefined::t') (cvt e)
    cvt (Cond e1 e2 e3)     = AST.Cond (cvt e1) (cvt e2) (cvt e3)
    cvt (PrimConst c)       = AST.PrimConst c
    cvt (PrimApp p e)       = AST.PrimApp p (cvt e)
    cvt (IndexScalar a e)   = AST.IndexScalar (convertOpenAcc alyt a) (cvt e)
    cvt (Shape a)           = AST.Shape (convertOpenAcc alyt a)
  
-- |Convert an expression closed wrt to scalar variables
--
convertExp :: Layout aenv aenv      -- array environment
           -> Exp t                 -- expression to be converted
           -> AST.Exp aenv (ElemRepr t)
convertExp alyt = convertOpenExp EmptyLayout alyt

-- |Convert a closed expression
--
convertClosedExp :: Exp t -> AST.Exp () (ElemRepr t)
convertClosedExp = convertExp EmptyLayout

-- |Convert a unary functions
--
convertFun1 :: forall a b aenv. Elem a
            => Layout aenv aenv 
            -> (Exp a -> Exp b) 
            -> AST.Fun aenv (ElemRepr a -> ElemRepr b)
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
            -> AST.Fun aenv (ElemRepr a -> ElemRepr b -> ElemRepr c)
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

instance Show (Exp t) where
  show e 
    = show (convertExp EmptyLayout e :: AST.Exp () (ElemRepr t))


-- |Smart constructors to construct representation AST forms
-- ---------------------------------------------------------

mkIndex :: forall slix e env aenv. (SliceIx slix, Elem e) 
        => slix {- dummy to fix the type variable -}
        -> e    {- dummy to fix the type variable -}
        -> AST.OpenAcc aenv (ArraysRepr (Array (SliceDim slix) e))
        -> AST.Exp     aenv (ElemRepr slix)
        -> AST.OpenAcc aenv (ArraysRepr (Array (Slice slix) e))
mkIndex slix _ arr e 
  = AST.Index (convertSliceIndex slix (sliceIndex slix)) arr e

mkReplicate :: forall slix e env aenv. (SliceIx slix, Elem e) 
        => slix {- dummy to fix the type variable -}
        -> e    {- dummy to fix the type variable -}
        -> AST.Exp     aenv (ElemRepr slix)
        -> AST.OpenAcc aenv (ArraysRepr (Array (Slice slix) e))
        -> AST.OpenAcc aenv (ArraysRepr (Array (SliceDim slix) e))
mkReplicate slix _ e arr
  = AST.Replicate (convertSliceIndex slix (sliceIndex slix)) e arr


-- |Smart constructors to construct HOAS AST expressions
-- -----------------------------------------------------

-- |Smart constructor for literals
-- -

constant :: Elem t => t -> Exp t
constant = Const

-- |Smart constructor for constants
-- -

mkMinBound :: (Elem t, IsBounded t) => Exp t
mkMinBound = PrimConst (PrimMinBound boundedType)

mkMaxBound :: (Elem t, IsBounded t) => Exp t
mkMaxBound = PrimConst (PrimMaxBound boundedType)

mkPi :: (Elem r, IsFloating r) => Exp r
mkPi = PrimConst (PrimPi floatingType)

-- |Smart constructors for primitive applications
-- -

-- Operators from Num

mkAdd :: (Elem t, IsNum t) => Exp t -> Exp t -> Exp t
mkAdd x y = PrimAdd numType `PrimApp` (x `Pair` y)

mkSub :: (Elem t, IsNum t) => Exp t -> Exp t -> Exp t
mkSub x y = PrimSub numType `PrimApp` (x `Pair` y)

mkMul :: (Elem t, IsNum t) => Exp t -> Exp t -> Exp t
mkMul x y = PrimMul numType `PrimApp` (x `Pair` y)

mkNeg :: (Elem t, IsNum t) => Exp t -> Exp t
mkNeg x = PrimNeg numType `PrimApp` x

mkAbs :: (Elem t, IsNum t) => Exp t -> Exp t
mkAbs x = PrimAbs numType `PrimApp` x

mkSig :: (Elem t, IsNum t) => Exp t -> Exp t
mkSig x = PrimSig numType `PrimApp` x

-- Operators from Integral & Bits

mkQuot :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkQuot x y = PrimQuot integralType `PrimApp` (x `Pair` y)

mkRem :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkRem x y = PrimRem integralType `PrimApp` (x `Pair` y)

mkIDiv :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkIDiv x y = PrimIDiv integralType `PrimApp` (x `Pair` y)

mkMod :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkMod x y = PrimMod integralType `PrimApp` (x `Pair` y)

mkBAnd :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBAnd x y = PrimBAnd integralType `PrimApp` (x `Pair` y)

mkBOr :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBOr x y = PrimBOr integralType `PrimApp` (x `Pair` y)

mkBXor :: (Elem t, IsIntegral t) => Exp t -> Exp t -> Exp t
mkBXor x y = PrimBXor integralType `PrimApp` (x `Pair` y)

mkBNot :: (Elem t, IsIntegral t) => Exp t -> Exp t
mkBNot x = PrimBNot integralType `PrimApp` x
  -- FIXME: add shifts

-- Operators from Fractional, Floating, RealFrac & RealFloat

mkFDiv :: (Elem t, IsFloating t) => Exp t -> Exp t -> Exp t
mkFDiv x y = PrimFDiv floatingType `PrimApp` (x `Pair` y)

mkRecip :: (Elem t, IsFloating t) => Exp t -> Exp t
mkRecip x = PrimRecip floatingType `PrimApp` x
  -- FIXME: add operations from Floating, RealFrac & RealFloat

-- Relational and equality operators

mkLt :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLt x y = PrimLt scalarType `PrimApp` (x `Pair` y)

mkGt :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGt x y = PrimGt scalarType `PrimApp` (x `Pair` y)

mkLtEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkLtEq x y = PrimLtEq scalarType `PrimApp` (x `Pair` y)

mkGtEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkGtEq x y = PrimGtEq scalarType `PrimApp` (x `Pair` y)

mkEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkEq x y = PrimEq scalarType `PrimApp` (x `Pair` y)

mkNEq :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp Bool
mkNEq x y = PrimLt scalarType `PrimApp` (x `Pair` y)

mkMax :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
mkMax x y = PrimMax scalarType `PrimApp` (x `Pair` y)

mkMin :: (Elem t, IsScalar t) => Exp t -> Exp t -> Exp t
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
