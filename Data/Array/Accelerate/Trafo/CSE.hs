{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
-- |
-- Module      : Data.Array.Accelerate.Trafo.CSE
-- Copyright   : [2012] Manuel M T Chakravarty, Gabriele Keller, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Trafo.CSE (

  -- Scalar expressions
  simplifyExp,
  simplifyFun,

) where

-- standard library
import Prelude                                          hiding ( exp )
import Data.Typeable
import Data.Hashable

-- friends
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Trafo.Substitution
import Data.Array.Accelerate.Array.Sugar                ( Elt )
import Data.Array.Accelerate.Tuple                      hiding ( Tuple )
import qualified Data.Array.Accelerate.Tuple            as Tuple
import qualified Data.Array.Accelerate.Array.Sugar      as Sugar


-- An environment that holds let-bound scalar expressions. The second
-- environment variable env' is used to project out the corresponding
-- index when looking up in the environment congruent expressions.
--
data Gamma env env' aenv where
  EmptyEnv :: Gamma env () aenv

  PushEnv  :: Typeable t
           => Gamma   env env'      aenv
           -> OpenExp env           aenv t
           -> Gamma   env (env', t) aenv

incEnv :: Gamma env env' aenv -> Gamma (env, s) env' aenv
incEnv EmptyEnv        = EmptyEnv
incEnv (PushEnv env e) = incEnv env `PushEnv` weakenE e

lookupEnv :: Typeable t
          => Gamma   env env' aenv
          -> OpenExp env      aenv t
          -> Maybe  (Idx env' t)
lookupEnv EmptyEnv        _             = Nothing
lookupEnv (PushEnv env e) x
  | Just REFL <- matchOpenExp e x       = Just ZeroIdx
  | otherwise                           = SuccIdx `fmap` lookupEnv env x


-- Simplify scalar expressions. Currently this takes the form of a pretty weedy
-- CSE optimisation, where we look for expressions of the form:
--
-- > let x = e1 in e2
--
-- and replace all occurrences of e1 in e2 with x. This doesn't do full CSE, but
-- is enough to catch some cases, particularly redundant array indexing
-- introduced by the fusion pass.
--
simplifyExp :: Exp aenv t -> Exp aenv t
simplifyExp = cseOpenExp EmptyEnv

simplifyFun :: Fun aenv t -> Fun aenv t
simplifyFun = cseOpenFun EmptyEnv


cseOpenExp
    :: forall env aenv t.
       Gamma   env env aenv
    -> OpenExp env     aenv t
    -> OpenExp env     aenv t
cseOpenExp env = cvt
  where
    cvtA :: OpenAcc aenv a -> OpenAcc aenv a
    cvtA = id

    cvt :: OpenExp env aenv e -> OpenExp env aenv e
    cvt exp = case exp of
      Let bnd body ->
        case lookupEnv env bnd of
          Just ix       -> cvt (inline body (Var ix))
          _             -> let bnd' = cvt bnd
                               env' = incEnv env `PushEnv` weakenE bnd'
                           in
                           Let bnd' (cseOpenExp env' body)
      --
      Var ix            -> Var ix
      Const c           -> Const c
      Tuple tup         -> Tuple (cseTuple env tup)
      Prj tup ix        -> Prj tup (cvt ix)
      IndexNil          -> IndexNil
      IndexCons sh sz   -> IndexCons (cvt sh) (cvt sz)
      IndexHead sh      -> IndexHead (cvt sh)
      IndexTail sh      -> IndexTail (cvt sh)
      IndexAny          -> IndexAny
      ToIndex sh ix     -> ToIndex (cvt sh) (cvt ix)
      FromIndex sh ix   -> FromIndex (cvt sh) (cvt ix)
      Cond p t e        -> Cond (cvt p) (cvt t) (cvt e)
      PrimConst c       -> PrimConst c
      PrimApp f x       -> PrimApp f (cvt x)
      IndexScalar a sh  -> IndexScalar (cvtA a) (cvt sh)
      Shape a           -> Shape (cvtA a)
      ShapeSize sh      -> ShapeSize (cvt sh)
      Intersect s t     -> Intersect (cvt s) (cvt t)


cseTuple
    :: Gamma env env aenv
    -> Tuple.Tuple (OpenExp env aenv) t
    -> Tuple.Tuple (OpenExp env aenv) t
cseTuple _   NilTup          = NilTup
cseTuple env (SnocTup tup e) = cseTuple env tup `SnocTup` cseOpenExp env e


cseOpenFun
    :: Gamma   env env aenv
    -> OpenFun env     aenv t
    -> OpenFun env     aenv t
cseOpenFun env (Body e) = Body (cseOpenExp env e)
cseOpenFun env (Lam  f) = Lam  (cseOpenFun (incEnv env `PushEnv` Var ZeroIdx) f)


-- Witness equality between types. A value of a :=: b is a proof that types a
-- and b are equal. By pattern matching on REFL this fact is introduced to the
-- type checker.
--
data s :=: t where
  REFL :: s :=: s

refl :: (Typeable s, Typeable t) => (x -> y -> Bool) -> x -> y -> Maybe (s :=: t)
refl f a b | f a b     = gcast REFL
           | otherwise = Nothing


-- Compute the congruence of two scalar expressions. Two nodes are congruent if
-- either:
--
--  1. The nodes label constants and the contents are equal
--  2. They have the same operator and their operands are congruent
--
--  The below attempts to use real typed equality, but occasionally still needs
--  to use a cast, particularly when we can only match the representation types.
--
matchOpenExp :: OpenExp env aenv s -> OpenExp env aenv t -> Maybe (s :=: t)
matchOpenExp (Let x1 e1)         (Let x2 e2)
  | Just REFL <- matchOpenExp x1 x2
  , Just REFL <- matchOpenExp e1 e2
  = Just REFL

matchOpenExp (Var v1)            (Var v2)            = matchIdx v1 v2
matchOpenExp (Const c1)          (Const c2)          = refl (==) c1 =<< cast c2
matchOpenExp (Tuple t1)          (Tuple t2)
  | Just REFL <- matchTuple t1 t2
  = gcast REFL

matchOpenExp (Prj ix1 t1)        (Prj ix2 t2)
  | Just REFL <- matchOpenExp  t1  t2
  , Just REFL <- matchTupleIdx ix1 ix2
  = Just REFL

matchOpenExp IndexAny            IndexAny            = gcast REFL
matchOpenExp IndexNil            IndexNil            = Just REFL
matchOpenExp (IndexCons sl1 a1)  (IndexCons sl2 a2)
  | Just REFL <- matchOpenExp sl1 sl2
  , Just REFL <- matchOpenExp a1 a2
  = Just REFL

matchOpenExp (IndexHead sl1)     (IndexHead sl2)
  | Just REFL <- matchOpenExp sl1 sl2
  = Just REFL

matchOpenExp (IndexTail sl1)     (IndexTail sl2)
  | Just REFL <- matchOpenExp sl1 sl2
  = Just REFL

matchOpenExp (ToIndex sh1 i1)    (ToIndex sh2 i2)
  | Just REFL <- matchOpenExp sh1 sh2
  , Just REFL <- matchOpenExp i1 i2
  = Just REFL

matchOpenExp (FromIndex sh1 i1)  (FromIndex sh2 i2)  = matchOpenExp i1 i2 >> matchOpenExp sh1 sh2
matchOpenExp (Cond p1 t1 e1)     (Cond p2 t2 e2)     = matchOpenExp p1 p2 >> matchOpenExp t1 t2 >> matchOpenExp e1 e2
matchOpenExp (PrimConst c1)      (PrimConst c2)      = refl matchPrimConst c1 =<< gcast c2
matchOpenExp (PrimApp f1 x1)     (PrimApp f2 x2)
  | Just REFL <- associative f1
  , Just REFL <- associative f2
  , Just REFL <- matchOpenExp (swizzle x1) (swizzle x2)
  , Just REFL <- matchPrimFun f1 f2
  = Just REFL

  | Just REFL <- matchOpenExp x1 x2
  , Just REFL <- matchPrimFun f1 f2
  = Just REFL

matchOpenExp (IndexScalar a1 x1) (IndexScalar a2 x2)
  | OpenAcc (Avar v1) <- a1
  , OpenAcc (Avar v2) <- a2
  , Just REFL         <- matchIdx v1 v2
  , Just REFL         <- matchOpenExp x1 x2
  = Just REFL

matchOpenExp (Shape a1)          (Shape a2)
  | OpenAcc (Avar v1) <- a1
  , OpenAcc (Avar v2) <- a2
  , Just REFL         <- matchIdx v1 v2
  = gcast REFL

matchOpenExp (ShapeSize sh1)     (ShapeSize sh2)
  | Just REFL <- matchOpenExp sh1 sh2
  = Just REFL

matchOpenExp (Intersect sa1 sb1) (Intersect sa2 sb2) = matchOpenExp sa1 sa2 >> matchOpenExp sb1 sb2
matchOpenExp _                   _                   = Nothing



swizzle :: OpenExp env aenv (a,a) -> OpenExp env aenv (a,a)
swizzle (Tuple (NilTup `SnocTup` x `SnocTup` y))
  | hashOpenExp x <= hashOpenExp y      = Tuple $ NilTup `SnocTup` x `SnocTup` y
  | otherwise                           = Tuple $ NilTup `SnocTup` y `SnocTup` x
swizzle _                               = error "swizzle: expected tuple"


-- Environment projection indices
--
matchIdx :: Idx env s -> Idx env t -> Maybe (s :=: t)
matchIdx ZeroIdx     ZeroIdx     = Just REFL
matchIdx (SuccIdx u) (SuccIdx v) = matchIdx u v
matchIdx _           _           = Nothing

-- Tuple projection indices
--
matchTupleIdx :: TupleIdx tup s -> TupleIdx tup t -> Maybe (s :=: t)
matchTupleIdx ZeroTupIdx     ZeroTupIdx     = Just REFL
matchTupleIdx (SuccTupIdx s) (SuccTupIdx t) = matchTupleIdx s t
matchTupleIdx _              _              = Nothing


matchTuple :: Tuple.Tuple (OpenExp env aenv) s
           -> Tuple.Tuple (OpenExp env aenv) t
           -> Maybe (s :=: t)
matchTuple NilTup          NilTup               = Just REFL
matchTuple (SnocTup t1 e1) (SnocTup t2 e2)
  | Just REFL <- matchTuple   t1 t2
  , Just REFL <- matchOpenExp e1 e2
  = Just REFL

matchTuple _               _                    = Nothing


matchPrimConst :: PrimConst c -> PrimConst c -> Bool
matchPrimConst (PrimMinBound _) (PrimMinBound _) = True
matchPrimConst (PrimMaxBound _) (PrimMaxBound _) = True
matchPrimConst (PrimPi _)       (PrimPi _)       = True
matchPrimConst _                _                = False


matchPrimFun :: (Elt s, Elt t) => PrimFun (a -> s) -> PrimFun (a -> t) -> Maybe (s :=: t)
matchPrimFun (PrimAdd _)            (PrimAdd _)            = Just REFL
matchPrimFun (PrimSub _)            (PrimSub _)            = Just REFL
matchPrimFun (PrimMul _)            (PrimMul _)            = Just REFL
matchPrimFun (PrimNeg _)            (PrimNeg _)            = Just REFL
matchPrimFun (PrimAbs _)            (PrimAbs _)            = Just REFL
matchPrimFun (PrimSig _)            (PrimSig _)            = Just REFL
matchPrimFun (PrimQuot _)           (PrimQuot _)           = Just REFL
matchPrimFun (PrimRem _)            (PrimRem _)            = Just REFL
matchPrimFun (PrimIDiv _)           (PrimIDiv _)           = Just REFL
matchPrimFun (PrimMod _)            (PrimMod _)            = Just REFL
matchPrimFun (PrimBAnd _)           (PrimBAnd _)           = Just REFL
matchPrimFun (PrimBOr _)            (PrimBOr _)            = Just REFL
matchPrimFun (PrimBXor _)           (PrimBXor _)           = Just REFL
matchPrimFun (PrimBNot _)           (PrimBNot _)           = Just REFL
matchPrimFun (PrimBShiftL _)        (PrimBShiftL _)        = Just REFL
matchPrimFun (PrimBShiftR _)        (PrimBShiftR _)        = Just REFL
matchPrimFun (PrimBRotateL _)       (PrimBRotateL _)       = Just REFL
matchPrimFun (PrimBRotateR _)       (PrimBRotateR _)       = Just REFL
matchPrimFun (PrimFDiv _)           (PrimFDiv _)           = Just REFL
matchPrimFun (PrimRecip _)          (PrimRecip _)          = Just REFL
matchPrimFun (PrimSin _)            (PrimSin _)            = Just REFL
matchPrimFun (PrimCos _)            (PrimCos _)            = Just REFL
matchPrimFun (PrimTan _)            (PrimTan _)            = Just REFL
matchPrimFun (PrimAsin _)           (PrimAsin _)           = Just REFL
matchPrimFun (PrimAcos _)           (PrimAcos _)           = Just REFL
matchPrimFun (PrimAtan _)           (PrimAtan _)           = Just REFL
matchPrimFun (PrimAsinh _)          (PrimAsinh _)          = Just REFL
matchPrimFun (PrimAcosh _)          (PrimAcosh _)          = Just REFL
matchPrimFun (PrimAtanh _)          (PrimAtanh _)          = Just REFL
matchPrimFun (PrimExpFloating _)    (PrimExpFloating _)    = Just REFL
matchPrimFun (PrimSqrt _)           (PrimSqrt _)           = Just REFL
matchPrimFun (PrimLog _)            (PrimLog _)            = Just REFL
matchPrimFun (PrimFPow _)           (PrimFPow _)           = Just REFL
matchPrimFun (PrimLogBase _)        (PrimLogBase _)        = Just REFL
matchPrimFun (PrimAtan2 _)          (PrimAtan2 _)          = Just REFL
matchPrimFun (PrimTruncate _ _)     (PrimTruncate _ _)     = gcast REFL -- output type
matchPrimFun (PrimRound _ _)        (PrimRound _ _)        = gcast REFL
matchPrimFun (PrimFloor _ _)        (PrimFloor _ _)        = gcast REFL
matchPrimFun (PrimCeiling _ _)      (PrimCeiling _ _)      = gcast REFL
matchPrimFun (PrimLt _)             (PrimLt _)             = Just REFL
matchPrimFun (PrimGt _)             (PrimGt _)             = Just REFL
matchPrimFun (PrimLtEq _)           (PrimLtEq _)           = Just REFL
matchPrimFun (PrimGtEq _)           (PrimGtEq _)           = Just REFL
matchPrimFun (PrimEq _)             (PrimEq _)             = Just REFL
matchPrimFun (PrimNEq _)            (PrimNEq _)            = Just REFL
matchPrimFun (PrimMax _)            (PrimMax _)            = Just REFL
matchPrimFun (PrimMin _)            (PrimMin _)            = Just REFL
matchPrimFun (PrimFromIntegral _ _) (PrimFromIntegral _ _) = gcast REFL
matchPrimFun PrimLAnd               PrimLAnd               = Just REFL
matchPrimFun PrimLOr                PrimLOr                = Just REFL
matchPrimFun PrimLNot               PrimLNot               = Just REFL
matchPrimFun PrimOrd                PrimOrd                = Just REFL
matchPrimFun PrimChr                PrimChr                = Just REFL
matchPrimFun PrimBoolToInt          PrimBoolToInt          = Just REFL
matchPrimFun _                      _                      = Nothing


-- Discriminate binary associative functions
-- TLM: how to extract the 'a' as a pair type?
--
associative :: PrimFun (a -> r) -> Maybe (a :=: (x,x))
associative _ = Nothing

{--
associative :: PrimFun f -> Bool
associative f = case f of
  PrimAdd     _ -> True
  PrimMul     _ -> True
  PrimBAnd    _ -> True
  PrimBOr     _ -> True
  PrimBXor    _ -> True
  PrimEq      _ -> True
  PrimNEq     _ -> True
  PrimMax     _ -> True
  PrimMin     _ -> True
  PrimLAnd      -> True
  PrimLOr       -> True
  _             -> False
--}


-- Hashable scalar expressions
--
hashIdx :: Idx env t -> Int
hashIdx = hash . idxToInt

hashTupleIdx :: TupleIdx tup e -> Int
hashTupleIdx = hash . tupleIdxToInt


hashOpenExp :: forall env aenv e. OpenExp env aenv e -> Int
hashOpenExp (Let x e)                   = hash "Let"            `combine` hashOpenExp x  `combine` hashOpenExp e
hashOpenExp (Var ix)                    = hash "Var"            `combine` hashIdx ix
hashOpenExp (Const c)                   = hash "Const"          `hashWithSalt` show (Sugar.toElt c :: e)
hashOpenExp (Tuple t)                   = hash "Tuple"          `combine` hashTuple t
hashOpenExp (Prj ix e)                  = hash "Prj"            `combine` hashTupleIdx ix `combine` hashOpenExp e
hashOpenExp IndexAny                    = hash "IndexAny"
hashOpenExp IndexNil                    = hash "IndexNil"
hashOpenExp (IndexCons sl a)            = hash "IndexCons"      `combine` hashOpenExp sl `combine` hashOpenExp a
hashOpenExp (IndexHead sl)              = hash "IndexHead"      `combine` hashOpenExp sl
hashOpenExp (IndexTail sl)              = hash "IndexTail"      `combine` hashOpenExp sl
hashOpenExp (ToIndex sh i)              = hash "ToIndex"        `combine` hashOpenExp sh `combine` hashOpenExp i
hashOpenExp (FromIndex sh i)            = hash "FromIndex"      `combine` hashOpenExp sh `combine` hashOpenExp i
hashOpenExp (Cond c t e)                = hash "Cond"           `combine` hashOpenExp c  `combine` hashOpenExp t `combine` hashOpenExp e
hashOpenExp (PrimApp f x)               = hash "PrimApp"        `combine` hashPrimFun f  `combine` hashOpenExp x
hashOpenExp (PrimConst c)               = hash "PrimConst"      `combine` hashPrimConst c
hashOpenExp (IndexScalar a ix)
  | OpenAcc (Avar v) <- a               = hash "IndexScalar"    `combine` hashIdx v      `combine` hashOpenExp ix
  | otherwise                           = error "hash: IndexScalar: expected array variable"
--
hashOpenExp (Shape a)
  | OpenAcc (Avar v) <- a               = hash "Shape"          `combine` hashIdx v
  | otherwise                           = error "hash: Shape: expected array variable"
--
hashOpenExp (ShapeSize sh)              = hash "ShapeSize"      `combine` hashOpenExp sh
hashOpenExp (Intersect sa sb)           = hash "Intersect"      `combine` hashOpenExp sa `combine` hashOpenExp sb


hashTuple :: Tuple.Tuple (OpenExp env aenv) e -> Int
hashTuple NilTup                        = hash "NilTup"
hashTuple (SnocTup t e)                 = hash "SnocTup"        `combine` hashTuple t `combine` hashOpenExp e


hashPrimConst :: PrimConst c -> Int
hashPrimConst (PrimMinBound _)          = hash "PrimMinBound"
hashPrimConst (PrimMaxBound _)          = hash "PrimMaxBound"
hashPrimConst (PrimPi _)                = hash "PrimPi"

hashPrimFun :: PrimFun f -> Int
hashPrimFun (PrimAdd _)                 = hash "PrimAdd"
hashPrimFun (PrimSub _)                 = hash "PrimSub"
hashPrimFun (PrimMul _)                 = hash "PrimMul"
hashPrimFun (PrimNeg _)                 = hash "PrimNeg"
hashPrimFun (PrimAbs _)                 = hash "PrimAbs"
hashPrimFun (PrimSig _)                 = hash "PrimSig"
hashPrimFun (PrimQuot _)                = hash "PrimQuot"
hashPrimFun (PrimRem _)                 = hash "PrimRem"
hashPrimFun (PrimIDiv _)                = hash "PrimIDiv"
hashPrimFun (PrimMod _)                 = hash "PrimMod"
hashPrimFun (PrimBAnd _)                = hash "PrimBAnd"
hashPrimFun (PrimBOr _)                 = hash "PrimBOr"
hashPrimFun (PrimBXor _)                = hash "PrimBXor"
hashPrimFun (PrimBNot _)                = hash "PrimBNot"
hashPrimFun (PrimBShiftL _)             = hash "PrimBShiftL"
hashPrimFun (PrimBShiftR _)             = hash "PrimBShiftR"
hashPrimFun (PrimBRotateL _)            = hash "PrimBRotateL"
hashPrimFun (PrimBRotateR _)            = hash "PrimBRotateR"
hashPrimFun (PrimFDiv _)                = hash "PrimFDiv"
hashPrimFun (PrimRecip _)               = hash "PrimRecip"
hashPrimFun (PrimSin _)                 = hash "PrimSin"
hashPrimFun (PrimCos _)                 = hash "PrimCos"
hashPrimFun (PrimTan _)                 = hash "PrimTan"
hashPrimFun (PrimAsin _)                = hash "PrimAsin"
hashPrimFun (PrimAcos _)                = hash "PrimAcos"
hashPrimFun (PrimAtan _)                = hash "PrimAtan"
hashPrimFun (PrimAsinh _)               = hash "PrimAsinh"
hashPrimFun (PrimAcosh _)               = hash "PrimAcosh"
hashPrimFun (PrimAtanh _)               = hash "PrimAtanh"
hashPrimFun (PrimExpFloating _)         = hash "PrimExpFloating"
hashPrimFun (PrimSqrt _)                = hash "PrimSqrt"
hashPrimFun (PrimLog _)                 = hash "PrimLog"
hashPrimFun (PrimFPow _)                = hash "PrimFPow"
hashPrimFun (PrimLogBase _)             = hash "PrimLogBase"
hashPrimFun (PrimAtan2 _)               = hash "PrimAtan2"
hashPrimFun (PrimTruncate _ _)          = hash "PrimTruncate"
hashPrimFun (PrimRound _ _)             = hash "PrimRound"
hashPrimFun (PrimFloor _ _)             = hash "PrimFloor"
hashPrimFun (PrimCeiling _ _)           = hash "PrimCeiling"
hashPrimFun (PrimLt _)                  = hash "PrimLt"
hashPrimFun (PrimGt _)                  = hash "PrimGt"
hashPrimFun (PrimLtEq _)                = hash "PrimLtEq"
hashPrimFun (PrimGtEq _)                = hash "PrimGtEq"
hashPrimFun (PrimEq _)                  = hash "PrimEq"
hashPrimFun (PrimNEq _)                 = hash "PrimNEq"
hashPrimFun (PrimMax _)                 = hash "PrimMax"
hashPrimFun (PrimMin _)                 = hash "PrimMin"
hashPrimFun (PrimFromIntegral _ _)      = hash "PrimFromIntegral"
hashPrimFun PrimLAnd                    = hash "PrimLAnd"
hashPrimFun PrimLOr                     = hash "PrimLOr"
hashPrimFun PrimLNot                    = hash "PrimLNot"
hashPrimFun PrimOrd                     = hash "PrimOrd"
hashPrimFun PrimChr                     = hash "PrimChr"
hashPrimFun PrimBoolToInt               = hash "PrimBoolToInt"

