{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
lookupEnv EmptyEnv        _                = Nothing
lookupEnv (PushEnv env e) x
  | maybe False (matchOpenExp e) (gcast x) = gcast ZeroIdx
  | otherwise                              = SuccIdx `fmap` lookupEnv env x


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


-- Compute the congruence of two scalar expressions. Two nodes are congruent if
-- either:
--
--  1. The nodes label constants and the contents are equal
--  2. They have the same operator and their operands are congruent
--
(==^) :: (Typeable a, Typeable b) => OpenExp env aenv a -> OpenExp env aenv b -> Bool
(==^) x y = maybe False (matchOpenExp x) (gcast y)

matchOpenExp :: OpenExp env aenv a -> OpenExp env aenv a -> Bool
matchOpenExp (Let _ _)           (Let _ _)           = error "matchOpenExp: Let"  -- False? We don't expect to get here.
matchOpenExp (Var v1)            (Var v2)            = idxToInt v1 == idxToInt v2
matchOpenExp (Const c1)          (Const c2)          = c1 == c2
matchOpenExp (Tuple t1)          (Tuple t2)          = matchTuple t1 t2
matchOpenExp (Prj ix1 t1)        (Prj ix2 t2)        = tupleIdxToInt ix1 == tupleIdxToInt ix2 && t1 ==^ t2
matchOpenExp IndexAny            IndexAny            = True
matchOpenExp IndexNil            IndexNil            = True
matchOpenExp (IndexCons sl1 a1)  (IndexCons sl2 a2)  = sl1 ==^ sl2 && a1 ==^ a2
matchOpenExp (IndexHead sl1)     (IndexHead sl2)     = sl1 ==^ sl2
matchOpenExp (IndexTail sl1)     (IndexTail sl2)     = sl1 ==^ sl2
matchOpenExp (ToIndex sh1 i1)    (ToIndex sh2 i2)    = sh1 ==^ sh2 && i1 ==^ i2
matchOpenExp (FromIndex sh1 i1)  (FromIndex sh2 i2)  = sh1 ==^ sh2 && i1 ==^ i2
matchOpenExp (Cond p1 t1 e1)     (Cond p2 t2 e2)     = p1  ==^ p2  && t1 ==^ t2 && e1 ==^ e2
matchOpenExp (PrimConst c1)      (PrimConst c2)      = matchPrimConst c1 c2
matchOpenExp (PrimApp f1 x1)     (PrimApp f2 x2)
  | not (maybe False (matchPrimFun f1) (gcast f2))
  = False

  | associative f1
  , Tuple (NilTup `SnocTup` a1 `SnocTup` b1)  <- x1
  , Tuple (NilTup `SnocTup` a2 `SnocTup` b2)  <- x2
  = let sub1  = hashOpenExp a1 < hashOpenExp b1
        sub2  = hashOpenExp a2 < hashOpenExp b2
    in
    if sub1     -- compare small hashes first
       then if sub2
               then a1 ==^ a2 && b1 ==^ b2
               else a1 ==^ b2 && b1 ==^ a2
       else if sub2
               then b1 ==^ a2 && a1 ==^ b2
               else b1 ==^ b2 && a1 ==^ a2

  | otherwise
  = x1 ==^ x2

matchOpenExp (IndexScalar a1 x1) (IndexScalar a2 x2)
  | OpenAcc (Avar v1) <- a1
  , OpenAcc (Avar v2) <- a2
  = idxToInt v1 == idxToInt v2 && x1 ==^ x2

  | otherwise
  = error "Eq: IndexScalar: expected array variable"

matchOpenExp (Shape a1)          (Shape a2)
  | OpenAcc (Avar v1) <- a1
  , OpenAcc (Avar v2) <- a2
  = idxToInt v1 == idxToInt v2

  | otherwise
  = error "Eq: Shape: expected array variable"

matchOpenExp (ShapeSize sh1)     (ShapeSize sh2)     = sh1 ==^ sh2
matchOpenExp (Intersect sa1 sb1) (Intersect sa2 sb2) = sa1 ==^ sa2 && sb1 ==^ sb2

matchOpenExp _                   _                   = False



matchTuple :: Tuple.Tuple (OpenExp env aenv) t -> Tuple.Tuple (OpenExp env aenv) t -> Bool
matchTuple NilTup          NilTup          = True
matchTuple (SnocTup t1 e1) (SnocTup t2 e2) = matchTuple t1 t2 && matchOpenExp e1 e2
matchTuple _               _               = False

matchPrimConst :: PrimConst c -> PrimConst c -> Bool
matchPrimConst (PrimMinBound _) (PrimMinBound _) = True
matchPrimConst (PrimMaxBound _) (PrimMaxBound _) = True
matchPrimConst (PrimPi _)       (PrimPi _)       = True
matchPrimConst _                _                = False


matchPrimFun :: PrimFun f -> PrimFun f -> Bool
matchPrimFun (PrimAdd _)            (PrimAdd _)            = True
matchPrimFun (PrimSub _)            (PrimSub _)            = True
matchPrimFun (PrimMul _)            (PrimMul _)            = True
matchPrimFun (PrimNeg _)            (PrimNeg _)            = True
matchPrimFun (PrimAbs _)            (PrimAbs _)            = True
matchPrimFun (PrimSig _)            (PrimSig _)            = True
matchPrimFun (PrimQuot _)           (PrimQuot _)           = True
matchPrimFun (PrimRem _)            (PrimRem _)            = True
matchPrimFun (PrimIDiv _)           (PrimIDiv _)           = True
matchPrimFun (PrimMod _)            (PrimMod _)            = True
matchPrimFun (PrimBAnd _)           (PrimBAnd _)           = True
matchPrimFun (PrimBOr _)            (PrimBOr _)            = True
matchPrimFun (PrimBXor _)           (PrimBXor _)           = True
matchPrimFun (PrimBNot _)           (PrimBNot _)           = True
matchPrimFun (PrimBShiftL _)        (PrimBShiftL _)        = True
matchPrimFun (PrimBShiftR _)        (PrimBShiftR _)        = True
matchPrimFun (PrimBRotateL _)       (PrimBRotateL _)       = True
matchPrimFun (PrimBRotateR _)       (PrimBRotateR _)       = True
matchPrimFun (PrimFDiv _)           (PrimFDiv _)           = True
matchPrimFun (PrimRecip _)          (PrimRecip _)          = True
matchPrimFun (PrimSin _)            (PrimSin _)            = True
matchPrimFun (PrimCos _)            (PrimCos _)            = True
matchPrimFun (PrimTan _)            (PrimTan _)            = True
matchPrimFun (PrimAsin _)           (PrimAsin _)           = True
matchPrimFun (PrimAcos _)           (PrimAcos _)           = True
matchPrimFun (PrimAtan _)           (PrimAtan _)           = True
matchPrimFun (PrimAsinh _)          (PrimAsinh _)          = True
matchPrimFun (PrimAcosh _)          (PrimAcosh _)          = True
matchPrimFun (PrimAtanh _)          (PrimAtanh _)          = True
matchPrimFun (PrimExpFloating _)    (PrimExpFloating _)    = True
matchPrimFun (PrimSqrt _)           (PrimSqrt _)           = True
matchPrimFun (PrimLog _)            (PrimLog _)            = True
matchPrimFun (PrimFPow _)           (PrimFPow _)           = True
matchPrimFun (PrimLogBase _)        (PrimLogBase _)        = True
matchPrimFun (PrimAtan2 _)          (PrimAtan2 _)          = True
matchPrimFun (PrimTruncate _ _)     (PrimTruncate _ _)     = True
matchPrimFun (PrimRound _ _)        (PrimRound _ _)        = True
matchPrimFun (PrimFloor _ _)        (PrimFloor _ _)        = True
matchPrimFun (PrimCeiling _ _)      (PrimCeiling _ _)      = True
matchPrimFun (PrimLt _)             (PrimLt _)             = True
matchPrimFun (PrimGt _)             (PrimGt _)             = True
matchPrimFun (PrimLtEq _)           (PrimLtEq _)           = True
matchPrimFun (PrimGtEq _)           (PrimGtEq _)           = True
matchPrimFun (PrimEq _)             (PrimEq _)             = True
matchPrimFun (PrimNEq _)            (PrimNEq _)            = True
matchPrimFun (PrimMax _)            (PrimMax _)            = True
matchPrimFun (PrimMin _)            (PrimMin _)            = True
matchPrimFun (PrimFromIntegral _ _) (PrimFromIntegral _ _) = True
matchPrimFun PrimLAnd               PrimLAnd               = True
matchPrimFun PrimLOr                PrimLOr                = True
matchPrimFun PrimLNot               PrimLNot               = True
matchPrimFun PrimOrd                PrimOrd                = True
matchPrimFun PrimChr                PrimChr                = True
matchPrimFun PrimBoolToInt          PrimBoolToInt          = True
matchPrimFun _                      _                      = False


-- Discriminate binary associative functions
--
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

