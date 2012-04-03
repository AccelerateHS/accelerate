{-# LANGUAGE StandaloneDeriving #-}
module Data.Array.Accelerate.SimpleAST  
--       (AccExp(..))
 where

-- Interned symbols:
-- import StringTable.Atom
-- import Data.Atom.Simple
import Data.Symbol
import Data.Map
import Data.Int
import Data.Word
import Foreign.C.Types

--------------------------------------------------------------------------------

-- A simple representation of variables
-- Honestly though, since we're trying to convert from de Brujin
-- indicies to this... it might just as well use the indicies. 
--data Var = Var String
-- deriving (Read,Show,Eq)          
var :: String -> Var
----------------------------------------
-- stringtable-atom:
-- var = toAtom
-- type Var = Atom

----------------------------------------
-- simple-atom:
-- var = intern
-- type Var = Symbol

----------------------------------------
-- 'symbol' package:
var = intern
type Var = Symbol 
instance Show Symbol where 
 show = unintern
instance Read Symbol where 
-- read = intern
-- NOTE - this package would seem to be unsafe because the Symbol type
-- constructor is exported.
----------------------------------------

-- A simple environment
-- {Var -> Expression}
type Env = Map Var AExp  


type Dimension = [Int]

data Type = TInt 
          | TDouble
          | TTuple [Type] 
          | TArray Type
 deriving (Read,Show,Eq)


--------------------------------------------------------------------------------
-- Accelerate Array-level Expressions
--------------------------------------------------------------------------------

data AExp = 

    Vr Var -- Array variable bound by a Let.

  | Unit Exp -- Turn an element into a singleton array

  | Let  Var     AExp AExp 
   -- | Let Binder Bindee Body - Bind the array in the var. 
   -- Used for common subexpression elimination

  | LetPair (Var, Var) AExp AExp 
    -- This binds an array expression returning a PAIR.
    -- Let (Var1, Var2) (PairArrays Array1 Array2) Body

  | PairArrays AExp AExp
    -- PairArrays Array1 Array2
     
  | Apply AFun AExp    -- Function $ Argument
  | Cond Exp AExp AExp -- Array level if statements
  
  | Use String -- A REAL ARRAY GOES HERE! -- TEMP - FIXME
  | Generate Exp Fun
    -- Generate Function Array, very similar to map
  | Replicate String Exp AExp  -- TEMP - fix first field
  | Index     String AExp Exp  -- TEMP - fix first field 
                               -- Index sliceIndex Array SliceDims
    
  | Map      Fun AExp          -- Map Function Array
  | ZipWith  Fun AExp AExp     -- ZipWith Function Array1 Array2
  | Fold     Fun Exp AExp      -- Fold Function Default Array
  | Fold1    Fun AExp          -- Fold1 Function Array
  | FoldSeg  Fun Exp AExp AExp -- FoldSeg Function Default Array 'Segment Descriptor'
  | Fold1Seg Fun     AExp AExp -- FoldSeg Function         Array 'Segment Descriptor'
  | Scanl    Fun Exp AExp      -- Scanl  Function InitialValue LinearArray
  | Scanl'   Fun Exp AExp      -- Scanl' Function InitialValue LinearArray
  | Scanl1   Fun     AExp      -- Scanl  Function              LinearArray
  | Scanr    Fun Exp AExp      -- Scanr  Function InitialValue LinearArray
  | Scanr'   Fun Exp AExp      -- Scanr' Function InitialValue LinearArray
  | Scanr1   Fun     AExp      -- Scanr  Function              LinearArray
  | Permute  Fun AExp AExp AExp -- Permute Function DefaultArray PermuteFunction SourceArray

--  | Reshape ??? AExp 
    -- Reshape Shape Array
--  | Backpermute ??? AExp AExp
    -- Backpermute DimensionsOfReulst PermuteFunction
    -- SourceArray
--  | Stencil AExp ??? AExp
    -- Stencil Function BoundaryCondition SourceArray
--  | Stencil2 AExp ??? AExp ??? AExp
    -- Stencial2 Function Boundary1 Array1 Boundary2 Array2

 deriving (Read,Show,Eq)

data AFun = ALam [Var] AExp
 deriving (Read,Show,Eq)

--------------------------------------------------------------------------------
-- Accelerate Scalar Expressions
--------------------------------------------------------------------------------

data Fun = Lam [Var] Exp
 deriving (Read,Show,Eq)
          
data Exp = 

    EVr Var -- Variable bound by a Let.

--  | Lam Var Exp

  | EPrimApp Prim [Exp]  -- *Any* primitive scalar function
  | ETuple [Exp]

  | EConst Const
  -- TODO -- support other types in Elt.

-- [2012.04.02] I can't presently compute the length from the TupleIdx.
--  | EPrj Int Int Exp  -- n m e : Project the nth field of an m-length tuple.

  | ETupProjectFromRight Int Exp  -- Project the nth field FROM THE RIGHT end of the tuple.

  -- Index into a multi-dimensional array:
  | EIndex [Exp]
  | EIndexAny 

  -- This is strange but Accelerate would seem to allow run-time CONSING of indices:
  | EIndexConsDynamic Exp Exp
  | EIndexHeadDynamic Exp 
  | EIndexTailDynamic Exp 

  -- -- Array indices & shapes
  -- IndexNil    :: PreOpenExp acc env aenv Z
  -- IndexCons   :: (Slice sl, Elt a)
  --             => PreOpenExp acc env aenv sl
  --             -> PreOpenExp acc env aenv a
  --             -> PreOpenExp acc env aenv (sl:.a)
  -- IndexHead   :: (Slice sl, Elt a)
  --             => PreOpenExp acc env aenv (sl:.a)
  --             -> PreOpenExp acc env aenv a
  -- IndexTail   :: (Slice sl, Elt a)
  --             => PreOpenExp acc env aenv (sl:.a)
  --             -> PreOpenExp acc env aenv sl

  -- Conditional expression (non-strict in 2nd and 3rd argument):
  | ECond Exp Exp Exp

  -- Project a single scalar from an array
  -- the array expression can not contain any free scalar variables
  | EIndexScalar AExp Exp 

  -- Array shape
  -- the array expression can not contain any free scalar variables
  | EShape AExp

  -- Number of elements of an array
  -- the array expression can not contain any free scalar variables
  | ESize AExp 


 deriving (Read,Show,Eq)

data Const = I Int  | I8 Int8  | I16 Int16  | I32 Int32  | I64 Int64
           | W Word | W8 Word8 | W16 Word16 | W32 Word32 | W64 Word64
           | F Float | D Double | C Char | B Bool
           | ConstTup [Const]
           -- Special constants:
           | MinBound | MaxBound | Pi
           -- C types, rather annoying:
           | CF CFloat   | CD CDouble 
           | CS  CShort  | CI  CInt  | CL  CLong  | CLL  CLLong
           | CUS CUShort | CUI CUInt | CUL CULong | CULL CULLong
           | CC  CChar   | CSC CSChar | CUC CUChar 
 deriving (Read,Show,Eq)

--------------------------------------------------------------------------------
-- Accelerate Primitive Operations
--------------------------------------------------------------------------------

data Prim = NP NumPrim
          | IP IntPrim
          | FP FloatPrim
  deriving (Read,Show,Eq)
          
-- Neg/Abs/Sig are unary:
data NumPrim = Add | Mul | Neg | Abs | Sig
  deriving (Read,Show,Eq)

-- All binops except BNot, shifts and rotates take an Int constant as second arg:
data IntPrim = Quot | Rem | IDiv | Mod | 
               BAnd | BOr | BXor | BNot | BShiftL | BShiftR | BRotateL | BRotateR
  deriving (Read,Show,Eq)
           
data FloatPrim = 
      -- Unary:
      Recip | Sin | Cos | Tan | Asin | Acos | Atan | Asinh | Acosh | Atanh | ExpFloating | Sqrt | Log |
      -- Binary:                  
      FDiv | FPow | LogBase | Atan2 | Truncate | Round | Floor | Ceiling
  deriving (Read,Show,Eq)
           
-- relational and equality operators
data ScalarPrim = Lt | Gt | LtEq | GtEq | Eq | NEq | Max | Min
  deriving (Read,Show,Eq)

data BoolPrim = And | Or | Not
  deriving (Read,Show,Eq)

data OtherPrim = Ord | Chr | BoolToInt | FromIntegral
  deriving (Read,Show,Eq)

{-
data PrimFun sig where
...
  -- logical operators
  PrimLAnd :: PrimFun ((Bool, Bool) -> Bool)
  PrimLOr  :: PrimFun ((Bool, Bool) -> Bool)
  PrimLNot :: PrimFun (Bool         -> Bool)

  -- character conversions
  PrimOrd  :: PrimFun (Char -> Int)
  PrimChr  :: PrimFun (Int  -> Char)
  -- FIXME: use IntegralType?

  -- FIXME: conversions between various integer types
  --        should we have an overloaded functions like 'toInt'?  
  --        (or 'fromEnum' for enums?)
  PrimBoolToInt    :: PrimFun (Bool -> Int)
  PrimFromIntegral :: IntegralType a -> NumType b -> PrimFun (a -> b)
-}

