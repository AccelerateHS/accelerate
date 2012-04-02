{-# LANGUAGE StandaloneDeriving #-}
module Data.Array.Accelerate.SimpleAST  
--       (AccExp(..))
 where

-- Interned symbols:
-- import StringTable.Atom
-- import Data.Atom.Simple
import Data.Symbol
import Data.Map

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
type Env = Map Var Exp  


type Dimension = [Int]

data Type = TInt 
          | TDouble
          | TTuple [Type] 
          | TArray Type
 deriving (Read,Show,Eq)


--------------------------------------------------------------------------------
-- Accelerate Array-level Expressions
--------------------------------------------------------------------------------

data Exp = 
    Int
    -- The element types. Only Int for now, but the others would be
    -- easy enough to add, I think.
--  | Array ??? Exp
    -- Array Dimension Element
  | Unit Exp
    -- Unit Element -- Turn an element into a singleton array
  | Let  Var     Exp Exp 
    -- Let Binder Bindee Body -- Bind the array in the var. Use forz
    -- common subexpression elimination

  | LetPair (Var, Var) Exp Exp 
    -- This binds an array expression returning a PAIR.
    -- Let (Var1, Var2) (PairArrays Array1 Array2) Body

  | PairArrays Exp Exp
    -- PairArrays Array1 Array2
  
  | Vr Var -- Variable bound by a Let.
    
-- Var "x"
  | Apply Exp Exp
    -- Function $ Argument
  | Cond Exp Exp Exp -- If statements
  | Use -- REAL ARRAY GOES HERE
    -- Use Array
--  | Reshape ??? Exp 
    -- Reshape Shape Array
  | Generate Exp Exp
    -- Generate Function Array, very similar to map
--  | Replicate ??? ??? Exp
    -- Replicate IndexOfSomeKind? SomeValue Array
--  | Index ??? Exp ???
    -- Index SomeMultiDimensionalIndex Array 'SliceValue'?
  | Map Exp Exp
    -- Map Function Array
  | ZipWith Exp Exp Exp
    -- ZipWith Function Array1 Array2
  | Fold Exp Exp Exp
    -- Fold Function Default Array
  | Fold1 Exp Exp
    -- Fold1 Function Array

  | FoldSeg Exp Exp Exp Exp

--  | FoldSeg Exp Exp Exp ???
    -- FoldSeg Function Default Array 'Segment Descriptor'
  | Fold1Seg Exp Exp Exp 
    -- FoldSeg Function Array 'Segment Descriptor'
  | Scanl Exp Exp Exp
    -- Scanl Function InitialValue LinearArray
  | Scanl' Exp Exp Exp
    -- Scanl' Function InitialValue LinearArray
  | Scanl1 Exp Exp
    -- Scanl Function LinearArray
  | Scanr Exp Exp Exp
    -- Scanr Function InitialValue LinearArray
  | Scanr' Exp Exp Exp
    -- Scanr' Function InitialValue LinearArray
  | Scanr1 Exp Exp
    -- Scanr Function LinearArray
  | Permute Exp Exp Exp Exp
    -- Permute Function DefaultArray PermuteFunction
    -- SourceArray
--  | Backpermute ??? Exp Exp
    -- Backpermute DimensionsOfReulst PermuteFunction
    -- SourceArray
--  | Stencil Exp ??? Exp
    -- Stencil Function BoundaryCondition SourceArray
--  | Stencil2 Exp ??? Exp ??? Exp
    -- Stencial2 Function Boundary1 Array1 Boundary2 Array2
  | Lam Var Exp
    -- \Var -> Body
  | PrimApp Prim [Exp]
    -- Any of the primitive functions

  | Tuple [Exp]

 deriving (Read,Show,Eq)


--------------------------------------------------------------------------------
-- Accelerate Scalar Expressions
--------------------------------------------------------------------------------


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

