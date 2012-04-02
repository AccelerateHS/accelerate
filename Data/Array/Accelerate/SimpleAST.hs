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
    Int
    -- The element types. Only Int for now, but the others would be
    -- easy enough to add, I think.
--  | Array ??? AExp
    -- Array Dimension Element
  | Unit AExp
    -- Unit Element -- Turn an element into a singleton array
  | Let  Var     AExp AExp 
    -- Let Binder Bindee Body -- Bind the array in the var. Use forz
    -- common subexpression elimination

  | LetPair (Var, Var) AExp AExp 
    -- This binds an array expression returning a PAIR.
    -- Let (Var1, Var2) (PairArrays Array1 Array2) Body

  | PairArrays AExp AExp
    -- PairArrays Array1 Array2
  
  | Vr Var -- Variable bound by a Let.
    
-- Var "x"
  | Apply AExp AExp
    -- Function $ Argument
  | Cond AExp AExp AExp -- If statements
  | Use -- REAL ARRAY GOES HERE
    -- Use Array
--  | Reshape ??? AExp 
    -- Reshape Shape Array
  | Generate AExp AExp
    -- Generate Function Array, very similar to map
--  | Replicate ??? ??? AExp
    -- Replicate IndexOfSomeKind? SomeValue Array
--  | Index ??? AExp ???
    -- Index SomeMultiDimensionalIndex Array 'SliceValue'?
  | Map AExp AExp
    -- Map Function Array
  | ZipWith AExp AExp AExp
    -- ZipWith Function Array1 Array2
  | Fold AExp AExp AExp
    -- Fold Function Default Array
  | Fold1 AExp AExp
    -- Fold1 Function Array

  | FoldSeg AExp AExp AExp AExp

--  | FoldSeg AExp AExp AExp ???
    -- FoldSeg Function Default Array 'Segment Descriptor'
  | Fold1Seg AExp AExp AExp 
    -- FoldSeg Function Array 'Segment Descriptor'
  | Scanl AExp AExp AExp
    -- Scanl Function InitialValue LinearArray
  | Scanl' AExp AExp AExp
    -- Scanl' Function InitialValue LinearArray
  | Scanl1 AExp AExp
    -- Scanl Function LinearArray
  | Scanr AExp AExp AExp
    -- Scanr Function InitialValue LinearArray
  | Scanr' AExp AExp AExp
    -- Scanr' Function InitialValue LinearArray
  | Scanr1 AExp AExp
    -- Scanr Function LinearArray
  | Permute AExp AExp AExp AExp
    -- Permute Function DefaultArray PermuteFunction
    -- SourceArray
--  | Backpermute ??? AExp AExp
    -- Backpermute DimensionsOfReulst PermuteFunction
    -- SourceArray
--  | Stencil AExp ??? AExp
    -- Stencil Function BoundaryCondition SourceArray
--  | Stencil2 AExp ??? AExp ??? AExp
    -- Stencial2 Function Boundary1 Array1 Boundary2 Array2
  | Lam Var AExp
    -- \Var -> Body
  | PrimApp Prim [AExp]
    -- Any of the primitive functions

  | Tuple [AExp]

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

