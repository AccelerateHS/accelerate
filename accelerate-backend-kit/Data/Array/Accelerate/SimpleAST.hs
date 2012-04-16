{-# LANGUAGE DeriveGeneric #-}
module Data.Array.Accelerate.SimpleAST  
   (     
     -- * The types making up Accelerate ASTs:
     AExp(..), AFun(..), 
     Exp(..), Fun(..), Var,
     Type(..), Const(..),
     Prim(..), NumPrim(..), FloatPrim(..), ScalarPrim(..), BoolPrim(..), OtherPrim(..),
     Boundary(..),
     
     -- * Helper routines and predicates:
     var, isIntType, isFloatType
    )   
 where

-- Interned symbols:
----------------------------------------
-- import StringTable.Atom
-- import Data.Atom.Simple
import Data.Symbol
----------------------------------------
import Data.Int
import Data.Word
import Foreign.C.Types
import Text.PrettyPrint.GenericPretty
import Pretty (text) -- ghc api

--------------------------------------------------------------------------------

-- A simple representation of variables
-- Honestly though, since we're trying to convert from de Brujin
-- indicies to this... it might just as well use the indicies. 
var :: String -> Var
----------------------------------------
-- stringtable-atom package:
-- var = toAtom
-- type Var = Atom
----------------------------------------
-- simple-atom package:
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

--------------------------------------------------------------------------------
-- Accelerate Types
--------------------------------------------------------------------------------

-- | Accelerate types.
data Type = TTuple [Type]
          | TArray Type
          | TInt  | TInt8  | TInt16  | TInt32  | TInt64
          | TWord | TWord8 | TWord16 | TWord32 | TWord64
          | TFloat | TDouble | TChar | TBool
          -- C types (rather annoying):
          | TCFloat  | TCDouble 
          | TCShort  | TCInt   | TCLong  | TCLLong
          | TCUShort | TCUInt  | TCULong | TCULLong
          | TCChar   | TCSChar | TCUChar 
 deriving (Read,Show,Eq,Generic)

isIntType ty =
  case ty of {
    TInt  ->t;     TInt8 ->t;    TInt16  ->t;  TInt32  ->t;  TInt64 ->t; 
    TCShort  ->t;  TCInt   ->t;  TCLong  ->t;  TCLLong ->t; 
    TCUShort ->t;  TCUInt  ->t;  TCULong ->t;  TCULLong ->t;
     _ -> False
  }
 where t = True

isFloatType ty = 
  case ty of {
    TFloat  ->t; TDouble ->t; 
    TCFloat ->t; TCDouble ->t;
    _ -> False  
  }
 where t = True

--------------------------------------------------------------------------------
-- Accelerate Array-level Expressions
--------------------------------------------------------------------------------

-- | Array-level expressions
data AExp = 
    Vr Var -- Array variable bound by a Let.
  | Unit Exp -- Turn an element into a singleton array
  | Let  Var Type AExp AExp    -- Let Var Type RHS Body
    -- Let is used for common subexpression elimination
  | LetPair (Var, Var) (Type,Type) AExp AExp 
    -- This binds an array expression returning a PAIR.
    -- Let (Var1, Var2) (Type1, Type2) (PairArrays Array1 Array2) Body
  | PairArrays AExp AExp       -- PairArrays Array1 Array2
  | Apply AFun AExp            -- Function $ Argument
  | Cond Exp AExp AExp         -- Array level if statements
  | Use      String -- A REAL ARRAY GOES HERE! -- TEMP - FIXME
  | Generate Type Exp Fun
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
  | Permute  Fun AExp Fun AExp -- Permute CombineFun DefaultArr PermFun SourceArray
  | Backpermute Exp Fun AExp   -- Backpermute ResultDimension   PermFun SourceArray
  | Reshape     Exp     AExp   -- Reshape Shape Array
  | Stencil  Fun Boundary AExp
  | Stencil2 Fun Boundary AExp Boundary AExp -- Two source arrays/boundaries
 deriving (Read,Show,Eq,Generic)

data AFun = ALam [(Var,Type)] AExp
 deriving (Read,Show,Eq,Generic)

-- | Boundary condition specification for stencil operations.
data Boundary = Clamp               -- ^clamp coordinates to the extent of the array
              | Mirror              -- ^mirror coordinates beyond the array extent
              | Wrap                -- ^wrap coordinates around on each dimension
              | Constant Const      -- ^use a constant value for outlying coordinates 
 deriving (Read,Show,Eq,Generic)
          
--------------------------------------------------------------------------------
-- Accelerate Scalar Expressions and Functions
--------------------------------------------------------------------------------

-- | Scalar functions
data Fun = Lam [(Var,Type)] Exp
 deriving (Read,Show,Eq,Generic)

-- | Scalar expressions
data Exp = 
    EVr Var -- Variable bound by a Let.
  | ELet Var Type Exp Exp    -- ELet Var Type RHS Body
  -- ELet is used for common subexpression elimination
  | EPrimApp Prim [Exp]  -- *Any* primitive scalar function
  | ETuple [Exp]
  | EConst Const
   -- [2012.04.02] I can't presently compute the length from the TupleIdx.
   --  | EPrj Int Int Exp  -- n m e : Project the nth field of an m-length tuple.
  | ETupProjectFromRight Int Exp  -- Project the nth field FROM THE RIGHT end of the tuple.  
  | EIndex [Exp] -- Index into a multi-dimensional array:
  | EIndexAny 
   -- I'm not sure I'm follwing this -- Accelerate would seem to allow run-time CONSING of indices:
  | EIndexConsDynamic Exp Exp
  | EIndexHeadDynamic Exp 
  | EIndexTailDynamic Exp 
   -- Conditional expression (non-strict in 2nd and 3rd argument):
  | ECond Exp Exp Exp
   -- Project a single scalar from an array
   -- the array expression can not contain any free scalar variables
  | EIndexScalar AExp Exp 
   -- Array shape
   -- the array expression can not contain any free scalar variables
  | EShape AExp
   -- Number of elements of a shape
  | EShapeSize Exp 
 deriving (Read,Show,Eq,Generic)


-- | Constants embedded within Accelerate programs.
data Const = I Int  | I8 Int8  | I16 Int16  | I32 Int32  | I64 Int64
           | W Word | W8 Word8 | W16 Word16 | W32 Word32 | W64 Word64
           | F Float | D Double | C Char | B Bool
           | Tup [Const]
            -- Special constants:
           | MinBound | MaxBound | Pi
            -- C types, rather annoying:
           | CF CFloat   | CD CDouble 
           | CS  CShort  | CI  CInt  | CL  CLong  | CLL  CLLong
           | CUS CUShort | CUI CUInt | CUL CULong | CULL CULLong
           | CC  CChar   | CSC CSChar | CUC CUChar 
 deriving (Read,Show,Eq,Generic)

--------------------------------------------------------------------------------
-- Accelerate Primitive Operations
--------------------------------------------------------------------------------

-- | A type that includes all primitives supported by Accelerate.
data Prim = NP NumPrim
          | IP IntPrim
          | FP FloatPrim
          | SP ScalarPrim
          | BP BoolPrim
          | OP OtherPrim
  deriving (Read,Show,Eq,Generic)
          
-- | Primitives that operate on /all/ numeric types.
--   Neg/Abs/Sig are unary:
data NumPrim = Add | Mul | Neg | Abs | Sig
  deriving (Read,Show,Eq,Generic)

-- | Primitive integral-only operations.
-- All binops except BNot, shifts and rotates take an Int constant as second arg:
data IntPrim = Quot | Rem | IDiv | Mod | 
               BAnd | BOr | BXor | BNot | BShiftL | BShiftR | BRotateL | BRotateR
  deriving (Read,Show,Eq,Generic)

-- | Primitive floating point-only operations.
data FloatPrim = 
      -- Unary:
      Recip | Sin | Cos | Tan | Asin | Acos | Atan | Asinh | Acosh | Atanh | ExpFloating | Sqrt | Log |
      -- Binary:                  
      FDiv | FPow | LogBase | Atan2 | Truncate | Round | Floor | Ceiling
  deriving (Read,Show,Eq,Generic)
           
-- | Relational and equality operators
data ScalarPrim = Lt | Gt | LtEq | GtEq | Eq | NEq | Max | Min
  deriving (Read,Show,Eq,Generic)

data BoolPrim = And | Or | Not
  deriving (Read,Show,Eq,Generic)

data OtherPrim = Ord | Chr | BoolToInt | FromIntegral
  deriving (Read,Show,Eq,Generic)


--------------------------------------------------------------------------------
-- Boilerplate for generic pretty printing:

instance Out Type
instance Out Fun
instance Out Exp
instance Out AExp
instance Out AFun
instance Out Const
instance Out Prim
instance Out NumPrim
instance Out IntPrim
instance Out FloatPrim
instance Out ScalarPrim
instance Out BoolPrim
instance Out OtherPrim
instance Out Boundary

instance Out Symbol where docPrec _ = text . show; doc = docPrec 0 
instance Out Int8   where docPrec _ = text . show; doc = docPrec 0 
instance Out Int16  where docPrec _ = text . show; doc = docPrec 0
instance Out Int32  where docPrec _ = text . show; doc = docPrec 0 
instance Out Int64  where docPrec _ = text . show; doc = docPrec 0
instance Out Word   where docPrec _ = text . show; doc = docPrec 0 
instance Out Word8  where docPrec _ = text . show; doc = docPrec 0 
instance Out Word16 where docPrec _ = text . show; doc = docPrec 0
instance Out Word32 where docPrec _ = text . show; doc = docPrec 0 
instance Out Word64 where docPrec _ = text . show; doc = docPrec 0
instance Out CFloat  where docPrec _ = text . show; doc = docPrec 0 
instance Out CDouble where docPrec _ = text . show; doc = docPrec 0 
instance Out CShort  where docPrec _ = text . show; doc = docPrec 0
instance Out CInt    where docPrec _ = text . show; doc = docPrec 0 
instance Out CLong   where docPrec _ = text . show; doc = docPrec 0                          
instance Out CLLong  where docPrec _ = text . show; doc = docPrec 0 
instance Out CUShort where docPrec _ = text . show; doc = docPrec 0
instance Out CUInt   where docPrec _ = text . show; doc = docPrec 0 
instance Out CULong  where docPrec _ = text . show; doc = docPrec 0
instance Out CULLong where docPrec _ = text . show; doc = docPrec 0 
instance Out CChar   where docPrec _ = text . show; doc = docPrec 0 
instance Out CSChar  where docPrec _ = text . show; doc = docPrec 0
instance Out CUChar  where docPrec _ = text . show; doc = docPrec 0 
