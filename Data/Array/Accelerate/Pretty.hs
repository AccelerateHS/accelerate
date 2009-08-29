{-# LANGUAGE GADTs, FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Embedded array processing language: pretty printing
--
--  Copyright (c) 2009 Manuel M T Chakravarty, Gabriele Keller, Sean Lee
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--

module Data.Array.Accelerate.Pretty (

  -- * Instances of Show

) where

-- standard libraries
import Text.PrettyPrint

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.AST


-- |Show instances
-- ---------------

instance Show (OpenAcc aenv a) where
  show c = render $ prettyAcc 0 c

instance Show (OpenFun env aenv f) where
  show f = render $ prettyFun 0 f

instance Show (OpenExp env aenv t) where
  show e = render $ prettyExp 0 noParens e


-- Pretty printing
-- ---------------

-- Pretty print an array expression
--
prettyAcc :: Int -> OpenAcc aenv a -> Doc
prettyAcc lvl (Let acc1 acc2) 
  = text "let a" <> int lvl <+> char '=' <+> prettyAcc lvl acc1 <+>
    text " in " <+> prettyAcc (lvl + 1) acc2
prettyAcc lvl (Let2 acc1 acc2) 
  = text "let (a" <> int lvl <> text ", a" <> int (lvl + 1) <+> char '=' <+>
    prettyAcc lvl acc1 <+>
    text " in " <+> prettyAcc (lvl + 2) acc2
prettyAcc _   (Avar idx)       = text $ "a" ++ show (idxToInt idx)
prettyAcc _   (Use arr)        = prettyArrOp "use" [prettyArray arr]
prettyAcc lvl (Unit e)         = prettyArrOp "unit" [prettyExp lvl parens e]
prettyAcc lvl (Reshape sh acc)
  = prettyArrOp "reshape" [prettyExp lvl parens sh, prettyAccParens lvl acc]
prettyAcc lvl (Replicate _ty ix acc) 
  = prettyArrOp "replicate" [prettyExp lvl id ix, prettyAccParens lvl acc]
prettyAcc lvl (Index _ty acc ix) 
  = sep [prettyAccParens lvl acc, char '!', prettyExp lvl id ix]
prettyAcc lvl (Map f acc)
  = prettyArrOp "map" [parens (prettyFun lvl f), prettyAccParens lvl acc]
prettyAcc lvl (ZipWith f acc1 acc2)    
  = prettyArrOp "zipWith"
      [parens (prettyFun lvl f), prettyAccParens lvl acc1, 
       prettyAccParens lvl acc2]
prettyAcc lvl (Fold f e acc)   
  = prettyArrOp "fold" [parens (prettyFun lvl f), prettyExp lvl parens e,
                        prettyAccParens lvl acc]
prettyAcc lvl (Scan f e acc)   
  = prettyArrOp "scan" [parens (prettyFun lvl f), prettyExp lvl parens e,
                        prettyAccParens lvl acc]
prettyAcc lvl (Permute f dfts p acc) 
  = prettyArrOp "permute" [parens (prettyFun lvl f), prettyAccParens lvl dfts,
                           parens (prettyFun lvl p), prettyAccParens lvl acc]
prettyAcc lvl (Backpermute sh p acc) 
  = prettyArrOp "backpermute" [prettyExp lvl parens sh,
                               parens (prettyFun lvl p),
                               prettyAccParens lvl acc]
    
prettyArrOp :: String -> [Doc] -> Doc
prettyArrOp name docs = hang (text name) 2 $ sep docs

-- Wrap into parenthesis
--    
prettyAccParens :: Int -> OpenAcc aenv a -> Doc
prettyAccParens lvl acc@(Avar _) = prettyAcc lvl acc
prettyAccParens lvl acc          = parens (prettyAcc lvl acc)

-- Pretty print a function over scalar expressions.
--
prettyFun :: Int -> OpenFun env aenv fun -> Doc
prettyFun lvl fun = 
  let (n, bodyDoc) = count fun
  in
  char '\\' <> hsep [text $ "x" ++ show idx | idx <- [0..n]] <+> text "->" <+> 
  bodyDoc
  where
     count :: OpenFun env aenv fun -> (Int, Doc)
     count (Body body) = (-1, prettyExp lvl noParens body)
     count (Lam fun)   = let (n, body) = count fun in (1 + n, body)

-- Pretty print an expression.
--
-- * Apply the wrapping combinator (1st argument) to any compound expressions.
--
prettyExp :: forall t env aenv. 
             Int -> (Doc -> Doc) -> OpenExp env aenv t -> Doc
prettyExp _   _    (Var idx)         = text $ "x" ++ show (idxToInt idx)
prettyExp _   _    (Const v)         = text $ show (toElem v :: t)
prettyExp lvl _    e@(Pair _ _)      = prettyTuple lvl e
prettyExp lvl wrap (Fst e)       
  = wrap $ text "fst" <+> prettyExp lvl parens e
prettyExp lvl wrap (Snd e)       
  = wrap $ text "snd" <+> prettyExp lvl parens e
prettyExp lvl wrap (Cond c t e) 
  = wrap $ sep [prettyExp lvl parens c <+> char '?', 
                parens (prettyExp lvl noParens t <> comma <+> 
                        prettyExp lvl noParens e)]
prettyExp _   _    (PrimConst a)     = prettyConst a
prettyExp lvl wrap (PrimApp p a)     
  = wrap $ prettyPrim p <+> prettyExp lvl parens a
prettyExp lvl wrap (IndexScalar idx i)
  = wrap $ cat [prettyAccParens lvl idx, char '!', prettyExp lvl parens i]
prettyExp lvl wrap (Shape idx)       = wrap $ text "shape" <+> prettyAccParens lvl idx

-- Pretty print nested pairs as a proper tuple.
--
prettyTuple :: Int -> OpenExp env aenv t -> Doc
prettyTuple lvl e = parens $ sep (map (<> comma) (init es) ++ [last es])
  where
    es = collect e
    --
    collect :: OpenExp env aenv t -> [Doc]
    collect (Pair e1 e2) = collect e1 ++ collect e2
    collect e            = [prettyExp lvl noParens e]

-- Pretty print a primitive constant
--
prettyConst :: PrimConst a -> Doc
prettyConst (PrimMinBound _) = text "minBound"
prettyConst (PrimMaxBound _) = text "maxBound"
prettyConst (PrimPi       _) = text "pi"

-- Pretty print a primitive operation
--
prettyPrim :: PrimFun a -> Doc
prettyPrim (PrimAdd _)       = text "(+)"
prettyPrim (PrimSub _)       = text "(-)"
prettyPrim (PrimMul _)       = text "(*)"
prettyPrim (PrimNeg _)       = text "negate"
prettyPrim (PrimAbs _)       = text "abs"
prettyPrim (PrimSig _)       = text "signum"
prettyPrim (PrimQuot _)      = text "quot"
prettyPrim (PrimRem _)       = text "rem"
prettyPrim (PrimIDiv _)      = text "div"
prettyPrim (PrimMod _)       = text "mod"
prettyPrim (PrimBAnd _)      = text "(.&.)"
prettyPrim (PrimBOr _)       = text "(.|.)"
prettyPrim (PrimBXor _)      = text "xor"
prettyPrim (PrimBNot _)      = text "complement"
prettyPrim (PrimFDiv _)      = text "(/)"
prettyPrim (PrimRecip _)     = text "recip"
prettyPrim (PrimLt _)        = text "(<*)"
prettyPrim (PrimGt _)        = text "(>*)"
prettyPrim (PrimLtEq _)      = text "(<=*)"
prettyPrim (PrimGtEq _)      = text "(>=*)"
prettyPrim (PrimEq _)        = text "(==*)"
prettyPrim (PrimNEq _)       = text "(/=*)"
prettyPrim (PrimMax _)       = text "max"
prettyPrim (PrimMin _)       = text "min"
prettyPrim PrimLAnd          = text "&&*"
prettyPrim PrimLOr           = text "||*"
prettyPrim PrimLNot          = text "not"
prettyPrim PrimOrd           = text "ord"
prettyPrim PrimChr           = text "chr"
prettyPrim PrimRoundFloatInt = text "round"
prettyPrim PrimTruncFloatInt = text "trunc"
prettyPrim PrimIntFloat      = text "intFloat"
prettyPrim PrimBoolToInt     = text "boolToInt"

{-
-- Pretty print type
--
prettyAnyType :: ScalarType a -> Doc
prettyAnyType ty = text $ show ty
-}

prettyArray :: forall dim a. Array dim a -> Doc
prettyArray arr@(Array sh _) 
  = hang (text "Array") 2 $
      sep [showDoc $ (toElem sh :: dim), dataDoc]
  where
    showDoc :: forall a. Show a => a -> Doc
    showDoc = text . show
    l       = toList arr
    dataDoc | length l <= 1000 = showDoc l
            | otherwise        = showDoc (take 1000 l) <+> 
                                 text "{truncated at 1000 elements}"


-- Auxilliary pretty printing combinators
-- 

noParens :: Doc -> Doc
noParens = id

-- Auxilliary ops
--

-- Convert a typed de Brujin index to the corresponding integer
--
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx

-- Auxilliary dictionary operations
-- 

{-
-- Show scalar values
--
runScalarShow :: ScalarType a -> (a -> String)
runScalarShow (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = show
runScalarShow (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = show
runScalarShow (NonNumScalarType ty)       
  | NonNumDict   <- nonNumDict ty   = show
-}
