{-# LANGUAGE GADTs, FlexibleInstances, PatternGuards, TypeOperators #-}

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
import Data.Array.Accelerate.AST


-- |Show instances
-- ---------------

instance Show Comps where
  show cs = render $ prettyComps cs

instance Show CompBinding where
  show c = render $ prettyCompBinding c

instance Show (Comp a) where
  show c = render $ prettyComp c

instance Show (OpenFun env f) where
  show f = render $ prettyFun f

instance Show (OpenExp env t) where
  show e = render $ prettyExp noParens e


-- |Pretty printing
-- ----------------

-- |Pretty print a sequence of collective operations.
--
prettyComps :: Comps -> Doc
prettyComps (Comps [])  = text "<empty>"
prettyComps (Comps [c]) = prettyCompReturn c
prettyComps (Comps cs)  =
  hang (text "do") 2 $
    vcat (map prettyCompBinding binds ++ [prettyCompReturn ret])
  where
    binds = init cs
    ret   = last cs

-- |Pretty print just the collective operation of a binding.
--
prettyCompReturn :: CompBinding -> Doc
prettyCompReturn (CompBinding _ coll) = prettyComp coll

-- |Pretty print a binding of a collective operation.
--
prettyCompBinding :: CompBinding -> Doc
prettyCompBinding (CompBinding r coll) 
  = prettyVarNames (strings r) <+> text "<-" <+> prettyComp coll
  where
    prettyVarNames []     = text "()"
    prettyVarNames [name] = text name
    prettyVarNames names  = 
      parens . hsep . punctuate (char ',') . map text $ names

-- |Pretty print a collective array computation.
--
prettyComp :: Comp a -> Doc
prettyComp (Use arr)          = text "use" <+> prettyArray arr
prettyComp (Unit e)           = text "unit" <+> prettyExp parens e
prettyComp (Reshape sh arr)
  = text "reshape" <+> prettyExp parens sh <+> prettyArr arr
prettyComp (Replicate ix arr) 
  = text "replicate" <+> prettyIndex ix <+> prettyArr arr
prettyComp (Index arr ix) 
  = prettyArr arr <> char '!' <> prettyIndex ix
prettyComp (Zip arr1 arr2)    = text "zip" <+> prettyArr arr1 <+> prettyArr arr2
prettyComp (Map f arr)      
  = text "map" <+> parens (prettyFun f) <+> prettyArr arr
prettyComp (Filter p arr)   
  = text "filter" <+> parens (prettyFun p) <+> prettyArr arr
prettyComp (Scan f e arr)   
  = text "scan" <+> parens (prettyFun f) <+> prettyExp parens e <+> 
    prettyArr arr
prettyComp (Permute f dfts p arr) 
  = text "permute" <+> parens (prettyFun f) <+> prettyArr dfts <+> 
    parens (prettyFun p) <+> prettyArr arr
prettyComp (Backpermute sh p arr) 
  = text "backpermute" <+> prettyExp parens sh <+> parens (prettyFun p) <+> 
    prettyArr arr

-- |Pretty print a function over scalar expressions.
--
prettyFun :: OpenFun env fun -> Doc
prettyFun fun = 
  let (n, bodyDoc) = count fun
  in
  char '\\' <> hsep [text $ "a" ++ show idx | idx <- [0..n]] <+> text "->" <+> 
  bodyDoc
  where
     count :: OpenFun env fun -> (Int, Doc)
     count (Body body) = (-1, prettyExp noParens body)
     count (Lam fun)   = let (n, body) = count fun in (1 + n, body)

-- |Pretty print an expression.
--
-- * Apply the wrapping combinator (1st argument) to any compound expressions.
--
prettyExp :: (Doc -> Doc) -> OpenExp env t -> Doc
--prettyExp wrap (Arg ty)      = wrap $ text "arg ::" <+> prettyAnyType ty
prettyExp wrap (Var _ idx)       = text $ "a" ++ show (count idx)
  where
    count :: Idx env t -> Int
    count ZeroIdx       = 0
    count (SuccIdx idx) = 1 + count idx
prettyExp _    (Const ty v)      = text $ runShow ty v
prettyExp _    (Pair e1 e2)      = prettyTuple (Pair e1 e2)
prettyExp wrap (Fst e)           = wrap $ text "fst" <+> prettyExp parens e
prettyExp wrap (Snd e)           = wrap $ text "snd" <+> prettyExp parens e
prettyExp wrap (Cond c t e) 
  = wrap $ sep [prettyExp parens c <+> char '?', prettyExp noParens (Pair t e)]
prettyExp _    (PrimConst a)     = prettyConst a
prettyExp wrap (PrimApp p a)     = wrap $ prettyPrim p <+> prettyExp parens a
prettyExp wrap (IndexScalar a i)
  = wrap $ cat [prettyArr a, char '!', prettyExp parens i]
prettyExp wrap (Shape a)         = wrap $ text "shape" <+> prettyArr a

-- |Pretty print nested pairs as a proper tuple.
--
prettyTuple :: OpenExp env t -> Doc
prettyTuple e = parens $ sep (map (<> comma) (init es) ++ [last es])
  where
    es = collect e
    --
    collect :: OpenExp env t -> [Doc]
    collect (Pair e1 e2) = collect e1 ++ collect e2
    collect e            = [prettyExp noParens e]

-- |Pretty print a primitive constant
--
prettyConst :: PrimConst a -> Doc
prettyConst (PrimMinBound _) = text "minBound"
prettyConst (PrimMaxBound _) = text "maxBound"
prettyConst (PrimPi       _) = text "pi"

-- |Pretty print a primitive operation
--
prettyPrim :: PrimFun a -> Doc
prettyPrim (PrimAdd _)   = text "(+)"
prettyPrim (PrimSub _)   = text "(-)"
prettyPrim (PrimMul _)   = text "(*)"
prettyPrim (PrimNeg _)   = text "negate"
prettyPrim (PrimAbs _)   = text "abs"
prettyPrim (PrimSig _)   = text "signum"
prettyPrim (PrimQuot _)  = text "quot"
prettyPrim (PrimRem _)   = text "rem"
prettyPrim (PrimIDiv _)  = text "div"
prettyPrim (PrimMod _)   = text "mod"
prettyPrim (PrimBAnd _)  = text "(.&.)"
prettyPrim (PrimBOr _)   = text "(.|.)"
prettyPrim (PrimBXor _)  = text "xor"
prettyPrim (PrimBNot _)  = text "complement"
prettyPrim (PrimFDiv _)  = text "(/)"
prettyPrim (PrimRecip _) = text "recip"
prettyPrim (PrimLt _)    = text "(<*)"
prettyPrim (PrimGt _)    = text "(>*)"
prettyPrim (PrimLtEq _)  = text "(<=*)"
prettyPrim (PrimGtEq _)  = text "(>=*)"
prettyPrim (PrimEq _)    = text "(==*)"
prettyPrim (PrimNEq _)   = text "(/=*)"
prettyPrim (PrimMax _)   = text "max"
prettyPrim (PrimMin _)   = text "min"
prettyPrim PrimLAnd      = text "&&*"
prettyPrim PrimLOr       = text "||*"
prettyPrim PrimLNot      = text "not"

-- |Pretty print type
--
prettyAnyType :: ScalarType a -> Doc
prettyAnyType ty = text $ show ty

-- |Pretty print the identification code of an APU array
--
prettyArray :: Array dim a -> Doc
prettyArray arr = text $ arrayId arr

-- |Pretty print a generalised array index
--
prettyIndex :: Index initial projected -> Doc
prettyIndex = parens . hsep . punctuate (char ',') . prettyIxs
  where
    prettyIxs :: Index initial projected -> [Doc]
    prettyIxs IndexNil           = [empty]
    prettyIxs (IndexAll ixs)     = char '.' : prettyIxs ixs
    prettyIxs (IndexFixed e ixs) = prettyExp noParens e : prettyIxs ixs

-- |Pretty print the identification of an array representation in collective code
--
prettyArr :: Arr dim a -> Doc
prettyArr (Arr _ str) = text str

-- |Auxilliary pretty printing combinators
-- -

noParens :: Doc -> Doc
noParens = id

-- |Auxilliary dictionary operations
-- -

-- |Show scalar values
--
runShow :: ScalarType a -> (a -> String)
runShow (NumScalarType (IntegralNumType ty)) 
  | IntegralDict <- integralDict ty = show
runShow (NumScalarType (FloatingNumType ty)) 
  | FloatingDict <- floatingDict ty = show
runShow (NonNumScalarType ty)       
  | NonNumDict   <- nonNumDict ty   = show
