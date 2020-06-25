{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern
-- Copyright   : [2018..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern (

  pattern Pattern,
  pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6,
  pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11,
  pattern T12, pattern T13, pattern T14, pattern T15, pattern T16,

  pattern Z_, pattern Ix, pattern (::.),
  pattern I0, pattern I1, pattern I2, pattern I3, pattern I4,
  pattern I5, pattern I6, pattern I7, pattern I8, pattern I9,

  pattern V2, pattern V3, pattern V4, pattern V8, pattern V16,

  mkPatterns,

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Representation.Vec
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type
import Data.Primitive.Vec

import Control.Monad
import Data.Bits
import Data.List                                                    ( foldl' )
import Language.Haskell.TH                                          hiding ( Exp, Match, match, tupP, tupE )
import Language.Haskell.TH.Extra
import qualified Language.Haskell.TH                                as TH


-- | A pattern synonym for working with (product) data types. You can declare
-- your own pattern synonyms based off of this.
--
pattern Pattern :: forall b a context. IsPattern context a b => b -> context a
pattern Pattern vars <- (destruct @context -> vars)
  where Pattern = construct @context

class IsPattern con a t where
  construct :: t -> con a
  destruct  :: con a -> t

-- | Pattern synonyms for indices, which may be more convenient to use than
-- 'Data.Array.Accelerate.Lift.lift' and
-- 'Data.Array.Accelerate.Lift.unlift'.
--
pattern Z_ :: Exp DIM0
pattern Z_ = Pattern Z
{-# COMPLETE Z_ #-}

infixl 3 ::.
pattern (::.) :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a ::. b = Pattern (a :. b)
{-# COMPLETE (::.) #-}

infixl 3 `Ix`
pattern Ix :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a :. b)
pattern a `Ix` b = a ::. b
{-# COMPLETE Ix #-}

-- IsPattern instances for Shape nil and cons
--
instance IsPattern Exp Z Z where
  construct _ = constant Z
  destruct _  = Z

instance (Elt a, Elt b) => IsPattern Exp (a :. b) (Exp a :. Exp b) where
  construct (Exp a :. Exp b) = Exp $ SmartExp $ Pair a b
  destruct (Exp t)           = Exp (SmartExp $ Prj PairIdxLeft t) :. Exp (SmartExp $ Prj PairIdxRight t)

-- Newtype wrapper to distinguish between T and V patterns
--
newtype VecPattern a = VecPattern a


mkPatterns :: Name -> DecsQ
mkPatterns nm = do
  info <- reify nm
  case info of
    TyConI dec -> mkDec dec
    _          -> fail "mkPatterns: expected the name of a newtype or datatype"

mkDec :: Dec -> DecsQ
mkDec dec =
  case dec of
    DataD    _ nm tv _ cs _ -> mkDataD nm tv cs
    NewtypeD _ nm tv _ c  _ -> mkNewtypeD nm tv c
    _                      -> fail "mkPatterns: expected the name of a newtype or datatype"

mkNewtypeD :: Name -> [TyVarBndr] -> Con -> DecsQ
mkNewtypeD tn tvs c = mkDataD tn tvs [c]

mkDataD :: Name -> [TyVarBndr] -> [Con] -> DecsQ
mkDataD tn tvs cs = do
  (pats, decs) <- unzip <$> go [] fts cs cts
  comp         <- pragCompleteD pats Nothing
  return $ comp : concat decs
  where
    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = error "mkPatterns: only constructors for \"vanilla\" syntax are supported"

    st  = length cs > 1
    fts = map fieldTys cs

    -- TODO: The GTags class demonstrates a way to generate the tags for
    -- a given constructor, rather than backwards-engineering the structure
    -- as we've done here. We should use that instead!
    --
    cts =
      let n = length cs
          m = n `quot` 2
          l = take m     (iterate (True:) [False])
          r = take (n-m) (iterate (True:) [True])
      in
      map bitsToTag (l ++ r)

    bitsToTag = foldl' f 0
      where
        f n False =         n `shiftL` 1
        f n True  = setBit (n `shiftL` 1) 0

    go prev (this:next) (con:cons) (tag:tags) = do
      r  <- mkCon st tn tvs prev next tag con
      rs <- go (this:prev) next cons tags
      return (r : rs)
    go _ [] [] [] = return []
    go _ _  _  _  = fail "mkPatterns: unexpected error"

mkCon :: Bool -> Name -> [TyVarBndr] -> [[Type]] -> [[Type]] -> Word8 -> Con -> Q (Name, [Dec])
mkCon st tn tvs prev next tag = \case
  NormalC nm fs -> mkNormalC st tn (map tyVarBndrName tvs) tag nm prev (map snd fs) next
  -- RecC nm fs    -> undefined
  -- InfixC a nm b -> undefined
  _             -> fail "mkPatterns: only constructors for \"vanilla\" syntax are supported"

mkNormalC :: Bool -> Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC st tn tvs tag cn ps fs ns = do
  (fun_mk,    dec_mk)    <- mkNormalC_mk st tn tvs tag cn ps fs ns
  (fun_match, dec_match) <- mkNormalC_match st tn tvs tag cn ps fs ns
  (pat,       dec_pat)   <- mkNormalC_pattern tn tvs cn fs fun_mk fun_match
  return $ (pat, concat [dec_pat, dec_mk, dec_match])

mkNormalC_pattern :: Name -> [Name] -> Name -> [Type] -> Name -> Name -> Q (Name, [Dec])
mkNormalC_pattern tn tvs cn fs mk match = do
  xs <- replicateM (length fs) (newName "_x")
  r  <- sequence [ patSynSigD pat sig
                 , patSynD    pat
                     (prefixPatSyn xs)
                     (explBidir [clause [] (normalB (varE mk)) []])
                     (parensP $ viewP (varE match) [p| Just $(tupP (map varP xs)) |])
                 ]
  return (pat, r)
  where
    pat = mkName (nameBase cn ++ "_")
    sig = forallT
            (map plainTV tvs)
            (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
            (foldr (\t ts -> [t| $t -> $ts |])
                   [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                   (map (\t -> [t| Exp $(return t) |]) fs))

mkNormalC_mk :: Bool -> Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC_mk sum_type tn tvs tag cn fs0 fs fs1 = do
  fun <- newName ("_mk" ++ nameBase cn)
  xs  <- replicateM (length fs) (newName "_x")
  let
    vs    = foldl' (\es e -> [| SmartExp ($es `Pair` $e) |]) [| SmartExp Nil |]
          $  map (\t -> [| unExp (undef @ $(return t)) |] ) (concat (reverse fs0))
          ++ map varE xs
          ++ map (\t -> [| unExp (undef @ $(return t)) |] ) (concat fs1)

    body  = clause (map (\x -> [p| (Exp $(varP x)) |]) xs) (normalB tagged) []
      where
        tagged
          | sum_type  = [| Exp $ SmartExp $ Pair (SmartExp (Const (SingleScalarType (NumSingleType (IntegralNumType TypeWord8))) $(litE (IntegerL (toInteger tag))))) $vs |]
          | otherwise = [| Exp $vs |]

  r <- sequence [ sigD fun sig
                , funD fun [body]
                ]
  return (fun, r)
  where
    sig   = forallT
              (map plainTV tvs)
              (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
              (foldr (\t ts -> [t| $t -> $ts |])
                     [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                     (map (\t -> [t| Exp $(return t) |]) fs))


mkNormalC_match :: Bool -> Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC_match sum_type tn tvs tag cn fs0 fs fs1 = do
  fun     <- newName ("_match" ++ nameBase cn)
  e       <- newName "_e"
  x       <- newName "_x"
  (ps,es) <- extract vs (if sum_type then [| Prj PairIdxRight $(varE x) |] else varE x) [] []
  let
    lhs   = [p| (Exp $(varP e)) |]
    body  = normalB $ caseE (varE e)
      [ TH.match (conP 'SmartExp [(conP 'Match [matchP ps, varP x])]) (normalB [| Just $(tupE es) |]) []
      , TH.match (conP 'SmartExp [(recP 'Match [])])                  (normalB [| Nothing         |]) []
      , TH.match wildP                                                (normalB [| error "Pattern synonym used outside 'match' context" |]) []
      ]

  r <- sequence [ sigD fun sig
                , funD fun [clause [lhs] body []]
                ]
  return (fun, r)
  where
    sig =
      forallT []
        (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
        [t| Exp $(foldl' appT (conT tn) (map varT tvs))
            -> Maybe $(tupT (map (\t -> [t| Exp $(return t) |]) fs)) |]

    matchP us
      | sum_type  = [p| TagRtag $(litP (IntegerL (toInteger tag))) $pat |]
      | otherwise = pat
      where
        pat = [p| $(foldl (\ps p -> [p| TagRpair $ps $p |]) [p| TagRunit |] us) |]

    extract []     _ ps es = return (ps, es)
    extract (u:us) x ps es = do
      _u <- newName "_u"
      let x' = [| Prj PairIdxLeft (SmartExp $x) |]
      if not u
         then extract us x' (wildP:ps)  es
         else extract us x' (varP _u:ps) ([| Exp (SmartExp (Match $(varE _u) (SmartExp (Prj PairIdxRight (SmartExp $x))))) |] : es)

    vs = reverse
       $ [ False | _ <- concat fs0 ] ++ [ True | _ <- fs ] ++ [ False | _ <- concat fs1 ]


-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of the
-- (unremarkable) boilerplate for us, but since the implementation is a little
-- tricky it is debatable whether or not this is a good idea...
--
runQ $ do
    let
        -- Generate instance declarations for IsPattern of the form:
        -- instance (Elt x, EltR x ~ (((), EltR a), EltR b), Elt a, Elt b,) => IsPattern Exp x (Exp a, Exp b)
        mkIsPattern :: Name -> TypeQ -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> Int -> Q [Dec]
        mkIsPattern con cst repr smart prj nil pair n = do
          a <- newName "a"
          let
              -- Type variables for the elements
              xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              -- Last argument to `IsPattern`, eg (Exp, a, Exp b) in the example
              b        = tupT (map (\t -> [t| $(conT con) $(varT t)|]) xs)
              -- Representation as snoc-list of pairs, eg (((), EltR a), EltR b)
              snoc     = foldl (\sn t -> [t| ($sn, $(appT repr $ varT t)) |]) [t| () |] xs
              -- Constraints for the type class, consisting of Elt constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the snoc representation `snoc`.
              context  = tupT
                       $ appT cst [t| $(varT a) |]
                       : [t| $repr $(varT a) ~ $snoc |]
                       : map (\t -> [t| $cst $(varT t)|]) xs
              --
              get x 0 = [| $(conE con) ($smart ($prj PairIdxRight $x)) |]
              get x i = get [| $smart ($prj PairIdxLeft $x) |] (i-1)
          --
          _x <- newName "_x"
          [d| instance $context => IsPattern $(conT con) $(varT a) $b where
                construct $(tupP (map (conP con . return . varP) xs)) =
                  $(conE con) $(foldl (\vs v -> appE smart (appE (appE pair vs) (varE v))) (appE smart nil) xs)
                destruct $(conP con [varP _x]) =
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        mkVecPattern :: Int -> Q [Dec]
        mkVecPattern n = do
          a <- newName "a"
          let
              v = foldr appE [| VecRnil (singleType @(EltR $(varT a))) |] (replicate n [| VecRsucc |])
              r = tupT (replicate n [t| Exp $(varT a) |])
              t = tupT (replicate n (varT a))
          --
          [d| instance VecElt $(varT a) => IsPattern Exp (Vec $(litT (numTyLit (fromIntegral n))) $(varT a)) (VecPattern $r) where
                construct (VecPattern x) =
                  case construct x :: Exp $t of
                    Exp x' -> Exp (SmartExp (VecPack $v x'))
                destruct (Exp x) = VecPattern (destruct (Exp (SmartExp (VecUnpack $v x)) :: Exp $t))
            |]

        mkExpPattern = mkIsPattern (mkName "Exp") [t| Elt    |] [t| EltR    |] [| SmartExp |] [| Prj  |] [| Nil  |] [| Pair  |]
        mkAccPattern = mkIsPattern (mkName "Acc") [t| Arrays |] [t| ArraysR |] [| SmartAcc |] [| Aprj |] [| Anil |] [| Apair |]
    --
    es <- mapM mkExpPattern [0..16]
    as <- mapM mkAccPattern [0..16]
    vs <- mapM mkVecPattern [2,3,4,8,16]
    return $ concat (es ++ as ++ vs)


-- | Specialised pattern synonyms for tuples, which may be more convenient to
-- use than 'Data.Array.Accelerate.Lift.lift' and
-- 'Data.Array.Accelerate.Lift.unlift'. For example, to construct a pair:
--
-- > let a = 4        :: Exp Int
-- > let b = 2        :: Exp Float
-- > let c = T2 a b   -- :: Exp (Int, Float); equivalent to 'lift (a,b)'
--
-- Similarly they can be used to destruct values:
--
-- > let T2 x y = c   -- x :: Exp Int, y :: Exp Float; equivalent to 'let (x,y) = unlift c'
--
-- These pattern synonyms can be used for both 'Exp' and 'Acc' terms.
--
-- Similarly, we have patterns for constructing and destructing indices of
-- a given dimensionality:
--
-- > let ix = Ix 2 3    -- :: Exp DIM2
-- > let I2 y x = ix    -- y :: Exp Int, x :: Exp Int
--
runQ $ do
    let
        mkT :: Int -> Q [Dec]
        mkT n =
          let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = map varT xs
              name  = mkName ('T':show n)
              con   = varT (mkName "con")
              ty1   = tupT ts
              ty2   = tupT (map (con `appT`) ts)
              sig   = foldr (\t r -> [t| $con $t -> $r |]) (appT con ty1) ts
          in
          sequence
            [ patSynSigD name [t| IsPattern $con $ty1 $ty2 => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir [p| Pattern $(tupP (map varP xs)) |]
            , pragCompleteD [name] (Just ''Acc)
            , pragCompleteD [name] (Just ''Exp)
            ]

        mkI :: Int -> Q [Dec]
        mkI n =
          let xs      = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts      = map varT xs
              name    = mkName ('I':show n)
              ix      = mkName "Ix"
              cst     = tupT (map (\t -> [t| Elt $t |]) ts)
              dim     = foldl (\h t -> [t| $h :. $t |]) [t| Z |] ts
              sig     = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp $dim |] ts
          in
          sequence
            [ patSynSigD name [t| $cst => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir (foldl (\ps p -> infixP ps ix (varP p)) [p| Z_ |] xs)
            , pragCompleteD [name] Nothing
            ]

        mkV :: Int -> Q [Dec]
        mkV n = do
          a <- newName "a"
          let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              ts    = replicate n (varT a)
              name  = mkName ('V':show n)
              sig   = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp (Vec $(litT (numTyLit (fromIntegral n))) $(varT a)) |] ts
          --
          sequence
            [ patSynSigD name [t| VecElt $(varT a) => $sig |]
            , patSynD    name (prefixPatSyn xs) implBidir [p| Pattern (VecPattern $(tupP (map varP xs))) |]
            , pragCompleteD [name] Nothing
            ]
    --
    ts <- mapM mkT [2..16]
    is <- mapM mkI [0..9]
    vs <- mapM mkV [2,3,4,8,16]
    return $ concat (ts ++ is ++ vs)

