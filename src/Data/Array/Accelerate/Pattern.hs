{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
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

  mkPattern,
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


mkPatterns :: [Name] -> DecsQ
mkPatterns nms = concat <$> mapM mkPattern nms

mkPattern :: Name -> DecsQ
mkPattern nm = do
  info <- reify nm
  case info of
    TyConI dec -> mkDec dec
    _          -> fail "mkPatterns: expected the name of a newtype or datatype"

mkDec :: Dec -> DecsQ
mkDec dec =
  case dec of
    DataD    _ nm tv _ cs _ -> mkDataD nm tv cs
    NewtypeD _ nm tv _ c  _ -> mkNewtypeD nm tv c
    _                       -> fail "mkPatterns: expected the name of a newtype or datatype"

mkNewtypeD :: Name -> [TyVarBndr] -> Con -> DecsQ
mkNewtypeD tn tvs c = mkDataD tn tvs [c]

mkDataD :: Name -> [TyVarBndr] -> [Con] -> DecsQ
mkDataD tn tvs cs = do
  (pats, decs) <- unzip <$> go cs
  comp         <- pragCompleteD pats Nothing
  return $ comp : concat decs
  where
    go []  = fail "mkPatterns: empty data declarations not supported"
    go [c] = return <$> mkConP tn tvs c
    go _   = go' [] (map fieldTys cs) ctags cs

    go' prev (this:next) (tag:tags) (con:cons) = do
      r  <- mkConS tn tvs prev next tag con
      rs <- go' (this:prev) next tags cons
      return (r : rs)
    go' _ [] [] [] = return []
    go' _ _  _  _  = fail "mkPatterns: unexpected error"

    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = error "mkPatterns: only constructors for \"vanilla\" syntax are supported"

    -- TODO: The GTags class demonstrates a way to generate the tags for
    -- a given constructor, rather than backwards-engineering the structure
    -- as we've done here. We should use that instead!
    --
    ctags =
      let n = length cs
          m = n `quot` 2
          l = take m     (iterate (True:) [False])
          r = take (n-m) (iterate (True:) [True])
          --
          bitsToTag = foldl' f 0
            where
              f i False =         i `shiftL` 1
              f i True  = setBit (i `shiftL` 1) 0
      in
      map bitsToTag (l ++ r)


mkConP :: Name -> [TyVarBndr] -> Con -> Q (Name, [Dec])
mkConP tn' tvs' = \case
  NormalC cn fs -> mkNormalC tn' cn (map tyVarBndrName tvs') (map snd fs)
  RecC cn fs    -> mkRecC tn' cn (map tyVarBndrName tvs') (map (rename . fst3) fs) (map thd3 fs)
  InfixC a cn b -> mkInfixC tn' cn (map tyVarBndrName tvs') [snd a, snd b]
  _             -> fail "mkPatterns: only constructors for \"vanilla\" syntax are supported"
  where
    mkNormalC :: Name -> Name -> [Name] -> [Type] -> Q (Name, [Dec])
    mkNormalC tn cn tvs fs = do
      xs <- replicateM (length fs) (newName "_x")
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (prefixPatSyn xs)
                         implBidir
                         [p| Pattern $(tupP (map varP xs)) |]
                     ]
      return (pat, r)
      where
        pat = rename cn
        sig = forallT
                (map plainTV tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkRecC :: Name -> Name -> [Name] -> [Name] -> [Type] -> Q (Name, [Dec])
    mkRecC tn cn tvs xs fs = do
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (recordPatSyn xs)
                         implBidir
                         [p| Pattern $(tupP (map varP xs)) |]
                     ]
      return (pat, r)
      where
        pat = rename cn
        sig = forallT
                (map plainTV tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkInfixC :: Name -> Name -> [Name] -> [Type] -> Q (Name, [Dec])
    mkInfixC tn cn tvs fs = do
      _a <- newName "_a"
      _b <- newName "_b"
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (infixPatSyn _a _b)
                         implBidir
                         [p| Pattern $(tupP [varP _a, varP _b]) |]
                     ]
      return (pat, r)
      where
        pat = mkName (':' : nameBase cn)
        sig = forallT
                (map plainTV tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

mkConS :: Name -> [TyVarBndr] -> [[Type]] -> [[Type]] -> Word8 -> Con -> Q (Name, [Dec])
mkConS tn' tvs' prev' next' tag' = \case
  NormalC nm fs -> mkNormalC tn' (map tyVarBndrName tvs') tag' nm prev' (map snd fs) next'
  -- RecC nm fs    -> undefined
  -- InfixC a nm b -> undefined
  _             -> fail "mkPatterns: only constructors for \"vanilla\" syntax are supported"
  where
    mkNormalC :: Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkNormalC tn tvs tag cn ps fs ns = do
      (fun_mk,    dec_mk)    <- mkNormalC_mk tn tvs tag cn ps fs ns
      (fun_match, dec_match) <- mkNormalC_match tn tvs tag cn ps fs ns
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
        pat = rename cn
        sig = forallT
                (map plainTV tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkNormalC_mk :: Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkNormalC_mk tn tvs tag cn fs0 fs fs1 = do
      fun <- newName ("_mk" ++ nameBase cn)
      xs  <- replicateM (length fs) (newName "_x")
      let
        vs    = foldl' (\es e -> [| SmartExp ($es `Pair` $e) |]) [| SmartExp Nil |]
              $  map (\t -> [| unExp (undef @ $(return t)) |] ) (concat (reverse fs0))
              ++ map varE xs
              ++ map (\t -> [| unExp (undef @ $(return t)) |] ) (concat fs1)

        tagged = [| Exp $ SmartExp $ Pair (SmartExp (Const (SingleScalarType (NumSingleType (IntegralNumType TypeWord8))) $(litE (IntegerL (toInteger tag))))) $vs |]
        body   = clause (map (\x -> [p| (Exp $(varP x)) |]) xs) (normalB tagged) []

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


    mkNormalC_match :: Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkNormalC_match tn tvs tag cn fs0 fs fs1 = do
      fun     <- newName ("_match" ++ nameBase cn)
      e       <- newName "_e"
      x       <- newName "_x"
      (ps,es) <- extract vs [| Prj PairIdxRight $(varE x) |] [] []
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

        matchP us = [p| TagRtag $(litP (IntegerL (toInteger tag))) $pat |]
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

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

rename :: Name -> Name
rename nm =
  let
      split acc []     = (reverse acc, '\0')  -- shouldn't happen
      split acc [l]    = (reverse acc, l)
      split acc (l:ls) = split (l:acc) ls
      --
      nm'              = nameBase nm
      (base, suffix)   = split [] nm'
   in
   case suffix of
     '_' -> mkName base
     _   -> mkName (nm' ++ "_")


-- IsPattern instances for up to 16-tuples (Acc and Exp). TH takes care of
-- the (unremarkable) boilerplate for us.
--
runQ $ do
    let
        -- Generate instance declarations for IsPattern of the form:
        -- instance (Arrays x, ArraysR x ~ (((), ArraysR a), ArraysR b), Arrays a, Arrays b,) => IsPattern Acc x (Acc a, Acc b)
        mkAccPattern :: Int -> Q [Dec]
        mkAccPattern n = do
          a <- newName "a"
          let
              -- Type variables for the elements
              xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              -- Last argument to `IsPattern`, eg (Acc a, Acc b) in the example
              b        = tupT (map (\t -> [t| Acc $(varT t)|]) xs)
              -- Representation as snoc-list of pairs, eg (((), ArraysR a), ArraysR b)
              snoc     = foldl (\sn t -> [t| ($sn, ArraysR $(varT t)) |]) [t| () |] xs
              -- Constraints for the type class, consisting of Arrays constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the snoc representation `snoc`.
              context  = tupT
                       $ [t| Arrays $(varT a) |]
                       : [t| ArraysR $(varT a) ~ $snoc |]
                       : map (\t -> [t| Arrays $(varT t)|]) xs
              --
              get x 0 = [| Acc (SmartAcc (Aprj PairIdxRight $x)) |]
              get x i = get  [| SmartAcc (Aprj PairIdxLeft $x) |] (i-1)
          --
          _x <- newName "_x"
          [d| instance $context => IsPattern Acc $(varT a) $b where
                construct $(tupP (map (\x -> [p| Acc $(varP x)|]) xs)) =
                  Acc $(foldl (\vs v -> [| SmartAcc ($vs `Apair` $(varE v)) |]) [| SmartAcc Anil |] xs)
                destruct (Acc $(varP _x)) =
                  $(tupE (map (get (varE _x)) [(n-1), (n-2) .. 0]))
            |]

        -- Generate instance declarations for IsPattern of the form:
        -- instance (Elt x, EltR x ~ (((), EltR a), EltR b), Elt a, Elt b,) => IsPattern Exp x (Exp a, Exp b)
        mkExpPattern :: Int -> Q [Dec]
        mkExpPattern n = do
          a <- newName "a"
          let
              -- Type variables for the elements
              xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
              -- Variables for sub-pattern matches
              ms       = [ mkName ('m' : show i) | i <- [0 .. n-1] ]
              tags     = foldl (\ts t -> [p| $ts `TagRpair` $(varP t) |]) [p| TagRunit |] ms
              -- Last argument to `IsPattern`, eg (Exp, a, Exp b) in the example
              b        = tupT (map (\t -> [t| Exp $(varT t)|]) xs)
              -- Representation as snoc-list of pairs, eg (((), EltR a), EltR b)
              snoc     = foldl (\sn t -> [t| ($sn, EltR $(varT t)) |]) [t| () |] xs
              -- Constraints for the type class, consisting of Elt constraints on all type variables,
              -- and an equality constraint on the representation type of `a` and the snoc representation `snoc`.
              context  = tupT
                       $ [t| Elt $(varT a) |]
                       : [t| EltR $(varT a) ~ $snoc |]
                       : map (\t -> [t| Elt $(varT t)|]) xs
              --
              get x 0 =     [| SmartExp (Prj PairIdxRight $x) |]
              get x i = get [| SmartExp (Prj PairIdxLeft $x)  |] (i-1)
          --
          _x <- newName "_x"
          _y <- newName "_y"
          [d| instance $context => IsPattern Exp $(varT a) $b where
                construct $(tupP (map (\x -> [p| Exp $(varP x)|]) xs)) =
                  let _unmatch :: SmartExp a -> SmartExp a
                      _unmatch (SmartExp (Match _ $(varP _y))) = $(varE _y)
                      _unmatch x = x
                  in
                  Exp $(foldl (\vs v -> [| SmartExp ($vs `Pair` _unmatch $(varE v)) |]) [| SmartExp Nil |] xs)
                destruct (Exp $(varP _x)) =
                  case $(varE _x) of
                    SmartExp (Match $tags $(varP _y))
                      -> $(tupE [[| Exp (SmartExp (Match $(varE m) $(get (varE _x) i))) |] | m <- ms | i <- [(n-1), (n-2) .. 0]])
                    _ -> $(tupE [[| Exp $(get (varE _x) i) |] | i <- [(n-1), (n-2) .. 0]])
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

