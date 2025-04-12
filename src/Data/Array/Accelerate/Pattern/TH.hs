{-# LANGUAGE CPP              #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.TH
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.TH (

  mkPattern,
  mkPatterns,

) where

import Data.Array.Accelerate.AST.Idx
import Data.Array.Accelerate.Pattern
import Data.Array.Accelerate.Representation.Tag
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Type

import Control.Monad
import Data.Char
import Data.List                                                    ( (\\), foldl' )
import Language.Haskell.TH.Extra                                    hiding ( Exp, Match, match )
import Numeric
import Text.Printf
import qualified Language.Haskell.TH.Extra                          as TH

import GHC.Stack


-- | As 'mkPattern', but for a list of types
--
mkPatterns :: [Name] -> DecsQ
mkPatterns nms = concat <$> mapM mkPattern nms

-- | Generate pattern synonyms for the given simple (Haskell'98) sum or
-- product data type.
--
-- Constructor and record selectors are renamed to add a trailing
-- underscore if it does not exist, or to remove it if it does. For infix
-- constructors, the name is prepended with a colon ':'. For example:
--
-- > data Point = Point { xcoord_ :: Float, ycoord_ :: Float }
-- >   deriving (Generic, Elt)
--
-- Will create the pattern synonym:
--
-- > Point_ :: Exp Float -> Exp Float -> Exp Point
--
-- together with the selector functions
--
-- > xcoord :: Exp Point -> Exp Float
-- > ycoord :: Exp Point -> Exp Float
--
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

mkNewtypeD :: Name -> [TyVarBndr a] -> Con -> DecsQ
mkNewtypeD tn tvs c = mkDataD tn tvs [c]

mkDataD :: Name -> [TyVarBndr a] -> [Con] -> DecsQ
mkDataD tn tvs cs = do
  (pats, decs) <- unzip <$> go cs
  comp         <- pragCompleteD pats Nothing
  return $ comp : concat decs
  where
    -- For single-constructor types we create the pattern synonym for the
    -- type directly in terms of Pattern
    go []  = fail "mkPatterns: empty data declarations not supported"
    go [c] = return <$> mkConP tn tvs c
    go _   = go' [] (map fieldTys cs) [0 .. fromIntegral (length cs - 1)] cs

    -- For sum-types, when creating the pattern for an individual
    -- constructor we need to know about the types of the fields all other
    -- constructors as well
    go' prev (this:next) (tag:tags) (con:cons) = do
      r  <- mkConS tn tvs prev next tag con
      rs <- go' (this:prev) next tags cons
      return (r : rs)
    go' _ [] [] [] = return []
    go' _ _  _  _  = fail "mkPatterns: unexpected error"

    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = fail "mkPatterns: only constructors for \"vanilla\" syntax are supported"


mkConP :: Name -> [TyVarBndr a] -> Con -> Q (Name, [Dec])
mkConP tn' tvs' con' = do
  checkExts [ PatternSynonyms ]
  case con' of
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
                (map (`plainInvisTV` specifiedSpec) tvs)
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
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkInfixC :: Name -> Name -> [Name] -> [Type] -> Q (Name, [Dec])
    mkInfixC tn cn tvs fs = do
      mf <- reifyFixity cn
      _a <- newName "_a"
      _b <- newName "_b"
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (infixPatSyn _a _b)
                         implBidir
                         [p| Pattern $(tupP [varP _a, varP _b]) |]
                     ]
      r' <- case mf of
              Nothing -> return r
-- Since template-haskell 2.22, NamespaceSpecifier has been added
-- https://hackage.haskell.org/package/template-haskell-2.22.0.0/changelog
#if MIN_VERSION_template_haskell(2,22,0)
              Just f  -> return (InfixD f NoNamespaceSpecifier pat : r)
#else
              Just f  -> return (InfixD f pat : r)
#endif
      return (pat, r')
      where
        pat = mkName (':' : nameBase cn)
        sig = forallT
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

mkConS :: Name -> [TyVarBndr a] -> [[Type]] -> [[Type]] -> Word8 -> Con -> Q (Name, [Dec])
mkConS tn' tvs' prev' next' tag' con' = do
  checkExts [GADTs, PatternSynonyms, ScopedTypeVariables, TypeApplications, ViewPatterns]
  case con' of
    NormalC cn fs -> mkNormalC tn' cn tag' (map tyVarBndrName tvs') prev' (map snd fs) next'
    RecC cn fs    -> mkRecC tn' cn tag' (map tyVarBndrName tvs') (map (rename . fst3) fs) prev' (map thd3 fs) next'
    InfixC a cn b -> mkInfixC tn' cn tag' (map tyVarBndrName tvs') prev' [snd a, snd b] next'
    _             -> fail "mkPatterns: only constructors for \"vanilla\" syntax are supported"
  where
    mkNormalC :: Name -> Name -> Word8 -> [Name] -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkNormalC tn cn tag tvs ps fs ns = do
      let pat = rename cn
      (fun_build, dec_build) <- mkBuild tn (nameBase cn) tvs tag ps fs ns
      (fun_match, dec_match) <- mkMatch tn (nameBase pat) (nameBase cn) tvs tag ps fs ns
      dec_pat                <- mkNormalC_pattern tn pat tvs fs fun_build fun_match
      return $ (pat, concat [dec_pat, dec_build, dec_match])

    mkRecC :: Name -> Name -> Word8 -> [Name] -> [Name] -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkRecC tn cn tag tvs xs ps fs ns = do
      let pat = rename cn
      (fun_build, dec_build) <- mkBuild tn (nameBase cn) tvs tag ps fs ns
      (fun_match, dec_match) <- mkMatch tn (nameBase pat) (nameBase cn) tvs tag ps fs ns
      dec_pat                <- mkRecC_pattern tn pat tvs xs fs fun_build fun_match
      return $ (pat, concat [dec_pat, dec_build, dec_match])

    mkInfixC :: Name -> Name -> Word8 -> [Name] -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkInfixC tn cn tag tvs ps fs ns = do
      let pat = mkName (':' : nameBase cn)
      (fun_build, dec_build) <- mkBuild tn (zencode (nameBase cn)) tvs tag ps fs ns
      (fun_match, dec_match) <- mkMatch tn ("(" ++ nameBase pat ++ ")") (zencode (nameBase cn)) tvs tag ps fs ns
      dec_pat                <- mkInfixC_pattern tn cn pat tvs fs fun_build fun_match
      return $ (pat, concat [dec_pat, dec_build, dec_match])

    mkNormalC_pattern :: Name -> Name -> [Name] -> [Type] -> Name -> Name -> Q [Dec]
    mkNormalC_pattern tn pat tvs fs build match = do
      xs <- replicateM (length fs) (newName "_x")
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (prefixPatSyn xs)
                         (explBidir [clause [] (normalB (varE build)) []])
                         (parensP $ viewP (varE match) [p| Just $(tupP (map varP xs)) |])
                     ]
      return r
      where
        sig = forallT
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt ([t| HasCallStack |] : map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkRecC_pattern :: Name -> Name -> [Name] -> [Name] -> [Type] -> Name -> Name -> Q [Dec]
    mkRecC_pattern tn pat tvs xs fs build match = do
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (recordPatSyn xs)
                         (explBidir [clause [] (normalB (varE build)) []])
                         (parensP $ viewP (varE match) [p| Just $(tupP (map varP xs)) |])
                     ]
      return r
      where
        sig = forallT
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt ([t| HasCallStack |] : map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkInfixC_pattern :: Name -> Name -> Name -> [Name] -> [Type] -> Name -> Name -> Q [Dec]
    mkInfixC_pattern tn cn pat tvs fs build match = do
      mf <- reifyFixity cn
      _a <- newName "_a"
      _b <- newName "_b"
      r  <- sequence [ patSynSigD pat sig
                     , patSynD    pat
                         (infixPatSyn _a _b)
                         (explBidir [clause [] (normalB (varE build)) []])
                         (parensP $ viewP (varE match) [p| Just $(tupP [varP _a, varP _b]) |])
                     ]
      r' <- case mf of
              Nothing -> return r
-- Since template-haskell 2.22, NamespaceSpecifier has been added
-- https://hackage.haskell.org/package/template-haskell-2.22.0.0/changelog
#if MIN_VERSION_template_haskell(2,22,0)
              Just f  -> return (InfixD f NoNamespaceSpecifier pat : r)
#else
              Just f  -> return (InfixD f pat : r)
#endif
      return r'
      where
        sig = forallT
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt ([t| HasCallStack |] : map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))

    mkBuild :: Name -> String -> [Name] -> Word8 -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkBuild tn cn tvs tag fs0 fs fs1 = do
      fun <- newName ("_build" ++ cn)
      xs  <- replicateM (length fs) (newName "_x")
      let
        vs    = foldl' (\es e -> [| SmartExp ($es `Pair` $e) |]) [| SmartExp Nil |]
              $  map (\t -> [| unExp $(varE 'undef `appTypeE` return t) |] ) (concat (reverse fs0))
              ++ map varE xs
              ++ map (\t -> [| unExp $(varE 'undef `appTypeE` return t) |] ) (concat fs1)

        tagged = [| Exp $ SmartExp $ Pair (SmartExp (Const (SingleScalarType (NumSingleType (IntegralNumType TypeWord8))) $(litE (IntegerL (toInteger tag))))) $vs |]
        body   = clause (map (\x -> [p| (Exp $(varP x)) |]) xs) (normalB tagged) []

      r <- sequence [ sigD fun sig
                    , funD fun [body]
                    ]
      return (fun, r)
      where
        sig = forallT
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
                (foldr (\t ts -> [t| $t -> $ts |])
                       [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                       (map (\t -> [t| Exp $(return t) |]) fs))


    mkMatch :: Name -> String -> String -> [Name] -> Word8 -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
    mkMatch tn pn cn tvs tag fs0 fs fs1 = do
      fun     <- newName ("_match" ++ cn)
      e       <- newName "_e"
      x       <- newName "_x"
      (ps,es) <- extract vs [| Prj PairIdxRight $(varE x) |] [] []
      unbind  <- isExtEnabled RebindableSyntax
      let
        eqE   = if unbind then letE [funD (mkName "==") [clause [] (normalB (varE '(==))) []]] else id
        lhs   = [p| (Exp $(varP e)) |]
        body  = normalB $ eqE $ caseE (varE e)
          [ TH.match (conP 'SmartExp [(conP 'Match [matchP ps, varP x])]) (normalB [| Just $(tupE es)  |]) []
          , TH.match (conP 'SmartExp [(recP 'Match [])])                  (normalB [| Nothing          |]) []
          , TH.match wildP                                                (normalB [| error $error_msg |]) []
          ]

      r <- sequence [ sigD fun sig
                    , funD fun [clause [lhs] body []]
                    ]
      return (fun, r)
      where
        sig = forallT
                (map (`plainInvisTV` specifiedSpec) tvs)
                (cxt ([t| HasCallStack |] : map (\t -> [t| Elt $(varT t) |]) tvs))
                [t| Exp $(foldl' appT (conT tn) (map varT tvs)) -> Maybe $(tupT (map (\t -> [t| Exp $(return t) |]) fs)) |]

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

        error_msg =
          let pv = unwords
                 $ take (length fs + 1)
                 $ concatMap (map reverse)
                 $ iterate (concatMap (\xs -> [ x:xs | x <- ['a'..'z'] ])) [""]
           in stringE $ unlines
             [ "Embedded pattern synonym used outside 'match' context."
             , ""
             , "To use case statements in the embedded language the case statement must"
             , "be applied as an n-ary function to the 'match' operator. For single"
             , "argument case statements this can be done inline using LambdaCase, for"
             , "example:"
             , ""
             , "> x & match \\case"
             , printf ">   %s%s -> ..." pn pv
             , printf ">   _%s -> ..." (replicate (length pn + length pv - 1) ' ')
             ]

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

checkExts :: [Extension] -> Q ()
checkExts req = do
  enabled <- extsEnabled
  let missing = req \\ enabled
  unless (null missing) . fail . unlines
    $ printf "You must enable the following language extensions to generate pattern synonyms:"
    : map (printf "    {-# LANGUAGE %s #-}" . show) missing

-- A simplified version of that stolen from GHC/Utils/Encoding.hs
--
type EncodedString = String

zencode :: String -> EncodedString
zencode []       = []
zencode (h:rest) = encode_digit h ++ go rest
  where
    go []     = []
    go (c:cs) = encode_ch c ++ go cs

unencoded_char :: Char -> Bool
unencoded_char 'z' = False
unencoded_char 'Z' = False
unencoded_char c   = isAlphaNum c

encode_digit :: Char -> EncodedString
encode_digit c | isDigit c = encode_as_unicode_char c
               | otherwise = encode_ch c

encode_ch :: Char -> EncodedString
encode_ch c | unencoded_char c = [c]     -- Common case first
encode_ch '('  = "ZL"
encode_ch ')'  = "ZR"
encode_ch '['  = "ZM"
encode_ch ']'  = "ZN"
encode_ch ':'  = "ZC"
encode_ch 'Z'  = "ZZ"
encode_ch 'z'  = "zz"
encode_ch '&'  = "za"
encode_ch '|'  = "zb"
encode_ch '^'  = "zc"
encode_ch '$'  = "zd"
encode_ch '='  = "ze"
encode_ch '>'  = "zg"
encode_ch '#'  = "zh"
encode_ch '.'  = "zi"
encode_ch '<'  = "zl"
encode_ch '-'  = "zm"
encode_ch '!'  = "zn"
encode_ch '+'  = "zp"
encode_ch '\'' = "zq"
encode_ch '\\' = "zr"
encode_ch '/'  = "zs"
encode_ch '*'  = "zt"
encode_ch '_'  = "zu"
encode_ch '%'  = "zv"
encode_ch c    = encode_as_unicode_char c

encode_as_unicode_char :: Char -> EncodedString
encode_as_unicode_char c
  = 'z'
  : if isDigit (head hex_str) then hex_str
                              else '0':hex_str
  where
    hex_str = showHex (ord c) "U"

