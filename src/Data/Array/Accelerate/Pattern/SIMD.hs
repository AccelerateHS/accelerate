{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ViewPatterns           #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.SIMD (

  pattern SIMD,
  pattern V2, pattern V3, pattern V4, pattern V8, pattern V16,

) where

import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Vec
import Data.Array.Accelerate.Type

import Language.Haskell.TH.Extra                                    hiding ( Exp, Match )
import GHC.Exts                                                     ( IsList(..) )


pattern SIMD :: forall b a context. IsSIMD context a b => b -> context a
pattern SIMD vars <- (vmatcher @context -> vars)
  where SIMD = vbuilder @context
{-# COMPLETE SIMD #-}

class IsSIMD context a b where
  vbuilder :: b -> context a
  vmatcher :: context a -> b


runQ $
  let
      -- Generate instance declarations for IsSIMD of the form:
      -- instance (Elt a, Elt v, EltR v ~ VecR n a) => IsSIMD Exp v (Exp a, Exp a)
      mkVecPattern :: Int -> Q [Dec]
      mkVecPattern n = do
        a  <- newName "a"
        v  <- newName "v"
        _x <- newName "_x"
        _y <- newName "_y"
        let
            aT       = varT a
            vT       = varT v
            nT       = litT (numTyLit (toInteger n))
            -- Last argument to `IsSIMD`, eg (Exp, a, Exp a) in the example
            tup      = tupT (replicate n ([t| Exp $aT |]))
            -- Constraints for the type class, consisting of the Elt
            -- constraints and the equality on representation types
            context  = [t| (Elt $aT, Elt $vT, SIMD $nT $aT, EltR $vT ~ VecR $nT $aT) |]
            -- Type variables for the elements
            xs       = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            -- Variables for sub-pattern matches
            -- ms       = [ mkName ('m' : show i) | i <- [0 .. n-1] ]
            -- tags     = foldl (\ts t -> [p| $ts `TagRpair` $(varP t) |]) [p| TagRunit |] ms
        --
        [d| instance $context => IsSIMD Exp $vT $tup where
              vbuilder $(tupP (map (\x -> [p| Exp $(varP x)|]) xs)) =
                let _unmatch :: SmartExp a -> SmartExp a
                    _unmatch (SmartExp (Match _ $(varP _y))) = $(varE _y)
                    _unmatch x = x
                in
                Exp $(foldl (\vs (i, x) -> [| mkInsert
                                                $(varE 'vecR `appTypeE` nT `appTypeE` aT)
                                                $(varE 'eltR `appTypeE` aT)
                                                TypeWord8
                                                $vs
                                                (SmartExp (Const (NumScalarType (IntegralNumType (SingleIntegralType TypeWord8))) i))
                                                (_unmatch $(varE x))
                                            |])
                        [| unExp (undef :: Exp (Vec $nT $aT)) |]
                        (zip [0 .. n-1] xs)
                 )

              vmatcher (Exp $(varP _x)) =
                case $(varE _x) of
                  -- SmartExp (Match $tags $(varP _y))
                  --   -> $(tupE [[| Exp (SmartExp (Match $(varE m) (unExp (extract (Exp $(varE _x) :: Exp $vec) (constant (i :: Word8)))))) |] | m <- ms | i <- [0 .. n-1]])
                  --   -> $(tupE [[| Exp (SmartExp (Match $(varE m) (mkExtract
                  --                   $(varE 'vecR `appTypeE` nT `appTypeE` aT)
                  --                   $(varE 'eltR `appTypeE` aT)
                  --                   TypeWord8
                  --                   $(varE _x)
                  --                   (SmartExp (Const (NumScalarType (IntegralNumType (SingleIntegralType TypeWord8))) i))))) |]
                  --             | m <- ms
                  --             | i <- [0 .. n-1] ])

                  _ -> $(tupE [[| Exp $ mkExtract
                                    $(varE 'vecR `appTypeE` nT `appTypeE` aT)
                                    $(varE 'eltR `appTypeE` aT)
                                    TypeWord8
                                    $(varE _x)
                                    (SmartExp (Const (NumScalarType (IntegralNumType (SingleIntegralType TypeWord8))) i))
                                |]
                              | i <- [0 .. n-1] ])
          |]
  in
  concat <$> mapM mkVecPattern [2,3,4,8,16]

        -- mkV :: Int -> Q [Dec]
        -- mkV n =
        --   let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
        --       a     = varT (mkName "a")
        --       ts    = replicate n a
        --       name  = mkName ('V':show n)
        --       tup   = tupT (map (\t -> [t| Exp $t |]) ts)
        --       vec   = [t| Vec $(litT (numTyLit (toInteger n))) $a |]
        --       cst   = [t| (Elt $a, SIMD $(litT (numTyLit (toInteger n))) $a, IsSIMD Exp $vec $tup) |]
        --       sig   = foldr (\t r -> [t| Exp $t -> $r |]) [t| Exp $vec |] ts
        --   in
        --   sequence
        --     [ patSynSigD name [t| $cst => $sig |]
        --     , patSynD    name (prefixPatSyn xs) implBidir [p| SIMD $(tupP (map varP xs)) |]
        --     , pragCompleteD [name] Nothing
        --     ]

        -- mkV :: Int -> Q [Dec]
        -- mkV n =
        --   let xs    = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
        --       ts    = map varT xs
        --       name  = mkName ('V':show n)
        --       con   = varT (mkName "con")
        --       ty1   = varT (mkName "vec")
        --       ty2   = tupT (map (con `appT`) ts)
        --       sig   = foldr (\t r -> [t| $con $t -> $r |]) (appT con ty1) ts
        --   in
        --   sequence
        --     [ patSynSigD name [t| IsVector $con $ty1 $ty2 => $sig |]
        --     , patSynD    name (prefixPatSyn xs) implBidir [p| Vector $(tupP (map varP xs)) |]
        --     , pragCompleteD [name] (Just ''Exp)
        --     ]

-- Generate polymorphic pattern synonyms which operate on both Haskell values
-- as well as embedded expressions
--
runQ $
  let
      mkV :: Int -> Q [Dec]
      mkV n = do
        a <- newName "a"
        v <- newName "v"
        let
            as      = replicate n (varT a)
            xs      = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            xsP     = map varP xs
            xsE     = map varE xs
            vn      = mkName ("V" ++ show n)
            isV     = mkName ("IsV" ++ show n)
            builder = mkName ("buildV" ++ show n)
            matcher = mkName ("matchV" ++ show n)
            ctx     = return [ ConT ''Elt `AppT` VarT a
                             , ConT ''SIMD `AppT` LitT (NumTyLit (toInteger n)) `AppT` VarT a
                             ]
        --
        sequence
          [ patSynSigD vn [t| $(conT isV) $(varT a) $(varT v) => $(foldr (\t r -> [t| $t -> $r |]) (varT v) as) |]
          , patSynD    vn (prefixPatSyn xs) (explBidir [clause [] (normalB (varE builder)) []]) (parensP $ viewP (varE matcher) (tupP xsP))
          , pragCompleteD [vn] Nothing
          --
          , classD (return []) isV [PlainTV a (), PlainTV v ()] [funDep [v] [a]]
            [ sigD builder (foldr (\t r -> [t| $t -> $r |]) (varT v) as)
            , sigD matcher [t| $(varT v) -> $(tupT as) |]
            ]
          -- This instance which goes via toList is horrible and I feel bad for using it
          --   TLM 2022-06-27
          , instanceD ctx [t| $(conT isV) $(varT a) ($(conT vn) $(varT a)) |]
            [ funD builder [ clause xsP (normalB [| fromList $(listE xsE) |]) []]
            , funD matcher [ clause [viewP (varE 'toList) (listP xsP)] (normalB (tupE xsE)) [] ]
            ]
          , instanceD ctx [t| $(conT isV) (Exp $(varT a)) (Exp ($(conT vn) $(varT a))) |]
            [ funD builder [ clause xsP (normalB [| SIMD $(tupE xsE) |]) []]
            , funD matcher [ clause [conP (mkName "SIMD") [tupP xsP]] (normalB (tupE xsE)) [] ]
            ]
          ]
  in
  concat <$> mapM mkV [2,3,4,8,16]

