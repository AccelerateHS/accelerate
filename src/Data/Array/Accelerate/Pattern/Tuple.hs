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
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
-- |
-- Module      : Data.Array.Accelerate.Pattern.Tuple
-- Copyright   : [2018..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pattern.Tuple (

  pattern T2,  pattern T3,  pattern T4,  pattern T5,  pattern T6,
  pattern T7,  pattern T8,  pattern T9,  pattern T10, pattern T11,
  pattern T12, pattern T13, pattern T14, pattern T15, pattern T16,

) where

import Data.Array.Accelerate.Pattern                                ( pattern Pattern )
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt

import Language.Haskell.TH.Extra                                    hiding ( Exp, Match )


-- Generate polymorphic pattern synonyms to construct and destruct tuples on
-- both Haskell values and embedded expressions. This isn't really necessary but
-- provides for a more consistent interface.
--
runQ $
  let
      mkT :: Int -> Q [Dec]
      mkT n =
        let xs        = [ mkName ('x' : show i) | i <- [0 .. n-1] ]
            res       = mkName "r"
            xsT       = map varT xs
            xsP       = map varP xs
            xsE       = map varE xs
            name      = mkName ('T':show n)
            isT       = mkName ("IsT" ++ show n)
            builder   = mkName ("buildT" ++ show n)
            matcher   = mkName ("matchT" ++ show n)
            sig       = foldr (\t r -> [t| $t -> $r |]) (varT res) xsT
            hdr ts r  = foldl appT (conT isT) (ts ++ [r])
        in
        sequence
          -- Value/Embedded polymorphic pattern synonym
          [ patSynSigD name [t| $(hdr xsT (varT res)) => $sig |]
          , patSynD    name (prefixPatSyn xs) (explBidir [clause [] (normalB (varE builder)) []]) (parensP $ viewP (varE matcher) (tupP xsP))
          , pragCompleteD [name] (Just (tupleTypeName n))
          , pragCompleteD [name] (Just ''Acc)
          , pragCompleteD [name] (Just ''Exp)
          --
          , classD (return []) isT (map plainTV (xs ++ [res])) [funDep [res] xs]
            [ sigD builder sig
            , sigD matcher [t| $(varT res) -> $(tupT xsT) |]
            ]
          , instanceD (return []) [t| $(hdr xsT (tupT xsT)) |]
            [ funD builder [ clause xsP (normalB (tupE xsE)) [] ]
            , funD matcher [ clause []  (normalB [| id |])   [] ]
            ]
          , instanceD (sequence ( [t| $(varT res) ~ $(tupT xsT) |] : map (\x -> [t| Elt $x |]) xsT )) [t| $(hdr (map (\x -> [t| Exp $x |]) xsT) [t| Exp $(varT res) |]) |]
            [ funD builder [ clause xsP (normalB [| Pattern $(tupE xsE) |]) [] ]
            , funD matcher [ clause [conP (mkName "Pattern") [tupP xsP]] (normalB (tupE xsE)) []]
            ]
          , instanceD (sequence ( [t| $(varT res) ~ $(tupT xsT) |] : map (\x -> [t| Arrays $x |]) xsT)) [t| $(hdr (map (\x -> [t| Acc $x |]) xsT) [t| Acc $(varT res) |]) |]
            [ funD builder [ clause xsP (normalB [| Pattern $(tupE xsE) |]) [] ]
            , funD matcher [ clause [conP (mkName "Pattern") [tupP xsP]] (normalB (tupE xsE)) []]
            ]
          ]
  in
  concat <$> mapM mkT [2..16]

