{-# LANGUAGE CPP                 #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
-- Module      : Language.Haskell.TH.Extra
-- Copyright   : [2019..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Language.Haskell.TH.Extra (

  module Language.Haskell.TH,
  module Language.Haskell.TH.Extra,

) where

import Language.Haskell.TH                                          hiding ( TyVarBndr, tupP, tupE )
import qualified Language.Haskell.TH                                as TH

#if !MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax                                   ( unTypeQ, unsafeTExpCoerce )
import GHC.Exts                                                     ( RuntimeRep, TYPE )
#else
import Language.Haskell.TH                                          ( TyVarBndr )
#endif


tupT :: [TypeQ] -> TypeQ
tupT [t] = t
tupT tup =
  let n = length tup
   in foldl (\ts t -> [t| $ts $t |]) (tupleT n) tup

tupP :: [PatQ] -> PatQ
tupP [p] = p
tupP ps  = TH.tupP ps

tupE :: [ExpQ] -> ExpQ
tupE [t] = t
tupE ts  = TH.tupE ts

#if !MIN_VERSION_template_haskell(2,17,0)

type CodeQ a = Q (TExp a)

type TyVarBndr flag = TH.TyVarBndr

data Specificity = SpecifiedSpec | InferredSpec

specifiedSpec :: Specificity
specifiedSpec = SpecifiedSpec

plainInvisTV' :: Name -> Specificity -> TyVarBndr Specificity
plainInvisTV' n _ = PlainTV n

unsafeCodeCoerce :: forall (r :: RuntimeRep) (a :: TYPE r). Q Exp -> Q (TExp a)
unsafeCodeCoerce = unsafeTExpCoerce

unTypeCode :: forall (r :: RuntimeRep) (a :: TYPE r). Q (TExp a) -> Q Exp
unTypeCode = unTypeQ

tyVarBndrName :: TyVarBndr flag -> Name
tyVarBndrName (PlainTV  n)   = n
tyVarBndrName (KindedTV n _) = n

#else

tyVarBndrName :: TyVarBndr flag -> Name
tyVarBndrName (PlainTV  n _)   = n
tyVarBndrName (KindedTV n _ _) = n

plainInvisTV' :: Name -> Specificity -> TyVarBndr Specificity
plainInvisTV' = PlainTV

#endif

