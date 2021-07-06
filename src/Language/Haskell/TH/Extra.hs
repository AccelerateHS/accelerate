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

#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH                                          hiding ( plainInvisTV, tupP, tupE )
#else
import Language.Haskell.TH                                          hiding ( TyVarBndr, tupP, tupE )
import Language.Haskell.TH.Syntax                                   ( unTypeQ, unsafeTExpCoerce )
#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Exts                                                     ( RuntimeRep, TYPE )
#endif
#endif
import qualified Language.Haskell.TH                                as TH


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


#if MIN_VERSION_template_haskell(2,17,0)

tyVarBndrName :: TyVarBndr flag -> Name
tyVarBndrName (PlainTV  n _)   = n
tyVarBndrName (KindedTV n _ _) = n

plainInvisTV :: Name -> Specificity -> TyVarBndr Specificity
plainInvisTV = PlainTV

#else

type CodeQ a = Q (TExp a)

type TyVarBndr flag = TH.TyVarBndr

data Specificity = SpecifiedSpec | InferredSpec

specifiedSpec :: Specificity
specifiedSpec = SpecifiedSpec

tyVarBndrName :: TyVarBndr flag -> Name
tyVarBndrName (PlainTV  n)   = n
tyVarBndrName (KindedTV n _) = n

plainInvisTV :: Name -> Specificity -> TyVarBndr Specificity
plainInvisTV n _ = PlainTV n

#if MIN_VERSION_template_haskell(2,16,0)
unsafeCodeCoerce :: forall (r :: RuntimeRep) (a :: TYPE r). Q Exp -> Q (TExp a)
#else
unsafeCodeCoerce :: Q Exp -> Q (TExp a)
#endif
unsafeCodeCoerce = unsafeTExpCoerce

#if MIN_VERSION_template_haskell(2,16,0)
unTypeCode :: forall (r :: RuntimeRep) (a :: TYPE r). Q (TExp a) -> Q Exp
#else
unTypeCode :: Q (TExp a) -> Q Exp
#endif
unTypeCode = unTypeQ

#endif

