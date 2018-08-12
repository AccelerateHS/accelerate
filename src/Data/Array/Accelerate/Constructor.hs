{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Module      : Data.Array.Accelerate.Constructor
-- Copyright   : [2018..2018] Joshua Meredith, Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Constructor (

  pattern MkT,

) where

import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart

import Language.Haskell.TH hiding (Exp)


-- | This pattern synonym can be used as an alternative to 'lift' and 'unlift'
-- for creating and accessing data types isomorphic to simple product (tuple)
-- types.
--
-- For example, let's say we have regular Haskell data type representing a point
-- in two-dimensional space:
--
-- > data Point = Point_ Float Float
-- >   deriving (Show, Generic, Elt, IsTuple)
--
-- Note that we derive instances for the 'Elt' class, so that this data type can
-- be used within Accelerate scalar expressions, and 'IsTuple', as this is
-- a product type (contains multiple values).
--
-- In order to access the individual fields of the data constructor from within
-- an Accelerate expression, we define the following pattern synonym:
--
-- > pattern Point :: Exp Float -> Exp Float -> Exp Point
-- > pattern Point x y = MkT (x,y)
--
-- In essence, the 'MkT' pattern is really telling GHC how to treat our @Point@
-- type as a regular pair for use in Accelerate code. The pattern can then be
-- used on both the left and right hand side of an expression:
--
-- > addPoint :: Exp Point -> Exp Point -> Exp Point
-- > addPoint (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)
--
-- Similarly, we can define pattern synonyms for values in 'Acc'. We can also
-- use record syntax to generate field accessors, if we desire:
--
-- > data SparseVector a = SparseVector_ (Vector Int) (Vector a)
-- >   deriving (Show, Generic, Arrays, IsAtuple)
-- >
-- > pattern SparseVector :: Elt a => Acc (Vector Int) -> Acc (Vector a) -> Acc (SparseVector a)
-- > pattern SparseVector { indices, values } = MkT (indices, values)
--
pattern MkT :: forall tup a context. MkData context a tup => tup -> context a
pattern MkT vars <- (destruct @context -> vars)
  where MkT = construct @context

class MkData con a t where
  construct :: t -> con a
  destruct  :: con a -> t

$(runQ $ do
    let
      genData :: Int -> TypeQ -> ExpQ -> TypeQ -> ExpQ -> ExpQ -> ExpQ -> ExpQ -> Q Dec
      genData n cst tpl conT con prj nil snc = do

        let
          t   = return . VarT $ mkName "t"
          vs  = map mkName . map (:[]) $ take n "abcdefghijklmno"
          x   = mkName "x"
          ts  = map VarT vs

          prodReprN = foldl (\acc a -> [t| ($acc, $(return a)) |]) [t| () |] ts

        eqRepr    <- [t| ProdRepr $t ~ $prodReprN |]
        isProduct <- [t| IsProduct $cst $t |]

        cst'      <- cst
        conT'     <- conT
        t'        <- t

        let
          tut | [x] <- ts = [t| $conT $(return x) |]
              | otherwise = return $ foldl (\acc x -> AppT acc (AppT conT' x)) (TupleT n) ts

          tup      = TupP $ map VarP vs
          snoc x y = [| $snc $x $(return $ VarE y) |]

          tupidx 1 = [| ZeroTupIdx |]
          tupidx n = [| SuccTupIdx $(tupidx (n - 1)) |]
          prjN   n = [| $con $ $prj $(tupidx n) $(return $ VarE x) |]

          cxt = isProduct : eqRepr : map (AppT cst') (t':ts)

        typ    <- [t| MkData $conT $t $tut |]
        constr <- [| $con $ $tpl $ $(foldl snoc nil vs) |]
        destr  <- TupE <$> mapM prjN [n,n-1..1]

        return $ InstanceD Nothing cxt typ
          [ FunD 'construct [Clause [tup   ] (NormalB constr) []]
          , FunD 'destruct  [Clause [VarP x] (NormalB destr ) []]
          ]

      genDataExp :: Int -> Q Dec
      genDataExp n = genData n [t|Elt|] [|Tuple|] [t|Exp|] [|Exp|] [|Prj|] [|NilTup|] [|SnocTup|]

      genDataAcc :: Int -> Q Dec
      genDataAcc n = genData n [t|Arrays|] [|Atuple|] [t|Acc|] [|Acc|] [|Aprj|] [|NilAtup|] [|SnocAtup|]

    exps <- mapM genDataExp [1..15]
    accs <- mapM genDataAcc [1..15]

    return $ exps ++ accs
 )
