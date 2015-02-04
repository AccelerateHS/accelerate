{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Fission where

import Prelude                                          as P

import Data.Array.Accelerate                            as A
import qualified Data.Array.Accelerate.Smart            as S
import qualified Data.Array.Accelerate.AST              as AST

import Data.Array.Accelerate.Debug
import Data.Array.Accelerate.Trafo
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing
-- import Data.Array.Accelerate.Type
-- import Data.Array.Accelerate.Product
-- import Data.Array.Accelerate.Array.Sugar

-- xs,ys :: Acc (Vector Int)
-- xs = use $ fromList (Z :. 10) [0..]
-- ys = use $ fromList (Z :. 10) [0..]
--
-- -- Split a given range `[0,len)` into `k` almost-equal pieces. Each of the
-- -- pieces is a range with inclusive-left exclusive-right.
-- --
-- split :: Exp Int -> Int -> [(Exp Int, Exp Int)]
-- split len k = P.zip points (P.tail points)
--   where
--     (chunk, leftover)   = len `P.quotRem` constant k
--
--     points              = P.map (splitIx . constant) [0 .. k]
--     splitIx i           =
--       i <* leftover ? ( i * (chunk + 1)
--                       , i * chunk + leftover )


pconcat :: forall a. Elt a => AST.OpenAcc (((), Vector a), Vector a) (Vector a)
pconcat =
  let acc = Sharing.convertAfun True True True True ((A.++) :: Acc (Vector a) -> Acc (Vector a) -> Acc (Vector a))
  in case acc of
       AST.Alam (AST.Alam (AST.Abody x)) -> x


pconcat' :: Elt a => AST.OpenAcc (((), Vector a), Vector a) (Vector a)
pconcat' =
  let vars      = [1, 0]
      x         = S.Acc $ S.Atag 1
      y         = S.Acc $ S.Atag 0
      alyt      = Sharing.EmptyLayout `Sharing.PushLayout` AST.SuccIdx AST.ZeroIdx
                                      `Sharing.PushLayout` AST.ZeroIdx
      config    = Sharing.Config True True True True
  in
  Sharing.convertOpenAcc config 2 vars alyt (x A.++ y)

