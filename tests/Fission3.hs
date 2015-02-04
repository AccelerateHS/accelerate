{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

import Prelude                                  as P
import Data.Array.Accelerate                    as A


-- Split a given range `[0,len)` into `k` almost-equal pieces. Each of the
-- pieces is a range with inclusive-left exclusive-right.
--
split :: Exp Int -> Int -> [(Exp Int, Exp Int)]
split len k = P.zip points (P.tail points)
  where
    (chunk, leftover)   = len `P.quotRem` constant k

    points              = P.map (splitIx . constant) [0 .. k]
    splitIx i           =
      i <* leftover ? ( i * (chunk + 1)
                      , i * chunk + leftover )


-- Split an array into 'k' pieces and map the function 'f' over each of them.
-- This results in 'k' separate let bindings.
--
fissionMap
    :: forall sh a b. (Shape sh, Slice sh, Elt a, Elt b)
    => Int
    -> (Exp a -> Exp b)
    -> Acc (Array (sh :. Int) a)
    -> Acc (Array (sh :. Int) b)
fissionMap k f xs =
  let _ :. sz   = unlift (shape xs)     :: Exp sh :. Exp Int
      yss       = P.map (\(m,n) -> slit m n xs) (split sz k)
  in
  foldl1 (A.++) (P.map (A.compute . A.map f) yss)

