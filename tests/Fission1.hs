{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Fission where

import Prelude                                          as P
import Control.Monad
import Data.Typeable

import qualified Data.Array.Accelerate                  as A
import qualified Data.Array.Accelerate.Smart            as S

import Data.Array.Accelerate.AST                        as AST

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Trafo.Base
import Data.Array.Accelerate.Trafo.Simplify
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Array.Sugar
import qualified Data.Array.Accelerate.Trafo            as T
import qualified Data.Array.Accelerate.Trafo.Sharing    as Sharing

import qualified Data.Array.Accelerate.Interpreter      as I
import qualified Data.Array.Accelerate.Debug            as Debug


-- ifThenElse :: Elt t => Exp Bool -> Exp t -> Exp t -> Exp t
-- ifThenElse p t e = p ? (t, e)

{--
xs :: Acc (Vector Int)
xs = use $ fromList (Z :. 10) [0..]

xs' :: Acc (Vector Int)
xs' = A.enumFromN (constant (Z :. 10)) 0

x1,x2 :: Acc (Vector Int)
[x1,x2] = fission 2 xs'


y1,y2 :: Acc (Vector Int)
y1 = generate (constant (Z :. 5)) (\ix -> A.fromIntegral (unindex1 ix))
y2 = generate (constant (Z :. 5)) (\ix -> A.fromIntegral (5 + unindex1 ix))

-- simplifier test: let x = e in .. e ..
--
test1 :: Acc (Vector Int)
test1 =
  generate
    (constant (Z :. 10))
    (\ix -> let i = unindex1 ix
            in  i <* 5 ? (i, 5+(5 - unindex1 ix)))


fission :: (Slice sh, Shape sh, Elt e)
        => Int
        -> Acc (Array (sh :. Int) e)
        -> [ Acc (Array (sh :. Int) e) ]
fission k acc =
  let n = indexHead (shape acc)
  in  P.map (flip (P.uncurry slit') acc) (split n k)


-- Slice out a subarray along the outermost dimension
--
slit' :: forall sh e. (Slice sh, Shape sh, Elt e)
      => Exp Int
      -> Exp Int
      -> Acc (Array (sh :. Int) e)
      -> Acc (Array (sh :. Int) e)
slit' m n acc =
  let sh :. sz  = unlift (shape acc)            :: Exp sh :. Exp Int
      index ix  = let j :. i = unlift ix        :: Exp sh :. Exp Int
                  in  lift (j :. i + m)
  in
  backpermute (lift (sh :. (n `min` ((sz - m) `max` 0)))) index acc


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
--}

type FissionAcc acc = forall aenv a. acc aenv a -> acc aenv a

{--
fissionPreOpenAcc
    :: forall acc aenv arrs. Arrays arrs
    => Int                      -- fission factor
    -> FissionAcc acc           -- parameterised via recursive closure
    -> PreOpenAcc acc aenv arrs
    -> PreOpenAcc acc aenv arrs
fissionPreOpenAcc k cvtA pacc =
  case pacc of
    _                   -> error "todo"
--    Map f a             -> fission1 (Map f) (cvtA a)

  where
--    fission1 :: (Shape sh, Elt a, Elt b)
--             => (acc aenv (Array sh a) -> PreOpenAcc acc aenv (Array sh b))
--             ->            acc aenv (Array sh a)
--             -> PreOpenAcc acc aenv (Array sh b)
--    fission1 c a = undefined
--}

-- When splitting an array 'acc' into 'k' pieces, what (outermost) index should
-- split point 'i' be?
--
splitIx :: forall sh e acc env aenv. (Slice sh, Shape sh, Elt e)
        => Int
        -> Int
        -> acc aenv (Array (sh :. Int) e)
        -> PreOpenExp acc env aenv Int
splitIx k' i' acc
  = Let n                                                                               -- n
  $ Let (PrimQuotRem int `app` tup2 (v z) k)                                            -- (chunk, leftover)
  $ Let (Prj (SuccTupIdx ZeroTupIdx) (v z))                                             -- chunk
  $ Let (Prj ZeroTupIdx (v (s z)))                                                      -- leftover
  $ Cond (PrimLt scalarType `app` tup2 i (v z))                                         -- i <* leftover
         (PrimMul num `app` tup2 i (PrimAdd num `app` tup2 (v (s z)) (constant 1)))     -- i * (chunk + 1)
         (PrimAdd num `app` tup2 (PrimMul num `app` tup2 i (v (s z))) (v z))            -- i * chunk + leftover
  where
      n         = IndexHead (Shape acc)
      k         = constant k'
      i         = constant i'
      i1        = constant (i'+1)


-- When splitting an array 'acc' into 'k' pieces, put into the environment as
-- the last two bound variables the split indices for the start and end of chunk
-- 'i'. Assumes that 'i < k'.
--
withSplitPts
    :: forall acc env aenv sh e t. (Slice sh, Shape sh, Elt e, Elt t)
    => Int
    -> Int
    -> acc aenv (Array (sh :. Int) e)
    -> PreOpenExp acc (((((((env, Int), (Int, Int)), Int), Int), Int), Int), Int) aenv t
    -> PreOpenExp acc env aenv t
withSplitPts k' i' acc cont
  = Let (IndexHead (Shape acc))                                                                         -- n
  $ Let (PrimQuotRem int `app` tup2 (v z) k)                                                            -- (chunk, leftover)
  $ Let (Prj (SuccTupIdx ZeroTupIdx) (v z))                                                             -- chunk
  $ Let (Prj ZeroTupIdx (v (s z)))                                                                      -- leftover
  $ Let (PrimAdd num `app` tup2 (v (s z)) (constant 1))                                                 -- chunk + 1
  $ Let (Cond (PrimLt scalarType `app` tup2 i (v (s z)))                                                -- if i <* leftover
              (PrimMul num `app` tup2 i (v z))                                                          --   then start = i * (chunk + 1)
              (PrimAdd num `app` tup2 (PrimMul num `app` tup2 i (v (s (s z)))) (v (s z))))              --   else start = i * chunk + leftover
  $ Let (Cond (PrimLt scalarType `app` tup2 i1 (v (s (s z))))                                           -- if i+1 <* leftover
              (PrimAdd num `app` tup2 (v z) (v (s (s (s z)))))                                          --   then end = start + chunk
              (PrimAdd num `app` tup2 (PrimMul num `app` tup2 i1 (v (s (s (s z))))) (v (s (s z)))))     --   else end = (i+1) * chunk + leftover
  $ cont
  where
    k   = constant k'
    i   = constant i'
    i1  = constant (i'+1)


bounds :: (Slice sh, Shape sh, Elt e)
       => Int
       -> Int
       -> acc aenv (Array (sh :. Int) e)
       -> PreOpenExp acc env aenv (Int, Int)
bounds k i acc
  = withSplitPts k i acc
  $ tup2 (v (s z)) (v z)


chunkA
    :: forall acc aenv sh e. (Slice sh, Shape sh, Elt e, Kit acc)
    => FissionAcc acc
    -> Int
    -> Int
    -> acc aenv (Array (sh :. Int) e)
    -> acc aenv (Array (sh :. Int) e)
chunkA _ k i acc
  = inject
  $ Backpermute sh' f acc
  where
    sh' = withSplitPts k i acc
        $ IndexCons (IndexTail (Shape acc))
                    (PrimSub num `app` tup2 (v z) (v (s z)))

    f   = Lam . Body
        $ withSplitPts k i acc
        $ IndexCons (IndexTail ix)
                    (PrimAdd num `app` tup2 (IndexHead ix) (v (s z)))
      where
        ix = v (s (s (s (s (s (s (s z)))))))

matchArrayShape
    :: forall acc aenv sh sh' e. (Shape sh, Shape sh')
    => acc aenv (Array sh e)
    -> sh'
    -> Maybe (sh :=: sh')
matchArrayShape _ _
  | Just REFL <- matchTupleType (eltType (undefined::sh)) (eltType (undefined::sh'))
  = gcast REFL

  | otherwise
  = Nothing

splitArray
  :: forall acc aenv sh e. (Shape sh, Elt e, Kit acc)
  => FissionAcc acc
  -> Int
  -> Int
  -> acc aenv (Array sh e)
  -> acc aenv (Array sh e)
splitArray cvtA k i acc
  | Just REFL <- matchArrayShape acc (undefined::Z)
  = acc

  | Just REFL <- matchArrayShape acc (undefined::DIM1)
  = chunkA cvtA k i acc

{--
fissionPreOpenAcc
    :: (Kit acc, Arrays arrs)
    => Int
    -> FissionAcc acc                                   -- parameterised via recursive closure
    -> PreOpenAcc acc aenv arrs
    -> PreOpenAcc acc aenv arrs
fissionPreOpenAcc k cvtA pacc =
  case pacc of
    Use a       -> Use a
    Map f a     ->
      let a'    = cvtA a
          a1    = inject $ Map f (splitArray cvtA k 0 a')
          a2    = inject $ Map (weaken SuccIdx f) (weaken SuccIdx (splitArray cvtA k 1 a'))
          r     = pconcat (inject (Avar (SuccIdx ZeroIdx))) (inject (Avar ZeroIdx))
      in
      Alet a1
      $ Alet a2
      $ r
{--
                   in Alet (inject $ Map f $
                            inject $ chunk cvtA 2 0 a') $ inject $
                      Alet (inject $ Map (weaken s f) $
                            inject $ chunk cvtA 2 1 (weaken s a')) $
                      pconcat (inject (Avar z)) (inject (Avar (s z)))
--}

    _           -> error "FIXME: Case not handled in fissionPreOpenAcc"
--}

--                       chunks   = map (\i -> chunk k i a') [0.. k-1]
--                       parts    = map (Let . Map f . weaken SuccIdx) chunks








-- infixr 5 ++
-- (++) :: forall sh e. (Slice sh, Shape sh, Elt e)
--      => Acc (Array (sh :. Int) e)
--      -> Acc (Array (sh :. Int) e)
--      -> Acc (Array (sh :. Int) e)
-- (++) xs ys
--   = let sh1 :. n        = unlift (shape xs)     :: Exp sh :. Exp Int
--         sh2 :. m        = unlift (shape ys)     :: Exp sh :. Exp Int
--     in
--     generate (lift (intersect sh1 sh2 :. n + m))
--              (\ix -> let sh :. i = unlift ix    :: Exp sh :. Exp Int
--                      in  i <* n ? ( xs ! ix, ys ! lift (sh :. i-n)) )

pconcat :: forall sh e acc env aenv. (Slice sh, Shape sh, Elt e)
           => acc aenv (Array (sh :. Int) e)
           -> acc aenv (Array (sh :. Int) e)
           -> acc aenv (Array (sh :. Int) e)
pconcat xs ys = undefined

{--
pconcat :: forall sh e acc env aenv. (Slice sh, Shape sh, Elt e)
           => acc aenv (Array (sh :. Int) e)
           -> acc aenv (Array (sh :. Int) e)
           -> acc aenv (Array (sh :. Int) e)
pconcat xs ys = Generate shape func
  where shape = Let (IndexHead (Shape xs)) $ -- n
                Let (IndexHead (Shape ys)) $ -- m
                Let (IndexTail (Shape xs)) $ -- sh1
                Let (IndexTail (Shape ys)) -- $ -- sh2
        func  = undefined
--}

-- Writing in abstract syntax is for chumps ):
--
constant :: Elt e => e -> PreOpenExp acc env aenv e
constant  = Const . fromElt

int :: IsIntegral a => IntegralType a
int = integralType

num :: IsNum a => NumType a
num = numType

app f x   = f `PrimApp` x
tup2 x y  = Tuple (NilTup `SnocTup` x `SnocTup` y)

v :: Elt t => Idx env t -> PreOpenExp acc env aenv t
v = Var
z = ZeroIdx
s = SuccIdx



-- Test
--------------------------------------------------------------------------------
{--
xs :: Vector Int
xs = A.fromList (Z :. 10) [0..]

xs' :: OpenAcc aenv (Vector Int)
xs' = OpenAcc (Use (fromArr xs))

ys :: OpenAcc () (Vector Int)
ys = Sharing.convertAcc True True True True
   $ A.enumFromN (A.constant (Z:.10)) (A.constant 0)

test1
  = T.convertOpenAcc True
  $ OpenAcc (Alet xs' (OpenAcc . Unit $ splitIx 3 3 (OpenAcc (Avar ZeroIdx))))

test2 k i
  = T.convertOpenAcc True
  $ OpenAcc $ Alet xs' (OpenAcc . Unit $ bounds k i (OpenAcc (Avar ZeroIdx)))

test3 k i
  = T.convertOpenAcc True
  $ OpenAcc $ Alet xs' (OpenAcc $ chunk k i (OpenAcc (Avar ZeroIdx)))

--}
