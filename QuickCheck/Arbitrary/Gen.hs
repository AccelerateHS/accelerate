{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Inspired by the post:
--   http://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/

module QuickCheck.Arbitrary.Gen where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Test.QuickCheck.Gen
import Test.QuickCheck.Random


-- A monad onion to keep track of the size of a thing being generated.
--
-- The ReaderT holds the minimum and maximum allowable sizes, the StateT holds
-- the current size, and the MaybeT allows for possible failure (if the size is
-- too big or too small). Rand QCGen is of course for generating random numbers.
--
newtype GenM a =
  GenM {
    unGenM :: ReaderT (Int,Int) (StateT Int (MaybeT (RandT QCGen Identity))) a
  }
  deriving (Functor, Alternative, Applicative, Monad, MonadPlus, MonadRandom, MonadState Int, MonadReader (Int,Int))


-- To run a computation in the monad, we take a target size and a tolerance and
-- use that to compute the minimum and maximum sizes.
--
runGenM :: Int -> Double -> GenM a -> IO (Maybe a)
runGenM target eps m = do
  let
      wiggle    = round $ fromIntegral target * eps
      minsize   = (target - wiggle) `max` 0
      maxsize   = (target + wiggle) `min` 1
  --
  g     <- newQCGen
  return . (evalRand ?? g)
         . runMaybeT
         . (evalStateT ?? 0)
         . (runReaderT ?? (minsize, maxsize))
         $ unGenM m


-- Using the generator from QuickCheck
--
mkGen :: Double -> GenM a -> Gen (Maybe a)
mkGen eps m = MkGen $ \g target ->
  let
      wiggle    = round $ fromIntegral target * eps
      minsize   = target - wiggle
      maxsize   = target + wiggle
  in
  (evalRand ?? g)
    . runMaybeT
    . (evalStateT ?? 0)
    . (runReaderT ?? (minsize, maxsize))
    $ unGenM m


-- Handles failing early if the size gets too big
--
atom :: GenM ()
atom = do
  (_, maxsize)  <- ask
  cursize       <- get
  when (cursize > maxsize) mzero
  put  (cursize + 1)


-- This is convenient to 'flip' argument order of composite functions.
-- Stolen from the 'lens' package by Edward A. Kmett.
--
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}


{--

data Tree = Leaf | Branch Tree Tree
  deriving Show

size :: Tree -> Int
size Leaf = 1
size (Branch l r) = 1 + size l + size r

genTreeUB :: GenM Tree
genTreeUB = do
  r <- getRandom
  atom
  if r <= (1/2 :: Double)
    then return Leaf
    else Branch <$> genTreeUB <*> genTreeUB

genTreeLB :: GenM Tree
genTreeLB = do
  put 0
  t <- genTreeUB
  tSize <- get
  (minSize, _) <- ask
  guard $ tSize >= minSize
  return t

genTree :: GenM Tree
genTree = genTreeLB `mplus` genTree


randomTree :: Gen Tree
randomTree = oneof [ return Leaf, liftM2 Branch randomTree randomTree ]
--}

