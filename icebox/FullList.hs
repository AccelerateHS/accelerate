{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.FullList
-- Copyright   : [2008..2017] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Non-empty lists of key/value pairs. The lists are strict in the key and lazy
-- in the values. We assume that keys only occur once.
--

module Data.Array.Accelerate.FullList (

  FullList(..),
  List(..),

  singleton,
  cons,
  size,
  mapM_,
  lookup,
  lookupDelete,

) where

import Prelude                  hiding ( lookup, mapM_ )


data FullList k v = FL !k v !(List k v)
data List k v     = Nil | Cons !k v !(List k v)

infixr 5 `Cons`

instance (Eq k, Eq v) => Eq (FullList k v) where
  (FL k1 v1 xs) == (FL k2 v2 ys)      = k1 == k2 && v1 == v2 && xs == ys
  (FL k1 v1 xs) /= (FL k2 v2 ys)      = k1 /= k2 || v1 /= v2 || xs /= ys

instance (Eq k, Eq v) => Eq (List k v) where
  (Cons k1 v1 xs) == (Cons k2 v2 ys) = k1 == k2 && v1 == v2 && xs == ys
  Nil == Nil = True
  _   == _   = False

  (Cons k1 v1 xs) /= (Cons k2 v2 ys) = k1 /= k2 || v1 /= v2 || xs /= ys
  Nil /= Nil = False
  _   /= _   = True


-- List-like operations
--
infixr 5 `cons`
cons :: k -> v -> FullList k v -> FullList k v
cons k v (FL k' v' xs) = FL k v (Cons k' v' xs)

singleton :: k -> v -> FullList k v
singleton k v = FL k v Nil

size :: FullList k v -> Int
size (FL _ _ xs) = 1 + sizeL xs

sizeL :: List k v -> Int
sizeL Nil           = 0
sizeL (Cons _ _ xs) = 1 + sizeL xs

lookup :: Eq k => k -> FullList k v -> Maybe v
lookup key (FL k v xs)
  | key == k    = Just v
  | otherwise   = lookupL key xs
{-# INLINABLE  lookup #-}
{-# SPECIALISE lookup :: () -> FullList () v -> Maybe v #-}

lookupL :: Eq k => k -> List k v -> Maybe v
lookupL !key = go
  where
    go Nil              = Nothing
    go (Cons k v xs)
      | key == k        = Just v
      | otherwise       = go xs
{-# INLINABLE  lookupL #-}
{-# SPECIALISE lookupL :: () -> List () v -> Maybe v #-}

lookupDelete :: Eq k => k -> FullList k v -> (Maybe v, Maybe (FullList k v))
lookupDelete key (FL k v xs)
  | key == k
  = case xs of
      Nil               -> (Just v, Nothing)
      Cons k' v' xs'    -> (Just v, Just $ FL k' v' xs')

  | (r, xs') <- lookupDeleteL k xs
  = (r, Just $ FL k v xs')
{-# INLINABLE  lookupDelete #-}
{-# SPECIALISE lookupDelete :: () -> FullList () v -> (Maybe v, Maybe (FullList () v)) #-}

lookupDeleteL :: Eq k => k -> List k v -> (Maybe v, List k v)
lookupDeleteL !key = go
  where
    go Nil                      = (Nothing, Nil)
    go (Cons k v xs)
      | key == k                = (Just v, xs)
      | (r, xs') <- go xs       = (r,      Cons k v xs')
{-# INLINABLE  lookupDeleteL #-}
{-# SPECIALISE lookupDeleteL :: () -> List () v -> (Maybe v, List () v) #-}

mapM_ :: Monad m => (k -> v -> m a) -> FullList k v -> m ()
mapM_ !f (FL k v xs) = f k v >> mapML_ f xs
{-# INLINABLE mapM_ #-}

mapML_ :: Monad m => (k -> v -> m a) -> List k v -> m ()
mapML_ !f = go
  where
    go Nil              = return ()
    go (Cons k v xs)    = f k v >> go xs
{-# INLINABLE mapML_ #-}

