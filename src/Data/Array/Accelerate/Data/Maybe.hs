{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Array.Accelerate.Data.Maybe
-- Copyright   : [2018..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- @since 1.2.0.0
--

module Data.Array.Accelerate.Data.Maybe (

  Maybe(..), pattern Nothing_, pattern Just_,
  just, nothing,
  maybe, isJust, isNothing, fromMaybe, fromJust, justs,

) where

import Data.Array.Accelerate.Analysis.Match
import Data.Array.Accelerate.Array.Sugar                            hiding ( (!), shape, ignore, toIndex )
import Data.Array.Accelerate.Language                               hiding ( chr )
import Data.Array.Accelerate.Prelude                                hiding ( filter )
import Data.Array.Accelerate.Interpreter
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.Classes.Eq
import Data.Array.Accelerate.Classes.Num
import Data.Array.Accelerate.Classes.Ord

import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Data.Monoid
#if __GLASGOW_HASKELL__ >= 800
import Data.Array.Accelerate.Data.Semigroup
#endif

import Data.Maybe                                                   ( Maybe(..) )
import Prelude                                                      ( ($), const, otherwise )


pattern Nothing_ :: Elt a => Exp (Maybe a)
pattern Nothing_ <- _
  where Nothing_ = nothing

pattern Just_ :: Elt a => Exp a -> Exp (Maybe a)
pattern Just_ <- _
  where Just_ = just

-- | Lift a value into a 'Just' constructor
--
just :: Elt a => Exp a -> Exp (Maybe a)
just x = lift (Just x)

-- | The 'Nothing' constructor
--
nothing :: forall a. Elt a => Exp (Maybe a)
nothing = lift (Nothing :: Maybe (Exp a))
--
-- Note: [lifting Nothing]
--
-- The lift instance for 'Nothing' uses our magic 'undef' term, meaning that our
-- backends will know that we can leave this slot in the values array undefined.
-- If we had instead written 'constant Nothing' this would result in writing an
-- actual (unspecified) value into the values array, which is what we want to
-- avoid.
--

-- | Returns 'True' if the argument is 'Nothing'
--
isNothing :: Elt a => Exp (Maybe a) -> Exp Bool
isNothing x = tag x == 0

-- | Returns 'True' if the argument is of the form @Just _@
--
isJust :: Elt a => Exp (Maybe a) -> Exp Bool
isJust x = tag x == 1

-- | The 'fromMaybe' function takes a default value and a 'Maybe' value. If the
-- 'Maybe' is 'Nothing', the default value is returned; otherwise, it returns
-- the value contained in the 'Maybe'.
--
fromMaybe :: Elt a => Exp a -> Exp (Maybe a) -> Exp a
fromMaybe d x = cond (isNothing x) d (fromJust x)

-- | The 'fromJust' function extracts the element out of the 'Just' constructor.
-- If the argument was actually 'Nothing', you will get an undefined value
-- instead.
--
fromJust :: Elt a => Exp (Maybe a) -> Exp a
fromJust (Exp x) = Exp $ SmartExp $ PairIdxRight `Prj` x

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value. If the 'Maybe' value is nothing, the default value is returned;
-- otherwise, it applies the function to the value inside the 'Just' and returns
-- the result
--
maybe :: (Elt a, Elt b) => Exp b -> (Exp a -> Exp b) -> Exp (Maybe a) -> Exp b
maybe d f x = cond (isNothing x) d (f (fromJust x))


-- | Extract from an array all of the 'Just' values, together with a segment
-- descriptor indicating how many elements along each dimension were returned.
--
justs :: (Shape sh, Slice sh, Elt a)
      => Acc (Array (sh:.Int) (Maybe a))
      -> Acc (Vector a, Array sh Int)
justs xs = filter' (map isJust xs) (map fromJust xs)


instance Functor Maybe where
  fmap f x = cond (isNothing x) Nothing_ (Just_ (f (fromJust x)))

instance Eq a => Eq (Maybe a) where
  ma == mb = cond (isNothing ma && isNothing mb) True_
           $ cond (isJust ma    && isJust mb)    (fromJust ma == fromJust mb)
           $ False_

instance Ord a => Ord (Maybe a) where
  compare ma mb = cond (isJust ma && isJust mb)
                       (compare (fromJust ma) (fromJust mb))
                       (compare (tag ma) (tag mb))

instance (Monoid (Exp a), Elt a) => Monoid (Exp (Maybe a)) where
  mempty        = Nothing_
#if __GLASGOW_HASKELL__ < 804
  mappend ma mb = cond (isNothing ma) mb
                $ cond (isNothing mb) ma
                $ lift (Just (fromJust ma `mappend` fromJust mb))
#endif

#if __GLASGOW_HASKELL__ >= 800
instance (Semigroup (Exp a), Elt a) => Semigroup (Exp (Maybe a)) where
  ma <> mb = cond (isNothing ma) mb
           $ cond (isNothing mb) mb
           $ lift (Just (fromJust ma <> fromJust mb))
#endif


tag :: Elt a => Exp (Maybe a) -> Exp Word8
tag (Exp x) = Exp $ SmartExp $ Prj PairIdxRight $ SmartExp $ Prj PairIdxLeft x


instance Elt a => Elt (Maybe a) where
  type EltRepr (Maybe a) = TupleRepr (Word8, EltRepr a)
  {-# INLINE eltType     #-}
  {-# INLINE [1] toElt   #-}
  {-# INLINE [1] fromElt #-}
  eltType          = eltType @(Word8,a)
  toElt (((),0),_) = Nothing
  toElt (_     ,x) = Just (toElt x)
  fromElt Nothing  = (((),0), evalUndef $ eltType @a)
  fromElt (Just a) = (((),1), fromElt a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Maybe a) where
  type Plain (Maybe a) = Maybe (Plain a)
  lift Nothing  = Exp $ SmartExp $ Pair t $ unExp $ undef @(Plain a)
    where
      t = SmartExp $ Pair (SmartExp Nil) $ SmartExp $ Const scalarTypeWord8 0
  lift (Just x) = Exp $ SmartExp $ Pair t $ unExp $ lift x
    where
      t = SmartExp $ Pair (SmartExp Nil) $ SmartExp $ Const scalarTypeWord8 1


-- Utilities
-- ---------

filter'
    :: forall sh e. (Shape sh, Slice sh, Elt e)
    => Acc (Array (sh:.Int) Bool)     -- tags
    -> Acc (Array (sh:.Int) e)        -- values
    -> Acc (Vector e, Array sh Int)
filter' keep arr
  | Just Refl <- matchShapeType @sh @Z
  = let
        (target, len)   = unlift $ scanl' (+) 0 (map boolToInt keep)
        prj ix          = keep!ix ? ( index1 (target!ix), ignore )
        dummy           = fill (index1 (the len)) undef
        result          = permute const dummy prj arr
    in
    null keep ?| ( lift (emptyArray, fill (constant Z) 0)
                 , lift (result, len)
                 )
  | otherwise
  = let
        sz              = indexTail (shape arr)
        (target, len)   = unlift $ scanl' (+) 0 (map boolToInt keep)
        (offset, valid) = unlift $ scanl' (+) 0 (flatten len)
        prj ix          = cond (keep!ix)
                               (index1 $ offset!index1 (toIndex sz (indexTail ix)) + target!ix)
                               ignore
        dummy           = fill (index1 (the valid)) undef
        result          = permute const dummy prj arr
    in
    null keep ?| ( lift (emptyArray, fill sz 0)
                 , lift (result, len)
                 )

emptyArray :: (Shape sh, Elt e) => Acc (Array sh e)
emptyArray = fill (constant empty) undef

