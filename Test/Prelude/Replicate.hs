{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Replicate (

  test_replicate,

) where

import Config
import Test.Framework

import Prelude                                                  as P
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.IO.Data.Array.Unboxed              as A
import Data.Array.Accelerate.Examples.Internal                  as A
import Data.Array.Unboxed                                       as IArray hiding ( Array )
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.HUnit                                               hiding ( Test )


--
-- Slice -----------------------------------------------------------------------
--

test_replicate :: Backend -> Config -> Test
test_replicate backend opt = testGroup "replicate" $ catMaybes
  [ testElt configInt8   (undefined :: Int8)
  , testElt configInt16  (undefined :: Int16)
  , testElt configInt32  (undefined :: Int32)
  , testElt configInt64  (undefined :: Int64)
  , testElt configWord8  (undefined :: Word8)
  , testElt configWord16 (undefined :: Word16)
  , testElt configWord32 (undefined :: Word32)
  , testElt configWord64 (undefined :: Word64)
  , testElt configFloat  (undefined :: Float)
  , testElt configDouble (undefined :: Double)
  ]
  where
    testElt :: forall e. (P.Num e, A.Num e, P.Eq e, IArray UArray e) => (Config :-> Bool) -> e -> Maybe Test
    testElt ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: e)))
          [
            testCase "(Z:.2:.All:.All)" $ ref1 @=? run' test1
          , testCase "(Z:.All:.2:.All)" $ ref2 @=? run' test2
          , testCase "(Z:.All:.All:.2)" $ ref3 @=? run' test3
          , testCase "(Any:.2)"         $ ref4 @=? run' test4
          ]

      where
        arr :: Acc (Array DIM2 e)
        arr = use $ fromList (Z:.2:.2) [1,2,3,4]

        run' = toUArray Nothing . run backend

        -- Replicate into z-axis
        -- should produce [1,2,3,4,1,2,3,4]
        test1 :: Acc (Array DIM3 e)
        test1 = A.replicate slice1 arr

        slice1 :: Exp (Z:.Int:.All:.All)
        slice1 = lift $ Z:.(2::Int):.All:.All

        ref1 :: UArray (Int,Int,Int) e
        ref1 = array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                       , ((0,0,1), 2)
                                       , ((0,1,0), 3)
                                       , ((0,1,1), 4)
                                       , ((1,0,0), 1)
                                       , ((1,0,1), 2)
                                       , ((1,1,0), 3)
                                       , ((1,1,1), 4) ]

        -- Replicate into y-axis
        -- should produce [1,2,1,2,3,4,3,4]
        test2 :: Acc (Array DIM3 e)
        test2 =  A.replicate slice2 arr

        slice2 :: Exp (Z:.All:.Int:.All)
        slice2 = lift $ Z:.All:.(2::Int):.All

        ref2 :: UArray (Int,Int,Int) e
        ref2 = array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                       , ((0,0,1), 2)
                                       , ((0,1,0), 1)
                                       , ((0,1,1), 2)
                                       , ((1,0,0), 3)
                                       , ((1,0,1), 4)
                                       , ((1,1,0), 3)
                                       , ((1,1,1), 4) ]

        -- Replicate into x-axis
        -- should produce [1,1,2,2,3,3,4,4]
        test3 :: Acc (Array DIM3 e)
        test3 =  A.replicate slice3 arr

        slice3 :: Exp (Z:.All:.All:.Int)
        slice3 = lift $ Z:.All:.All:.(2::Int)

        ref3 :: UArray (Int,Int,Int) e
        ref3 = array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                       , ((0,0,1), 1)
                                       , ((0,1,0), 2)
                                       , ((0,1,1), 2)
                                       , ((1,0,0), 3)
                                       , ((1,0,1), 3)
                                       , ((1,1,0), 4)
                                       , ((1,1,1), 4) ]

        -- Replicates an array into the rightmost dimension of the result array.
        --
        repN :: forall sh a. (Shape sh, Elt a)
             => Int
             -> Acc (Array sh a)
             -> Acc (Array (sh:.Int) a)
        repN n a = A.replicate (lift (Any:.n :: Any sh:.Int)) a

        test4 :: Acc (Array DIM3 e)
        test4 = repN 2 arr

        ref4 :: UArray (Int,Int,Int) e
        ref4 = array ((0,0,0),(1,1,1)) [ ((0,0,0), 1)
                                       , ((0,0,1), 1)
                                       , ((0,1,0), 2)
                                       , ((0,1,1), 2)
                                       , ((1,0,0), 3)
                                       , ((1,0,1), 3)
                                       , ((1,1,0), 4)
                                       , ((1,1,1), 4) ]

