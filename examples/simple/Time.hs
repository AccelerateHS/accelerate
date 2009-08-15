-- |Auxiliary functions to time benchmarks
--
--  Copyright (c) [2007..2009] Roman Leshchinskiy, Manuel M T Chakravarty
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------

module Time (
  Time,
  getTime,
  wallTime, cpuTime,
  picoseconds, milliseconds, seconds,

  minus, plus, div,
  min, max, avg,
  sum, minimum, maximum, average,
  
  showTime, showMinAvgMax
) where

import System.CPUTime
import System.Time

import Prelude hiding (div, min, max, sum, minimum, maximum)
import qualified Prelude as P

infixl 6 `plus`, `minus`
infixl 7 `div`

data Time = Time { cpu_time  :: Integer
                 , wall_time :: Integer
                 }

type TimeUnit = Integer -> Integer

picoseconds :: TimeUnit
picoseconds = id

milliseconds :: TimeUnit
milliseconds n = n `P.div` 1000000000

seconds :: TimeUnit
seconds n = n `P.div` 1000000000000

cpuTime :: TimeUnit -> Time -> Integer
cpuTime f = f . cpu_time

wallTime :: TimeUnit -> Time -> Integer
wallTime f = f . wall_time

getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 wall1) (Time cpu2 wall2) =
  Time (f cpu1 cpu2) (f wall1 wall2)

minus :: Time -> Time -> Time
minus = zipT (-)

plus :: Time -> Time -> Time
plus = zipT (+)

div :: Time -> Int -> Time
div (Time cpu clock) n = Time (cpu `P.div` n') (clock `P.div` n')
  where
    n' = toInteger n

min :: Time -> Time -> Time
min = zipT P.min

max :: Time -> Time -> Time
max = zipT P.max

avg :: Time -> Time -> Time
avg t1 t2 = (t1 `plus` t2) `div` 2

sum :: [Time] -> Time
sum = foldr1 plus

minimum :: [Time] -> Time
minimum = foldr1 min

maximum :: [Time] -> Time
maximum = foldr1 max

average :: [Time] -> Time
average ts = sum ts `div` length ts

showTime :: TimeUnit -> Time -> String
showTime f t = show (wallTime f t) ++ "; " ++ show (cpuTime f t)
  
showMinAvgMax :: TimeUnit -> [Time] -> String
showMinAvgMax f ts = show (wallTime f min) ++ "/" ++ 
                     show (wallTime f avg) ++ "/" ++
                     show (wallTime f max) ++ " - " ++
                     show (cpuTime f min)  ++ "/" ++ 
                     show (cpuTime f avg)  ++ "/" ++
                     show (cpuTime f max)
  where
    min = minimum ts
    avg = average ts
    max = maximum ts
