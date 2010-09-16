{-# LANGUAGE NoMonomorphismRestriction, CPP, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Properties where

import Data.Bits
import Test.QuickCheck                             hiding ((.&.))
import qualified Test.QuickCheck                   as QC

import Data.Int
import Data.Word
import Data.List
import Foreign.C.Types

import Data.Array.Accelerate                       (Elem, Acc, Vector, Array)
import Data.Array.Accelerate.Test.QuickCheck

import qualified Data.Array.Accelerate             as Acc
import qualified Data.Array.Accelerate.Interpreter as Interp

#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA        as CUDA
#endif

-- friends
import qualified Function                          as Acc


-- Auxiliary functions
-- -------------------

class Similar a where
  sim :: a -> a -> Bool

instance Similar a => Similar [a] where
  sim xs ys = and (zipWith sim xs ys)

instance (Similar a, Similar b) => Similar (a,b) where
  sim (x1,y1) (x2,y2) = x1 `sim` x2 && y1 `sim` y2

instance Similar Int     where sim = (==)
instance Similar Int8    where sim = (==)
instance Similar Int16   where sim = (==)
instance Similar Int32   where sim = (==)
instance Similar Int64   where sim = (==)
instance Similar Word    where sim = (==)
instance Similar Word8   where sim = (==)
instance Similar Word16  where sim = (==)
instance Similar Word32  where sim = (==)
instance Similar Word64  where sim = (==)
instance Similar CShort  where sim = (==)
instance Similar CUShort where sim = (==)
instance Similar CInt    where sim = (==)
instance Similar CUInt   where sim = (==)
instance Similar CLong   where sim = (==)
instance Similar CULong  where sim = (==)
instance Similar CLLong  where sim = (==)
instance Similar CULLong where sim = (==)

instance Similar Float   where sim = (=~) 0.001
instance Similar CFloat  where sim = (=~) 0.001
instance Similar Double  where sim = (=~) 0.00001
instance Similar CDouble where sim = (=~) 0.00001

instance Similar Bool    where sim = (==)
instance Similar Char    where sim = (==)
instance Similar CChar   where sim = (==)
instance Similar CSChar  where sim = (==)
instance Similar CUChar  where sim = (==)


(=~) :: (Fractional a, Ord a) => a -> a -> a -> Bool
(=~) epsilon x y = abs ((x-y) / (x+y+epsilon)) < epsilon

trivial :: Testable a => Bool -> a -> Property
trivial = (`classify` "trivial")

vec :: Elem a => [a] -> Acc (Vector a)
vec xs = Acc.use (Acc.fromList (length xs) xs)



eq1 :: (Elem a, Similar b) => ([a] -> [b]) -> (Acc (Vector a) -> Acc (Array dim b)) -> [a] -> Property
eq1 f g xs = not (null xs) ==>
  let ref    = f xs
      expr   = g (vec xs)
      interp = Acc.toList (Interp.run expr)
#ifdef ACCELERATE_CUDA_BACKEND
      cuda   = Acc.toList (CUDA.run expr)
  in (ref `sim` interp) QC..&. (ref `sim` cuda)
#else
  in (ref `sim` interp)
#endif


eq2 :: (Elem a, Elem b, Similar c)
    => ([a] -> [b] -> [c])
    -> (Acc (Vector a) -> Acc (Vector b) -> Acc (Array dim c)) -> [a] -> [b] -> Property
eq2 f g xs ys = not (null xs) && not (null ys) ==>
  let ref    = f xs ys
      expr   = g (vec xs) (vec ys)
      interp = Acc.toList (Interp.run expr)
#ifdef ACCELERATE_CUDA_BACKEND
      cuda   = Acc.toList (CUDA.run expr)
  in (ref `sim` interp) QC..&. (ref `sim` cuda)
#else
  in (ref `sim` interp)
#endif


toRange :: (RealFrac a, Fractional a) => (a,a) -> a -> a
toRange (m,n) x = let (_::Int,f) = properFraction x
                  in  abs f * (n-m) + m


-- Primitive Functions
-- -------------------

-- operators from Num
prop_Add      = ("(+)",    zipWith (+) `eq2` Acc.zipWith (+))
prop_Sub      = ("(-)",    zipWith (-) `eq2` Acc.zipWith (-))
prop_Mul      = ("(*)",    zipWith (*) `eq2` Acc.zipWith (*))
prop_Abs      = ("abs",    map abs     `eq1` Acc.map abs)
prop_Negate   = ("negate", map negate  `eq1` Acc.map negate)
prop_Signum   = ("signum", map signum  `eq1` Acc.map signum)

-- operators from Integral and Bits
prop_Quot     = ("quot",       \xs ys -> all (/= 0) ys ==> eq2 (zipWith quot) (Acc.zipWith quot) xs ys)
prop_Rem      = ("rem",        \xs ys -> all (/= 0) ys ==> eq2 (zipWith rem)  (Acc.zipWith rem)  xs ys)
prop_Idiv     = ("div",        \xs ys -> all (/= 0) ys ==> eq2 (zipWith div)  (Acc.zipWith div)  xs ys)
prop_Mod      = ("mod",        \xs ys -> all (/= 0) ys ==> eq2 (zipWith mod)  (Acc.zipWith mod)  xs ys)
prop_Band     = ("(.&.)",      zipWith (.&.)  `eq2` Acc.zipWith (.&.))
prop_BOr      = ("(.|.)",      zipWith (.|.)  `eq2` Acc.zipWith (.|.))
prop_BXor     = ("xor",        zipWith xor    `eq2` Acc.zipWith xor)
prop_BNot     = ("complement", map complement `eq1` Acc.map complement)
prop_BShiftL  = ("shiftL",     forAll (listOf (choose (0,31))) . (zipWith shiftL  `eq2` Acc.zipWith Acc.shiftL))
prop_BShiftR  = ("shiftR",     forAll (listOf (choose (0,31))) . (zipWith shiftR  `eq2` Acc.zipWith Acc.shiftR))
prop_BRotateL = ("rotateL",    forAll (listOf (choose (0,31))) . (zipWith rotateL `eq2` Acc.zipWith Acc.rotateL))
prop_BRotateR = ("rotateR",    forAll (listOf (choose (0,31))) . (zipWith rotateR `eq2` Acc.zipWith Acc.rotateR))

-- operators from Int
prop_intToFloat         = ("intToFloat",         map fromIntegral `eq1` Acc.map Acc.intToFloat)
prop_roundFloatToInt    = ("roundFloatToInt",    map round        `eq1` Acc.map Acc.roundFloatToInt)
prop_truncateFloatToInt = ("truncateFloatToInt", map truncate     `eq1` Acc.map Acc.truncateFloatToInt)

-- operators from Fractional, Floating, RealFrac & RealFloat
prop_FDiv     = ("(/)",     \xs ys -> all (/= 0) ys ==> eq2 (zipWith (/)) (Acc.zipWith (/)) xs ys)
prop_Recip    = ("recip",   \xs    -> all (/= 0) xs ==> eq1 (map recip) (Acc.map recip) xs)
prop_Sin      = ("sin",     map sin `eq1` Acc.map sin)
prop_Cos      = ("cos",     map cos `eq1` Acc.map cos)
prop_Tan      = ("tan",     map tan `eq1` Acc.map tan)
prop_ASin     = ("asin",    (map asin `eq1` Acc.map asin) . map (toRange (-1,1)))
prop_ACos     = ("acos",    (map acos `eq1` Acc.map acos) . map (toRange (-1,1)))
prop_ATan     = ("atan",    map atan  `eq1` Acc.map atan)
prop_ASinh    = ("asinh",   map asinh `eq1` Acc.map asinh)
prop_ACosh    = ("acosh",   (map acosh `eq1` Acc.map acosh) . map ((+1) . abs))
prop_ATanh    = ("atanh",   (map atanh `eq1` Acc.map atanh) . map (toRange (-1,1)))
prop_Exp      = ("exp",     (map exp   `eq1` Acc.map exp)   . map (toRange (-42,42)))
prop_Sqrt     = ("sqrt",    (map sqrt  `eq1` Acc.map sqrt)  . map abs)
prop_Log      = ("log",     \xs -> all (/= 0) xs ==> eq1 (map log) (Acc.map log) (map abs xs))
prop_Pow      = ("(**)",    \xs ys -> eq2 (zipWith (**)) (Acc.zipWith (**)) (map (toRange (0,12)) xs) (map (toRange (-21,21)) ys))
prop_LogBase  = ("logBase", \xs ys -> all (/= 0) xs && all (/= 0) ys ==> eq2 (zipWith logBase) (Acc.zipWith logBase) (map abs xs) (map abs ys))

-- relational and equality
prop_Lt       = ("(<)",  zipWith (<)  `eq2` Acc.zipWith (Acc.<*))
prop_Gt       = ("(>)",  zipWith (>)  `eq2` Acc.zipWith (Acc.>*))
prop_LtEq     = ("(<=)", zipWith (<=) `eq2` Acc.zipWith (Acc.<=*))
prop_GtEq     = ("(>=)", zipWith (>=) `eq2` Acc.zipWith (Acc.>=*))
prop_Eq       = ("(==)", zipWith (==) `eq2` Acc.zipWith (Acc.==*))
prop_NEq      = ("(/=)", zipWith (/=) `eq2` Acc.zipWith (Acc./=*))
prop_Min      = ("min",  zipWith min  `eq2` Acc.zipWith Acc.min)
prop_Max      = ("max",  zipWith max  `eq2` Acc.zipWith Acc.max)

-- logical operators
prop_LAnd     = ("(&&)", zipWith (&&) `eq2` Acc.zipWith (Acc.&&*))
prop_LOr      = ("(||)", zipWith (||) `eq2` Acc.zipWith (Acc.||*))
prop_LNot     = ("not",  map not `eq1` Acc.map Acc.not)


-- Array computations
-- ------------------

unit x           = [x]
backpermute v is = [ v!!i | i <- is]
foldSeg v        = snd . mapAccumL (\a i -> let (x,r) = splitAt i a in (r,sum x)) v

prop_Sum         = ("sum",               (unit . sum)     `eq1` Acc.fold (+) 0)
prop_Product     = ("product",           (unit . product) `eq1` Acc.fold (*) 1)
prop_Minimum     = ("minimum",           (unit . minimum) `eq1` (\arr -> Acc.fold Acc.min (arr Acc.! 0) arr))
prop_Maximum     = ("maximum",           (unit . maximum) `eq1` (\arr -> Acc.fold Acc.max (arr Acc.! 0) arr))
prop_Zip         = ("zip",               zip `eq2` Acc.zip)
prop_FstUnzip    = ("fst . unzip",       (fst . unzip) `eq1` (fst . Acc.unzip))
prop_SndUnzip    = ("snd . unzip",       (snd . unzip) `eq1` (snd . Acc.unzip))
prop_Backpermute = ("backpermute",       \xs -> forAll (listOf (choose (0,length xs -1))) (eq2 backpermute (\ax ai -> Acc.backpermute (Acc.shape ai) (ai Acc.!) ax) xs))
prop_Scanl       = ("scanl (+) 0",       (init . scanl (+) 0) `eq1` (fst . Acc.scanl (+) 0))
prop_ScanlRdx    = (" ... reduction",    (unit . last . scanl (+) 0) `eq1` (snd . Acc.scanl (+) 0))
prop_Scanr       = ("scanr (+) 0",       (tail . scanr (+) 0) `eq1` (fst . Acc.scanr (+) 0))
prop_ScanrRdx    = (" ... reduction",    (unit . head . scanr (+) 0) `eq1` (snd . Acc.scanr (+) 0))
prop_Square      = ("square",            map (\x -> x*x) `eq1` Acc.map (\x -> x*x))
prop_Saxpy       = ("saxpy",             \a -> (zipWith (\x y -> a*x + y) `eq2` Acc.zipWith (\x y -> Acc.constant a * x + y)))
prop_Dotp        = ("dotp",              (\xs ys -> unit . sum $ zipWith (*) xs ys) `eq2` (\xs ys -> Acc.fold (+) 0 $ Acc.zipWith (*) xs ys))
prop_Filter      = ("filter (< 0)",      filter (< 0) `eq1` Acc.filter (Acc.<* 0))
prop_MapAddPair  = ("map (uncurry (+))", map (uncurry (+)) `eq1` Acc.map (Acc.uncurry (+)))
prop_ScanlPair   = ("scanl (+,*) (0,1)", (init . scanl (\a b -> (fst a + fst b, snd a * snd b)) (0,1)) `eq1` (fst . Acc.scanl (\a b -> Acc.tuple (Acc.fst a + Acc.fst b, Acc.snd a * Acc.snd b)) (Acc.constant (0,1))))
prop_ScanrPair   = ("scanr (+,*) (0,1)", (tail . scanr (\a b -> (fst a + fst b, snd a * snd b)) (0,1)) `eq1` (fst . Acc.scanr (\a b -> Acc.tuple (Acc.fst a + Acc.fst b, Acc.snd a * Acc.snd b)) (Acc.constant (0,1))))

prop_FoldSeg :: forall a. (Arbitrary a, Similar a, Acc.IsNum a, Elem a) => (String, [a] -> Property)
prop_FoldSeg = ("foldSeg", const $ (map (\i -> abs i `rem` 250) `fmap` listOf arbitrary) >>=
                                   \is -> forAll (vector (sum is) :: Gen [a]) (\xs -> eq2 foldSeg (Acc.foldSeg (+) 0) xs is))

-- Arbitrarily Generated
-- ---------------------

#ifdef ACCELERATE_CUDA_BACKEND
test_arbitrary :: forall e. (Arbitrary (Acc (Vector e)), Similar e) => e -> Property
test_arbitrary _ =
  forAll (arbitrary :: Gen (Acc (Vector e))) $ \expr ->
    let interp = Acc.toList (Interp.run expr)
        cuda   = Acc.toList (CUDA.run expr)
    in
    interp `sim` cuda
#endif

