{-# LANGUAGE GADTs, BangPatterns, PatternGuards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
-- |
-- Module      : Data.Array.Accelerate.CUDA
-- Copyright   : [2008..2009] Manuel M T Chakravarty, Gabriele Keller, Sean Lee
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module is the CUDA backend for the embedded array language.

module Data.Array.Accelerate.CUDA (

  -- * Generate CUDA code and execute it
  Arrays, run
  
) where

-- standard libraries
import Control.Monad.State
import Data.Bits
import Data.Char            (chr, ord)
import Data.Map             as M (empty, insert, lookup, member)
import Data.Maybe           (fromJust)
import Foreign
import Foreign.Ptr          (WordPtr)
import System.Exit          (ExitCode(ExitSuccess), exitFailure)
import System.Posix.Process (
  ProcessStatus(..), executeFile, forkProcess, getProcessStatus)
import System.Posix.Types   (ProcessID)
import Data.ByteString.Char8 as B

-- friends
import Data.Array.Accelerate.Analysis.Type
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation
import Data.Array.Accelerate.Array.Sugar (
  Array(..), Scalar, Vector, Segments, DIM0)
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar

import qualified Data.Array.Accelerate.CUDA.Data    as CUDA
import qualified Data.Array.Accelerate.CUDA.Monad   as CUDA
import qualified Data.Array.Accelerate.CUDA.Scalar  as CUDA
import qualified Data.Array.Accelerate.CUDA.Syntax  as CUDA
import qualified Data.Array.Accelerate.CUDA.Fold    as CUDA
import qualified Data.Array.Accelerate.CUDA.Map     as CUDA
import qualified Data.Array.Accelerate.CUDA.ZipWith as CUDA

import qualified Foreign.CUDA.Driver as CUDA (
  AllocFlag(..), Context(..), ContextFlag(..), Device(..), FunParam(..),
  count, create, destroy, device, devPtrToWordPtr, getFun, initialise, launch,
  loadDataEx, maxGridSize, nullDevPtr, pop, props, setBlockShape, setParams,
  setSharedSize, sync)

-- Program execution
-- -----------------

-- |Characterises the types that may be returned when running an array program.
--
class Delayable as => Arrays as where
  
instance Arrays ()  
instance Arrays (Array dim e)
instance (Arrays as1, Arrays as2) => Arrays (as1, as2)

-- |Compiles and runs a complete embedded array program using the CUDA backend.
--
run :: Arrays a => Sugar.Acc a -> IO a
run acc = do
  let ast       = Sugar.convertAcc acc
      initState = CUDA.CGState 0 0 M.empty M.empty M.empty M.empty
  (device, context) <- initCUDA
  (_,             state   ) <- runStateT (memHtoD    ast)  initState
  (_,             state'  ) <- runStateT (codeGenAcc ast)      state
  (arr@(_, arr'), state'' ) <- runStateT (execute    ast)      state'
  (_,             state''') <- runStateT (memDtoH    ast arr)  state''
  finalise
  Prelude.putStrLn $ (show $ CUDA.dataTransferHtoD state''')
                  ++ " host-to-device data transfer(s) triggered."
  return arr'

-- |Initialises the CUDA device and the context.
--
initCUDA :: IO (CUDA.Device, CUDA.Context)
initCUDA = do
  CUDA.initialise []
  count   <- CUDA.count
  if count == 0
    then finalise >> error "No CUDA device found."
    else do
      device  <- CUDA.device 0
      context <- CUDA.create device [CUDA.SchedAuto]
      return (device, context)

-- |Finalises the CUDA device and the context.
--
finalise :: IO ()
finalise = CUDA.pop >>= CUDA.destroy

-- |Executes the generated code
--
execute :: (Delayable a)
        => OpenAcc aenv a -> CUDA.CGIO ([CUDA.FunParam WordPtr], a)
execute op@(Fold fun def xs) = do
  currentState <- get
  let foldMap = CUDA.foldMap currentState
      key     = show fun
  case M.lookup key foldMap of
    Just  v -> do
      getCompilerProcessStatus (CUDA.compilerPID v)
      props  <- liftIO $ CUDA.device 0 >>= CUDA.props
      ptx    <- liftIO $ B.readFile (CUDA.progName v ++ ".ptx")
      (m, _) <- liftIO $ CUDA.loadDataEx ptx []
      fun    <- liftIO $ CUDA.getFun m ("_" ++ CUDA.progName v ++ "Scan4")
      xs'@(xsFunParams, Array sh rf) <- execute xs
      let (maxGridSizeX, maxGridSizeY, maxGridSizeZ) = CUDA.maxGridSize props
          n              = size sh
          ctaSize        = 128
          numElemsPerCTA = ctaSize * 8
          ys@(Array newSh newRf) = newArray_ (Sugar.toElem ())
          execute' p e xsFunParams' n' = do
            let isFullBlock = if n' `mod` numElemsPerCTA == 0 then 1 else 0
                numBlocks   = (n' + numElemsPerCTA - 1) `div` numElemsPerCTA
                gridDim     = ( min maxGridSizeX numBlocks
                              , (numBlocks + maxGridSizeY - 1) `div` maxGridSizeY)
            dptr <- CUDA.mallocArray newRf numBlocks
            let ysFunParams = CUDA.toFunParams newRf dptr
            liftIO $ CUDA.setSharedSize fun (fromIntegral $ 256 * sizeOf (undefined::Float))
            liftIO $ CUDA.setParams fun
              ([CUDA.IArg e, CUDA.FArg 0] ++ xsFunParams' ++ ysFunParams ++
              [CUDA.IArg n', CUDA.IArg isFullBlock])
            liftIO $ CUDA.setBlockShape fun (ctaSize, 1, 1)
            liftIO $ CUDA.launch fun gridDim Nothing
            liftIO $ CUDA.sync
            if p == 0
              then CUDA.decUse rf >>= \ decUse -> if decUse == 0
                then CUDA.free rf $ CUDA.fromFunParams rf xsFunParams'
                else return ()
              else CUDA.free newRf $ CUDA.fromFunParams newRf xsFunParams'
            if numBlocks == 1
              then return ysFunParams
              else execute' (p + 1) 0 ysFunParams numBlocks
      ysFunParams <- execute' 0 1 xsFunParams (n + 1)
      return (ysFunParams, ys)
    Nothing ->
      error $ "Code generation for \n\t" ++ show op ++ "\nfailed."
execute op@(Map fun xs) = do
  currentState <- get
  let mapMap = CUDA.mapMap currentState
      key    = show fun
  case M.lookup key mapMap of
    Just v -> do
      getCompilerProcessStatus (CUDA.compilerPID v)
      props  <- liftIO $ CUDA.device 0 >>= CUDA.props
      ptx    <- liftIO $ B.readFile (CUDA.progName v ++ ".ptx")
      (m, _) <- liftIO $ CUDA.loadDataEx ptx []
      fun    <- liftIO $ CUDA.getFun m ("_" ++ CUDA.progName v)
      xs'@(xsFunParams, Array newSh rf) <- execute xs
      let (maxGridSizeX, maxGridSizeY, maxGridSizeZ) = CUDA.maxGridSize props
          n              = size sh
          ctaSize        = 128
          numElemsPerCTA = ctaSize * 2
          numBlocks      = (n + numElemsPerCTA - 1) `div` numElemsPerCTA
          gridDim        = ( min maxGridSizeX numBlocks
                           , (numBlocks + maxGridSizeY - 1) `div` maxGridSizeY)
          isFullBlock    = if n `mod` numElemsPerCTA == 0 then 1 else 0
          ys@(Array sh rf) = newArray_ (Sugar.toElem newSh)
      dptr <- CUDA.mallocArray rf (size sh)
      let ysFunParams = CUDA.toFunParams rf dptr
      liftIO $ CUDA.setParams fun
        (xsFunParams ++ ysFunParams ++
        [CUDA.IArg n, CUDA.IArg isFullBlock])
      liftIO $ CUDA.setBlockShape fun (ctaSize, 1, 1)
      liftIO $ CUDA.launch fun gridDim Nothing
      liftIO $ CUDA.sync
      CUDA.decUse rf >>= \ decUse -> if decUse == 0
        then CUDA.free rf $ CUDA.fromFunParams rf xsFunParams
        else return ()
      return (ysFunParams, ys)
    Nothing ->
      error $ "Code generation for \n\t" ++ show op ++ "\nfailed."
execute op@(ZipWith fun xs ys) = do
  currentState <- get
  let zipWithMap = CUDA.zipWithMap currentState
      key        = show fun
  case M.lookup key zipWithMap of
    Just v -> do
      getCompilerProcessStatus (CUDA.compilerPID v)
      props  <- liftIO $ CUDA.device 0 >>= CUDA.props
      ptx    <- liftIO $ B.readFile (CUDA.progName v ++ ".ptx")
      (m, _) <- liftIO $ CUDA.loadDataEx ptx []
      fun    <- liftIO $ CUDA.getFun m ("_" ++ CUDA.progName v)
      xs'@(xsFunParams, Array sh1 rf1) <- execute xs
      ys'@(ysFunParams, Array sh2 rf2) <- execute ys
      let (maxGridSizeX, maxGridSizeY, maxGridSizeZ) = CUDA.maxGridSize props
          newSh          = sh1 `intersect` sh2
          n              = size newSh
          ctaSize        = 128
          numElemsPerCTA = ctaSize * 2
          numBlocks      = (n + numElemsPerCTA - 1) `div` numElemsPerCTA
          gridDim        = ( min maxGridSizeX numBlocks
                           , (numBlocks + maxGridSizeY - 1) `div` maxGridSizeY)
          isFullBlock    = if n `mod` numElemsPerCTA == 0 then 1 else 0
          zs@(Array sh3 rf3) = newArray_ (Sugar.toElem newSh)
      dptr <- CUDA.mallocArray rf3 (size sh3)
      let zsFunParams = CUDA.toFunParams rf3 dptr
      liftIO $ CUDA.setParams fun
        (xsFunParams ++ ysFunParams ++ zsFunParams ++
        [CUDA.IArg n, CUDA.IArg isFullBlock])
      liftIO $ CUDA.setBlockShape fun (ctaSize, 1, 1)
      liftIO $ CUDA.launch fun gridDim Nothing
      liftIO $ CUDA.sync
      CUDA.decUse rf1 >>= \ decUse -> if decUse == 0
        then CUDA.free rf1 $ CUDA.fromFunParams rf1 xsFunParams
        else return ()
      CUDA.decUse rf2 >>= \ decUse -> if decUse == 0
        then CUDA.free rf2 $ CUDA.fromFunParams rf2 ysFunParams
        else return ()
      return (zsFunParams, zs)
    Nothing ->
      error $ "Code generation for \n\t" ++ show op ++ "\nfailed."
--execute op@(Fold    fun left xs)   = return []
execute op@(Use arr@(Array _ e)) = do
  arr' <- CUDA.toFunParams' e
  return (arr', arr)

-- Create an array for output
newArray_ :: (Sugar.Ix dim, Sugar.Elem e) => dim -> Array dim e
{-# INLINE newArray_ #-}
newArray_ sh
  = adata `seq` Array (Sugar.fromElem sh) adata
  where
    (adata, _) = runArrayData $ do
                   arr <- newArrayData (Sugar.size sh)
                   return (arr, undefined)

getCompilerProcessStatus :: ProcessID -> CUDA.CGIO ProcessStatus
getCompilerProcessStatus pid = do
  status <- liftIO $ getProcessStatus True True pid
  case status of
    Just s@(Exited ExitSuccess) -> return s
    _ -> error $ "An nvcc process (PID=" ++ show pid
              ++ ") terminated abnormally."

--
-- Host To GPU data transfer
-- -------------------------

-- |Allocates device memory and triggers asynchronous data transfer.
--
memHtoD :: Delayable a => OpenAcc aenv a -> CUDA.CGIO ()
memHtoD op@(Map     fun xs)        = memHtoD xs
memHtoD op@(ZipWith fun xs ys)     = memHtoD xs >> memHtoD ys
memHtoD op@(Fold    fun left xs)   = memHtoD xs
memHtoD op@(Use     (Array dim e)) = do
  let numElems  = size dim
      allocFlag = [CUDA.Portable, CUDA.WriteCombined]
  -- hptrs <- CUDA.mallocHostArray e allocFlag numElems
  dptrs <- CUDA.mallocArray e numElems
  CUDA.pokeArrayAsync e numElems dptrs Nothing
memHtoD op = error $ show op ++ " not supported yet by the code generator."

--
-- GPU To Host data transfer
-- -------------------------

-- |Transfers the result from GPU
--
memDtoH :: Delayable a
        => OpenAcc aenv a -> ([CUDA.FunParam WordPtr], a) -> CUDA.CGIO ()
memDtoH op@(Fold _ _ _) (wptr, Array sh rf) = do
  CUDA.peekArray rf (size sh) (CUDA.fromFunParams rf wptr)
  CUDA.free rf $ CUDA.fromFunParams rf wptr
memDtoH op@(Map _ _) (wptr, Array sh rf) = do
  CUDA.peekArray rf (size sh) (CUDA.fromFunParams rf wptr)
  CUDA.free rf $ CUDA.fromFunParams rf wptr
memDtoH op@(ZipWith _ _ _) (wptr, Array sh rf) = do
  CUDA.peekArray rf (size sh) (CUDA.fromFunParams rf wptr)
  CUDA.free rf $ CUDA.fromFunParams rf wptr

--
-- CUDA code compilation flags
-- ---------------------------

cuCompileFlags :: String -> [String]
cuCompileFlags progName = 
  [ "-m32", "--compiler-options", "-fno-strict-aliasing", "-DUNIX"
  , "-O2", "-o", progName ++ ".ptx", "-ptx", progName ++ ".cu"]

--
-- CUDA/PTX code generation and compilation
-- ----------------------------------------

-- Generate CUDA code and PTX code for an array expression
--
codeGenAcc :: Delayable a => OpenAcc aenv a -> CUDA.CGIO ()
codeGenAcc op@(Fold fun left xs) = do
  currentState <- get
  let uniqueID = CUDA.uniqueID currentState
      progName = "CUDAFold" ++ show uniqueID
      foldMap      = CUDA.foldMap currentState
      foldMapKey   = show fun
      foldMapValue = case M.lookup foldMapKey foldMap of
        Just  v -> v
        Nothing -> CUDA.OperationValue 0 progName (CUDA.devPtrToWordPtr CUDA.nullDevPtr)
      scalar   = CUDA.Scalar
        { CUDA.params   =
          [ (codeGenTupleType $ expType left, "x1")
          , (codeGenTupleType $ accType xs,   "x0")]
        , CUDA.outTy    = codeGenTupleType $ accType op
        , CUDA.comp     = codeGenFun fun
        , CUDA.identity = Nothing}
  liftIO $ CUDA.foldGen progName scalar (codeGenTupleType $ expType left, "left")
  pid <- liftIO $ forkProcess $
    executeFile "nvcc" True (cuCompileFlags progName) Nothing
  let foldMapValue' = foldMapValue
        {CUDA.compilerPID = pid, CUDA.progName  = progName}
  put $ currentState
    { CUDA.uniqueID = uniqueID + 1
    , CUDA.foldMap  =
      insert foldMapKey foldMapValue' foldMap}
codeGenAcc op@(Map fun xs) = do
  currentState <- get
  let uniqueID    = CUDA.uniqueID currentState
      progName    = "CUDAMap" ++ show uniqueID
      mapMap      = CUDA.mapMap currentState
      mapMapKey   = show fun
      mapMapValue = case M.lookup mapMapKey mapMap of
        Just  v -> v
        Nothing -> CUDA.OperationValue 0 progName (CUDA.devPtrToWordPtr CUDA.nullDevPtr)
      scalar   = CUDA.Scalar
        { CUDA.params   =
          [ (codeGenTupleType $ accType xs, "x0")]
        , CUDA.outTy    = codeGenTupleType $ accType op
        , CUDA.comp     = codeGenFun fun
        , CUDA.identity = Nothing}
  liftIO $ CUDA.mapGen progName scalar
  pid <- liftIO $ forkProcess $
    executeFile "nvcc" True (cuCompileFlags progName) Nothing
  let mapMapValue' = mapMapValue
        {CUDA.compilerPID = pid, CUDA.progName  = progName}
  put $ currentState
    { CUDA.uniqueID = uniqueID + 1
    , CUDA.mapMap   =
      insert mapMapKey mapMapValue' mapMap}
codeGenAcc op@(ZipWith fun xs ys) = do
  currentState <- get
  let uniqueID        = CUDA.uniqueID currentState
      progName        = "CUDAZipWith" ++ show uniqueID 
      zipWithMap      = CUDA.zipWithMap currentState
      zipWithMapKey   = show fun
      zipWithMapValue = case M.lookup zipWithMapKey zipWithMap of
        Just  v -> v
        Nothing -> CUDA.OperationValue 0 progName (CUDA.devPtrToWordPtr CUDA.nullDevPtr)
      scalar   = CUDA.Scalar
        { CUDA.params   =
          [ (codeGenTupleType $ accType xs, "x1")
          , (codeGenTupleType $ accType ys, "x0")]
        , CUDA.outTy    = codeGenTupleType $ accType op
        , CUDA.comp     = codeGenFun fun
        , CUDA.identity = Nothing}
  liftIO $ CUDA.zipWithGen progName scalar
  pid <- liftIO $ forkProcess $
    executeFile "nvcc" True (cuCompileFlags progName) Nothing
  let zipWithMapValue' = zipWithMapValue
        {CUDA.compilerPID = pid, CUDA.progName  = progName}
  put $ currentState
    { CUDA.uniqueID   = uniqueID + 1
    , CUDA.zipWithMap =
      insert zipWithMapKey zipWithMapValue' zipWithMap}
codeGenAcc op@(Use acc   ) = return ()
codeGenAcc op = error $ show op ++ " not supported yet by the code generator."

-- Scalar function
-- ---------------

codeGenFun :: OpenFun env aenv t -> [CUDA.BlkItem]
codeGenFun f@(Lam lam) = codeGenFun lam
codeGenFun (Body body) =
  [CUDA.StmtItem $ CUDA.JumpStmt $ CUDA.Return $ Just $ codeGenExp body]

-- Expression
-- ----------

codeGenExp :: forall t env aenv . OpenExp env aenv t -> CUDA.Exp
codeGenExp e@(Var idx) =
  let idxToInt :: Idx env' t' -> Int
      idxToInt ZeroIdx        = 0
      idxToInt (SuccIdx idx') = 1 + idxToInt idx'
  in  CUDA.Exp [CUDA.toAssignExp $ CUDA.Ident $ "x" ++ show (idxToInt idx)]
codeGenExp e@(Const c) =
  let valStr = show (Sugar.toElem c :: t)
      getCUDAConst (CUDA.Float)        = CUDA.FloatConst   $ read valStr
      getCUDAConst (CUDA.Double)       = CUDA.DoubleConst  $ read valStr
      getCUDAConst (CUDA.Char Nothing) = CUDA.CharConst    $ read valStr
      getCUDAConst (CUDA.Int  Nothing) | valStr == "True"  = CUDA.IntegerConst 1
                                       | valStr == "False" = CUDA.IntegerConst 0
                                       | otherwise = CUDA.IntegerConst $ read valStr
      getCUDAConst t' = error $ (show t') ++ " not supported yet as a const type"
  in  CUDA.Exp [CUDA.toAssignExp $ getCUDAConst $ codeGenTupleType $ expType e]
codeGenExp e@(Tuple tup) = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."
codeGenExp e@(Prj idx e') = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."
codeGenExp e@(Cond e1 e2 e3) =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.NestedExp $ codeGenExp e1)
    (codeGenExp e2)
    (CUDA.toCondExp $ CUDA.NestedExp $ codeGenExp e3)]
codeGenExp e@(PrimConst c) = codeGenPrimConst c
codeGenExp e@(PrimApp f a@(Tuple (NilTup `SnocTup` e1 `SnocTup` e2))) =
  codeGenPrim f $ Prelude.map CUDA.NestedExp [codeGenExp e1, codeGenExp e2]
codeGenExp e@(IndexScalar acc ix) = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."
codeGenExp e@(Shape acc) = error $ "the expression, " ++ (show e) ++ ", not supported yet by the code generator."

-- Types
-- -----

codeGenTupleType :: TupleType a -> CUDA.TySpec
codeGenTupleType (UnitTuple)             = error "not implemented yet"
codeGenTupleType (SingleTuple t)         = codeGenScalarType t
codeGenTupleType (PairTuple UnitTuple t) = codeGenTupleType t
codeGenTupleType (PairTuple _         _) = error "not implemented yet"

codeGenScalarType :: ScalarType a -> CUDA.TySpec
codeGenScalarType (NumScalarType t)    = codeGenNumType t
codeGenScalarType (NonNumScalarType t) = codeGenNonNumType t

codeGenNumType :: NumType a -> CUDA.TySpec
codeGenNumType (IntegralNumType t) = codeGenIntegralType t
codeGenNumType (FloatingNumType t) = codeGenFloatingType t

codeGenIntegralType :: IntegralType a -> CUDA.TySpec
codeGenIntegralType (TypeInt     _) = CUDA.Int      Nothing
codeGenIntegralType (TypeInt8    _) = CUDA.Char     Nothing
codeGenIntegralType (TypeInt16   _) = CUDA.Short    Nothing
codeGenIntegralType (TypeInt32   _) = CUDA.Long     Nothing
codeGenIntegralType (TypeInt64   _) = CUDA.LongLong Nothing
codeGenIntegralType (TypeWord    _) = CUDA.Int      $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord8   _) = CUDA.Char     $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord16  _) = CUDA.Short    $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord32  _) = CUDA.Long     $ Just CUDA.Unsigned
codeGenIntegralType (TypeWord64  _) = CUDA.LongLong $ Just CUDA.Unsigned
codeGenIntegralType (TypeCShort  _) = CUDA.Short    Nothing
codeGenIntegralType (TypeCUShort _) = CUDA.Short    $ Just CUDA.Unsigned
codeGenIntegralType (TypeCInt    _) = CUDA.Int      Nothing
codeGenIntegralType (TypeCUInt   _) = CUDA.Int      $ Just CUDA.Unsigned
codeGenIntegralType (TypeCLong   _) = CUDA.Long     Nothing
codeGenIntegralType (TypeCULong  _) = CUDA.Long     $ Just CUDA.Unsigned
codeGenIntegralType (TypeCLLong  _) = CUDA.LongLong Nothing
codeGenIntegralType (TypeCULLong _) = CUDA.LongLong $ Just CUDA.Unsigned

codeGenFloatingType :: FloatingType a -> CUDA.TySpec
codeGenFloatingType (TypeFloat   _) = CUDA.Float
codeGenFloatingType (TypeDouble  _) = CUDA.Double
codeGenFloatingType (TypeCFloat  _) = CUDA.Float
codeGenFloatingType (TypeCDouble _) = CUDA.Double

codeGenNonNumType :: NonNumType a -> CUDA.TySpec
codeGenNonNumType (TypeBool   _) = CUDA.Char $ Just CUDA.Unsigned
codeGenNonNumType (TypeChar   _) = CUDA.Char Nothing
codeGenNonNumType (TypeCChar  _) = CUDA.Char Nothing
codeGenNonNumType (TypeCSChar _) = CUDA.Char $ Just CUDA.Signed
codeGenNonNumType (TypeCUChar _) = CUDA.Char $ Just CUDA.Unsigned

-- Scalar primitives
-- -----------------

codeGenPrimConst :: PrimConst a -> CUDA.Exp
codeGenPrimConst (PrimMinBound ty) = codeGenMinBound ty
codeGenPrimConst (PrimMaxBound ty) = codeGenMaxBound ty
codeGenPrimConst (PrimPi       ty) = codeGenPi ty

codeGenMinBound :: forall a . BoundedType a -> CUDA.Exp
codeGenMinBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty =
    CUDA.Exp
      [CUDA.toAssignExp $ CUDA.IntegerConst $ fromIntegral (minBound :: a)]
codeGenMinBound (NonNumBoundedType ty@(TypeBool _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1]
codeGenMinBound (NonNumBoundedType ty@(TypeChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCSChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CSChar)]
codeGenMinBound (NonNumBoundedType ty@(TypeCUChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (minBound :: CUChar)]

codeGenMaxBound :: forall a . BoundedType a -> CUDA.Exp
codeGenMaxBound (IntegralBoundedType ty)
  | IntegralDict <- integralDict ty =
    CUDA.Exp
      [CUDA.toAssignExp $ CUDA.IntegerConst $ fromIntegral (maxBound :: a)]
codeGenMaxBound (NonNumBoundedType ty@(TypeBool _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1]
codeGenMaxBound (NonNumBoundedType ty@(TypeChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCSChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CSChar)]
codeGenMaxBound (NonNumBoundedType ty@(TypeCUChar _))
  | NonNumDict <- nonNumDict ty =
    CUDA.Exp
      [ CUDA.toAssignExp $ CUDA.IntegerConst
      $ fromIntegral (maxBound :: CUChar)]

codeGenPi :: FloatingType a -> CUDA.Exp
codeGenPi ty@(TypeFloat _)   | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.FloatConst pi]
codeGenPi ty@(TypeDouble _)  | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.DoubleConst pi]
codeGenPi ty@(TypeCFloat _)  | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.FloatConst pi]
codeGenPi ty@(TypeCDouble _) | FloatingDict <- floatingDict ty =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.DoubleConst pi]

codeGenPrim :: PrimFun sig -> [CUDA.PrimaryExp] -> CUDA.Exp
codeGenPrim (PrimAdd         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Add (CUDA.toAddExp x) (CUDA.toMulExp y)]
codeGenPrim (PrimSub         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub (CUDA.toAddExp x) (CUDA.toMulExp y)]
codeGenPrim (PrimMul         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Mul (CUDA.toMulExp x) (CUDA.toCastExp y)]
codeGenPrim (PrimNeg         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub
    (CUDA.toAddExp $ CUDA.IntegerConst 0) (CUDA.toMulExp x)]
codeGenPrim (PrimAbs         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt
      (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
    (CUDA.Exp [CUDA.toAssignExp $ CUDA.Sub
      (CUDA.toAddExp $ CUDA.IntegerConst 0) (CUDA.toMulExp x)])
    (CUDA.toCondExp x)]
codeGenPrim (PrimSig         _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt
      (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
    (CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst (-1)])
    (CUDA.Cond
      (CUDA.toLgcOrExp $ CUDA.Gt
        (CUDA.toRelExp x) (CUDA.toShftExp $ CUDA.IntegerConst 0))
      (CUDA.Exp [CUDA.toAssignExp $ CUDA.IntegerConst 1])
      (CUDA.toCondExp $ CUDA.IntegerConst 0))]
codeGenPrim (PrimQuot        _) [x, y] = error "the code generation of PrimQuot not implemented yet."
codeGenPrim (PrimRem         _) [x, y] = error "the code generation of PrimRem not implemented yet."
codeGenPrim (PrimIDiv        _) [x, y] = error "the code generation of PrimIDiv not implemented yet."
codeGenPrim (PrimMod         _) [x, y] = error "the code generation of PrimMod not implemented yet."
codeGenPrim (PrimBAnd        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.And (CUDA.toAndExp x) (CUDA.toEqExp y)]
codeGenPrim (PrimBOr         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Or (CUDA.toOrExp x) (CUDA.toXorExp y)]
codeGenPrim (PrimBXor        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Xor (CUDA.toXorExp x) (CUDA.toAndExp y)]
codeGenPrim (PrimBNot        _) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.BitNot (CUDA.toCastExp x)]
codeGenPrim (PrimFDiv        _) [x, y] = error "the code generation of PrimFDiv not implemented yet."
codeGenPrim (PrimRecip       _) [x] = error "the code generation of PrimRecip not implemented yet."
codeGenPrim (PrimSin         _) [x] = error "the code generation of PrimSin not implemented yet."
codeGenPrim (PrimCos         _) [x] = error "the code generation of PrimCos not implemented yet."
codeGenPrim (PrimTan         _) [x] = error "the code generation of PrimTan not implemented yet."
codeGenPrim (PrimAsin        _) [x] = error "the code generation of PrimAsin not implemented yet."
codeGenPrim (PrimAcos        _) [x] = error "the code generation of PrimAcos not implemented yet."
codeGenPrim (PrimAtan        _) [x] = error "the code generation of PrimAtan not implemented yet."
codeGenPrim (PrimAsinh       _) [x] = error "the code generation of PrimAsinh not implemented yet."
codeGenPrim (PrimAcosh       _) [x] = error "the code generation of PrimAcosh not implemented yet."
codeGenPrim (PrimAtanh       _) [x] = error "the code generation of PrimAtanh not implemented yet."
codeGenPrim (PrimExpFloating _) [x] = error "the code generation of PrimExpFloating not implemented yet."
codeGenPrim (PrimSqrt        _) [x] = error "the code generation of PrimSqrt not implemented yet."
codeGenPrim (PrimLog         _) [x] = error "the code generation of PrimLog not implemented yet."
codeGenPrim (PrimFPow        _) [x, y] = error "the code generation of PrimFPow not implemented yet."
codeGenPrim (PrimLogBase     _) [x, y] = error "the code generation of PrimLogBase not implemented yet."
codeGenPrim (PrimLt          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Lt (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimGt          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Gt (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimLtEq        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Le (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimGtEq        _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Ge (CUDA.toRelExp x) (CUDA.toShftExp y)]
codeGenPrim (PrimEq          _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Eq (CUDA.toEqExp x) (CUDA.toRelExp y)]
codeGenPrim (PrimNEq         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Neq (CUDA.toEqExp x) (CUDA.toRelExp y)]
codeGenPrim (PrimMax         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Gt (CUDA.toRelExp x) (CUDA.toShftExp y))
    (CUDA.Exp [CUDA.toAssignExp x])
    (CUDA.toCondExp y)]
codeGenPrim (PrimMin         _) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.Cond
    (CUDA.toLgcOrExp $ CUDA.Lt (CUDA.toRelExp x) (CUDA.toShftExp y))
    (CUDA.Exp [CUDA.toAssignExp x])
    (CUDA.toCondExp y)]
codeGenPrim (PrimLAnd         ) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcAnd
    (CUDA.toLgcAndExp x) (CUDA.toOrExp y)]
codeGenPrim (PrimLOr          ) [x, y] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcOr
    (CUDA.toLgcOrExp x) (CUDA.toLgcAndExp y)]
codeGenPrim (PrimLNot         ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.LgcNot (CUDA.toCastExp x)]
codeGenPrim (PrimOrd          ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Int Nothing)] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimChr          ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Char Nothing)] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimRoundFloatInt) [x] = error "the code generation of PrimRoundFloatInt not implemented yet."
codeGenPrim (PrimTruncFloatInt) [x] = error "the code generation of PrimTruncFloatInt not implemented yet."
codeGenPrim (PrimIntFloat     ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec CUDA.Float] Nothing)
    (CUDA.toCastExp x)]
codeGenPrim (PrimBoolToInt    ) [x] =
  CUDA.Exp [CUDA.toAssignExp $ CUDA.TyCast
    (CUDA.TyName [CUDA.SpecQualTySpec (CUDA.Int Nothing)] Nothing)
    (CUDA.toCastExp x)]
