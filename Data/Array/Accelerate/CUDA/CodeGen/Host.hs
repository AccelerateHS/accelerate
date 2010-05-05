
module Data.Array.Accelerate.CUDA.CodeGen.Host (
  -- Data structures
  HostCodeConfig(..), Param(..), Associativity(..), Operation(..),
  -- The host code generator
  hostCodeGen
) where

import Prelude   hiding (init, Either(..))

import Data.Array.Accelerate.CUDA.Scalar
import Data.Array.Accelerate.CUDA.Syntax

import qualified Data.Array.Accelerate.CUDA.CodeGen.Device as DeviceCodeGen

--
-- The Host Code Configuration
--
-- Each host function comes with a progName, a list of input parameters, a
-- list of output parameters, and a list of kernels to be executed.
--
data HostCodeConfig = HostCodeConfig
  { progName :: String
  , ctaSize :: Int
  , nPerThread :: Int
  , sharedMemSize :: Maybe AssignExp
  , inParams :: [Param]
  , outParam :: Param
  , scalar :: Maybe Scalar
  , op :: Operation}

--
-- The Parameter Description
--
-- Each parameter has a name and a type.
--
data Param
  = Array {paramName :: String, paramType :: TySpec}
  | Variable {paramName :: String, paramType :: TySpec}

--
-- The associativity of the operation. This is required to describe how the
-- result of each pass is passed to the next pass.
--
data Associativity
  = Left {leftVarName :: String}
  | Left1
  | Right {rightVarName :: String}
  | Right1
  | SegmentedLeft {leftVarName :: String, segDescriptorName :: String}
  | SegmentedLeft1 {segDescriptorName :: String}
  | SegmentedRight {rightVarName :: String, segDescriptorName :: String}
  | SegmentedRight1 {segDescriptorName :: String}

--
-- The op function may be generated as a device function, a host function, or
-- as a generic function.
--
data OpTarget = Device
              | Host
              | DeviceHost

--
-- The Kernel Description
--
-- (1) Map executes the kernel only once. (map, zip, zipWith, etc)
-- (2) Fold executes the kernel to reduce.
-- (3) Scan executes the reduce kernel to reduce and the scatter
--     kernel to scatter. (scanl)
--
data Operation
  = Map
    { kernel            :: DeviceCodeGen.Kernel}
  | Fold
    { kernel            :: DeviceCodeGen.Kernel
    , associativity     :: Associativity}
  | Scan 
    { reduceKernel      :: DeviceCodeGen.Kernel
    , scatterKernel     :: DeviceCodeGen.Kernel
    , associativity     :: Associativity}
  | SegmentedFold
    { reduceKernel      :: DeviceCodeGen.Kernel
    , scatterKernel     :: DeviceCodeGen.Kernel
    , segdReduceKernel  :: DeviceCodeGen.Kernel
    , segdScatterKernel :: DeviceCodeGen.Kernel
    , compactKernel     :: DeviceCodeGen.Kernel
    , associativity     :: Associativity}
  | SegmentedScan
    { reduceKernel      :: DeviceCodeGen.Kernel
    , scatterKernel     :: DeviceCodeGen.Kernel
    , associativity     :: Associativity}
  | Filter
    { validateKernel    :: DeviceCodeGen.Kernel
    , reduceKernel      :: DeviceCodeGen.Kernel
    , scatterKernel     :: DeviceCodeGen.Kernel
    , compactKernel     :: DeviceCodeGen.Kernel}

--
-- The Host Code Generator
--
-- It takes the configuration (HostCodeConfig) from the user and generates the
-- host code according to the configuration. It returns the function prototype
-- for the header file and the function implementation.
--
hostCodeGen :: HostCodeConfig -> (TransUnit, TransUnit)
hostCodeGen conf =
  ( TransUnit [] (hostFuncPrototype conf)
  , TransUnit (includes conf ++ macros) (scalarFunc conf ++ hostFunc conf))

--
-- hostFuncPrototype generates the function prototype of the host function,
-- which is to be included in the header file.
--
hostFuncPrototype :: HostCodeConfig -> [ExtDecln]
hostFuncPrototype conf = [GlobalDecln $ Decln
  [DeclnStSpec $ Extern (Just $ StrLit "C"), DeclnTySpec Void]
  [InitDeclr
    (Declr
      Nothing
      (FuncDeclr
        (IdentDeclr $ Ident $ progName conf)
        (map
          (\ (ty, ptr, varName) -> ParamDecln
            ty (Declr ptr (IdentDeclr $ Ident varName)))
          ( [ ( [DeclnTySpec $ paramType param]
              , case param of
                Array    {} -> Just $ Pointer [[]]
                Variable {} -> Nothing
              , paramName param)
            | param <- inParams conf]
          ++[ let param = outParam conf
              in  ( [DeclnTySpec $ paramType param]
                  , Just $ Pointer [[]], paramName param)]
          ++[([DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")]))))
    Nothing]]

--
-- includes includes the header files.
--
includes :: HostCodeConfig -> [Prepro]
includes conf =
  map
    (Include . Ident)
    ["stdio.h", "stdlib.h", "math.h", "cutil.h", "cuda.h"]
  ++
  [LocalInclude $ Ident $ progName conf ++ ".h"]

--
-- macros generates the macros that are required by the host function
--
-- #ifndef min
-- #define min(x,y) (((x) < (y)) ? (x) : (y))
-- #endif
-- #ifndef max
-- #define max(x,y) (((x) > (y)) ? (x) : (y))
-- #endif
--
macros :: [Prepro]
macros = concatMap
  (\ (macroName, op) ->
    [ IfNDef $ Ident macroName
    , Define
      (Ident $ macroName ++ "(x,y)")
      (Exp [toAssignExp $ NestedExp $ Exp [toAssignExp $ Cond
        (toLgcOrExp $ NestedExp $ Exp [toAssignExp $ op
          (toRelExp $ NestedExp $ Exp [toAssignExp $ Ident "x"])
          (toShftExp $ NestedExp $ Exp [toAssignExp $ Ident "y"])])
        (Exp [toAssignExp $ NestedExp $ Exp [toAssignExp $ Ident "x"]])
        (toCondExp $ NestedExp $ Exp [toAssignExp $ Ident "y"])]])
     ,EndIf])
  [("min", Lt), ("max", Gt)]

--
-- scalarFunc generates the scalar function, which may be required by the host
-- function. The scalar function for the host is defined as an inline
-- function.
--
scalarFunc :: HostCodeConfig -> [ExtDecln]
scalarFunc conf = case scalar conf of
  Just scalar' -> [FuncDef
    [DeclnFnSpec Inline, DeclnTySpec $ outTy scalar']
    (Declr Nothing (IdentDeclr $ Ident $ "_" ++ progName conf ++ "Scalar"))
    (Prelude.map
      (\ (ty, varName) -> Decln
        [DeclnTySpec ty]
        [InitDeclr (Declr Nothing (IdentDeclr $ Ident varName)) Nothing])
      (params scalar'))
    (Blk $ comp scalar')]
  Nothing                  -> []

--
-- hostFunc generates the host function which controls the GPU operations.
--
-- extern "C" void progName(...)
-- {
--   ...
-- }
hostFunc :: HostCodeConfig -> [ExtDecln]
hostFunc conf = [FuncDef
  [DeclnStSpec $ Extern (Just $ StrLit "C"), DeclnTySpec Void]
  (Declr Nothing (IdentDeclr $ Ident $ progName conf))
  (map
    (\ (ty, ptr, varName) -> Decln
      ty [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) Nothing])
    ( [ ( [DeclnTySpec $ paramType param]
        , case param of
          Array    {} -> Just $ Pointer [[]]
          Variable {} -> Nothing
        , paramName param)
      | param <- inParams conf]
    ++[ let param = outParam conf
        in  ( [DeclnTySpec $ paramType param]
            , Just $ Pointer [[]], paramName param)]
    ++[([DeclnTySpec (Int $ Just Unsigned)], Nothing, "n")]))
  (Blk $ decl conf ++ init conf
      ++ malloc conf ++ multiPassExec conf ++ free conf ++ finalise)]

--
-- decl generates the declarations of the local variables.
--
decl :: HostCodeConfig -> [BlkItem]
decl conf =
  -- CUfunction cuFunction[nKerenls];
  -- where nKernels is the number of the kernels to be executed.
  [DeclnItem $ Decln
    [DeclnTySpec $ TypedefName $ Ident "CUfunction"]
    [InitDeclr
      (Declr Nothing (ArrayDeclr
        (IdentDeclr $ Ident "cuFunction")
        Nothing
        (Just $ toAssignExp $ IntegerConst $ case op conf of
          Scan {} -> 2
          _                -> 1)))
      Nothing]]
  ++
  -- CUdevice cuDevice;
  -- CUcontext cuContext;
  -- CUmodule cuModule;
  -- CUdeviceptr d_...; (as many as the number of input array parameters)
  -- CUdeviceptr d_...; (output parameter)
  -- uint maxPerPass;
  -- uint totalMem;
  -- uint offset;
  -- uint i;
  map
    (\ (ty, ptr, varName, exp) -> DeclnItem $ Decln
      [DeclnTySpec ty]
      [InitDeclr (Declr ptr (IdentDeclr $ Ident varName)) exp])
    ( [ ( TypedefName $ Ident "CUdevice", Nothing, "cuDevice", Nothing)
      , ( TypedefName $ Ident "CUcontext", Nothing, "cuContext", Nothing)
      , ( TypedefName $ Ident "CUmodule", Nothing, "cuModule", Nothing)]
    ++[ ( TypedefName $ Ident "CUdeviceptr", Nothing
        , "d_" ++ paramName param, Nothing)
      | param@(Array {}) <- inParams conf ++ [outParam conf]]
    ++[ ( Int $ Just Unsigned, Nothing, "maxPerPass", Nothing)
      , ( Int $ Just Unsigned, Nothing, "totalMem", Nothing)
      , ( Int $ Just Unsigned, Nothing, "offset", Nothing)
      , ( Int $ Just Unsigned, Nothing, "i", Nothing)]
    ++case op conf of
        Map {} ->
          [ ( Int $ Just Unsigned, Nothing, "d_n", Nothing)
          , ( Int $ Just Unsigned, Nothing, "d_nBlocks", Nothing)]
        _      ->
          ( [ ( Int Nothing, Nothing, "j", Nothing)
            , ( TypedefName $ Ident "CUdeviceptr", Just $ Pointer [[]]
              , "d_blockSums", Nothing)
            , ( Int $ Just Unsigned, Just $ Pointer [[]], "d_n", Nothing)
            , ( Int $ Just Unsigned, Just $ Pointer [[]], "d_nBlocks", Nothing)
            , ( Int $ Just Unsigned, Nothing, "temp", Nothing)
            , ( Int Nothing, Nothing, "level"
              , Just $ AssignExp $ toAssignExp $ IntegerConst 0)]
          ++case associativity $ op conf of
            Left leftVarName ->
              [ ( paramType param, Nothing, "startingVal"
                , Just $ AssignExp $ toAssignExp $ Ident $ paramName param)
              | param@(Variable {}) <- inParams conf
              , paramName param == leftVarName]
            Right rightVarName ->
              [ ( paramType param, Nothing, "startingVal"
                , Just $ AssignExp $ toAssignExp $ Ident $ paramName param)
              | param@(Variable {}) <- inParams conf
              , paramName param == rightVarName]
            _                  -> [])
    ++case sharedMemSize conf of
        Just size ->
          [(Int $ Just Unsigned, Nothing, "sharedMemSize", Just $ AssignExp size)]
        Nothing -> [])

--
-- init generates the declarations of the local variables.
--
init :: HostCodeConfig -> [BlkItem]
init conf =
  -- cuDeviceGet(&cuDevice, 0);
  -- cuCtxCreate(&cuContext, CU_CTX_SCHED_AUTO, cuDevice);
  -- cuModuleLoad(&cuModule, "data/progName.cubin");
  -- cuDeviceTotalMem(&totalMem, cuDevice);
  -- cuModuleGetFunction(&cuFunction[0], cuModule, ...);
  map
    (\ (funcName, args) ->
      StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident funcName) (ArgExpList args)])
    ( [ ( "cuInit"
        , [ toAssignExp $ IntegerConst 0])
      , ( "cuDeviceGet"
        , [ toAssignExp $ AddrOf $ toCastExp $ Ident "cuDevice"
          , toAssignExp $ IntegerConst 0])
      , ( "cuCtxCreate"
        , [ toAssignExp $ AddrOf $ toCastExp $ Ident "cuContext"
          , toAssignExp $ Ident "CU_CTX_SCHED_AUTO"
          , toAssignExp $ Ident "cuDevice"])
      , ( "cuModuleLoad"
        , [ toAssignExp $ AddrOf $ toCastExp $ Ident "cuModule"
          , toAssignExp $ StrLitExp $ StrLit
          $ progName conf ++ ".cubin"])
      , ( "cuDeviceTotalMem"
        , [ toAssignExp $ AddrOf $ toCastExp $ Ident "totalMem"
          , toAssignExp $ Ident "cuDevice"])]
    ++[ ( "cuModuleGetFunction"
        , [ toAssignExp $ AddrOf $ toCastExp $ toArrayElem
            (Ident "cuFunction") (IntegerConst i)
          , toAssignExp $ Ident "cuModule"
          , toAssignExp $ StrLitExp $ StrLit
          $ "_" ++ progName conf])
      | (kernel, i) <- case op conf of
          op@(Scan {}) ->
            [(reduceKernel op, 0), (scatterKernel op, 1)]
          op                    ->
            [(kernel op, 0)]])
  ++
  -- maxPerPass =
  --   (totalMem - 83886080) / (sizeof (t1) + sizeof (t2) + ... + sizeof (tn));
  -- where t1, t2, ..., tn are the types of the parameters, and totalMem - 80MB
  -- is to leave 80MB of the graphics memory for the OS and other applications.
  [StmtItem $ ExpStmt $ Just $ Exp [Assign
    (toUnaryExp $ Ident "maxPerPass")
    (toAssignExp $ Div
      (toMulExp $ NestedExp $ Exp [toAssignExp $ Sub
        (toAddExp $ Ident "totalMem")
        (toMulExp $ IntegerConst $ 80 * 1024 * 1024)])
      (toCastExp $ NestedExp $ Exp [toAssignExp $ foldl1
        (\ tySize1 (MulExp tySize2) -> Add tySize1 tySize2)
        ( [ toAddExp $ TySize $ TyName
            [SpecQualTySpec $ paramType param] Nothing
          | param@(Array {}) <- inParams conf ++ [outParam conf]]
        ++case op conf of
          Map {} -> []
          _      ->
            (case scalar conf of
              Just scalar' ->
                [toAddExp $ TySize $ TyName
                  [SpecQualTySpec $ outTy scalar'] Nothing]
              Nothing      -> [])
            ++
            [toAddExp $ TySize $ TyName
              [SpecQualTySpec (Int $ Just Unsigned)] Nothing])]))]]

--
-- malloc generates the cuMemAlloc statements according to the given
-- parameters.
--
-- cuMemAlloc(&d_..., min(maxPerPass, n) * sizeof (t));
--
malloc :: HostCodeConfig -> [BlkItem]
malloc conf =
  case op conf of
    Map {} -> []
    _      ->
      [ StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident "temp")
        (toAssignExp $ FuncCall
          (toPostfixExp $ Ident "min")
          (ArgExpList $ Prelude.map
            (toAssignExp . Ident) ["maxPerPass", "n"]))]
      , StmtItem $ IterStmt $ DoWhile
        (CompStmt $ Blk
          [ StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ Ident "temp")
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident "max")
              (ArgExpList
                [ toAssignExp $ IntegerConst 1
                , toAssignExp $ TyCast
                  (TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing)
                  (toCastExp $ FuncCall
                    (toPostfixExp $ Ident "ceil")
                    (ArgExpList
                      [toAssignExp $ Div
                        ( toMulExp $ TyCast
                          (TyName [SpecQualTySpec Double] Nothing)
                          (toCastExp $ Ident "temp"))
                        ( toCastExp $ DoubleConst $ fromIntegral
                        $ nPerThread conf * ctaSize conf)]))]))]])
        (Exp [toAssignExp $ LgcAnd
          (toLgcAndExp $ Gt
            (toRelExp $ Ident "temp") (toShftExp $ IntegerConst 1))
          (toOrExp $ UnaryInc $ toUnaryExp $ Ident "level")])]
      ++
      map
        (\ (ty, varName, n) -> StmtItem $ ExpStmt $ Just $ Exp [Assign
          (toUnaryExp $ Ident varName)
          (toAssignExp $ TyCast
            (TyName
              [SpecQualTySpec ty]
              (Just $ AbstDeclrPointer $ Pointer [[]]))
            (toCastExp $ FuncCall
              (toPostfixExp $ Ident "malloc")
              (ArgExpList [toAssignExp $ Mul
                n
                (toCastExp $ TySize $ TyName
                  [SpecQualTySpec ty] Nothing)])))])
        [ ( Int $ Just Unsigned, "d_n"
          , toMulExp $ NestedExp $ Exp [toAssignExp $ Add
            (toAddExp $ Ident "level") (toMulExp $ IntegerConst 1)])
        , ( Int $ Just Unsigned, "d_nBlocks"
          , toMulExp $ NestedExp $ Exp [toAssignExp $ Add
            (toAddExp $ Ident "level") (toMulExp $ IntegerConst 1)])
        , ( TypedefName $ Ident "CUdeviceptr", "d_blockSums"
          , toMulExp $ Ident "level")]
      ++
      [ StmtItem $ IterStmt $ For
        (Just $ Exp
          [ Assign (toUnaryExp $ Ident "j") (toAssignExp $ IntegerConst 0)
          , Assign
            (toUnaryExp $ toArrayElem (Ident "d_n") (Ident "j"))
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident "min")
              (ArgExpList $ Prelude.map
                (toAssignExp . Ident) ["maxPerPass", "n"]))
          , Assign
            (toUnaryExp $ toArrayElem (Ident "d_nBlocks") (Ident "j"))
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident "max")
              (ArgExpList
                [ toAssignExp $ IntegerConst 1
                , toAssignExp $ TyCast
                  (TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing)
                  (toCastExp $ FuncCall
                    (toPostfixExp $ Ident "ceil")
                    (ArgExpList
                      [toAssignExp $ Div
                        ( toMulExp $ TyCast
                          (TyName [SpecQualTySpec Double] Nothing)
                          (toCastExp $ toArrayElem
                            (Ident "d_n") (Ident "j")))
                        ( toCastExp $ DoubleConst $ fromIntegral
                        $ nPerThread conf * ctaSize conf)]))]))])
        (Just $ Exp
          [toAssignExp $ Lt (toRelExp $ Ident "j") (toShftExp $ Ident "level")])
        (Just $ Exp [toAssignExp $ UnaryInc $ toUnaryExp $ Ident "j"])
        (CompStmt $ Blk
          [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
            (toPostfixExp $ Ident "cuMemAlloc")
            (ArgExpList
              [ toAssignExp $ AddrOf $ toCastExp $ toArrayElem
                ( Ident "d_blockSums") (Ident "j")
              , toAssignExp $ Mul
                ( toMulExp $ toArrayElem (Ident "d_nBlocks") (Ident "j"))
                ( toCastExp $ TySize
                $ TyName [SpecQualTySpec $ paramType $ outParam conf] Nothing)])]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            ( toUnaryExp $ toArrayElem
              (Ident "d_n")
              (Add (toAddExp $ Ident "j") (toMulExp $ IntegerConst 1)))
            ( toAssignExp $ toArrayElem (Ident "d_nBlocks") (Ident "j"))]
          , StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem
              (Ident "d_nBlocks")
              (Add (toAddExp $ Ident "j") (toMulExp $ IntegerConst 1)))
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident "max")
              (ArgExpList
                [ toAssignExp $ IntegerConst 1
                , toAssignExp $ TyCast
                  (TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing)
                  (toCastExp $ FuncCall
                    (toPostfixExp $ Ident "ceil")
                    (ArgExpList
                      [toAssignExp $ Div
                        ( toMulExp $ TyCast
                          (TyName [SpecQualTySpec Double] Nothing)
                          (toCastExp $ toArrayElem
                            (Ident "d_n")
                            (Add
                              (toAddExp $ Ident "j")
                              (toMulExp $ IntegerConst 1))))
                        ( toCastExp $ DoubleConst $ fromIntegral
                        $ nPerThread conf * ctaSize conf)]))]))]])]
  ++
  map
    (\ (ty, varName) -> StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident "cuMemAlloc")
      (ArgExpList
        [ toAssignExp $ AddrOf $ toCastExp $ Ident varName
        , toAssignExp $ Mul
          (toMulExp $ FuncCall
            (toPostfixExp $ Ident "min")
            (ArgExpList $ map (toAssignExp . Ident) ["maxPerPass", "n"]))
          (toCastExp $ TySize $ TyName [SpecQualTySpec ty] Nothing)])])
    [ (paramType param, "d_" ++ paramName param)
    | param@(Array {}) <- inParams conf ++ [outParam conf]]


--
-- multiPassExec generates the executions of the kernels in multi-passes,
-- according to the execution mode and the parameters descriptions.
--
-- for (i = 0; i < n; i += maxPerPass) {
--   // Transfer the input from host memory to device global memory
--   // Execute kernels
--   // Transfer the output from device global menory to host memory
-- }
--
multiPassExec :: HostCodeConfig -> [BlkItem]
multiPassExec conf = [StmtItem $ IterStmt $ For
  (Just $ Exp [Assign
    (toUnaryExp $ Ident "i") (toAssignExp $ IntegerConst 0)])
  (Just $ Exp [toAssignExp $ Lt
    (toRelExp $ Ident "i") (toShftExp $ Ident "n")])
  (Just $ Exp [AddAssign
    (toUnaryExp $ Ident "i") (toAssignExp $ Ident "maxPerPass")])
  (CompStmt $ Blk $ nForThePass conf ++ memcpyHtoD conf
                 ++ exec conf (op conf) ++ memcpyDtoH conf
                 ++ adjust conf (op conf))]

--
-- nForThePass generates the statement to figure the number of elements to be
-- processed for the pass, and the number of the thread blocks required.
--
-- uint d_n = min(maxPerPass, n - i);
-- uint numBlocks = max(1, (uint) ceil((double) d_n / 512.0));
-- where maxPerPass is the maximum number of elements that can be proceesed at
-- a single pass, n is the total number of elements, and i is the index of the
-- first element for the pass.
--
nForThePass :: HostCodeConfig -> [BlkItem]
nForThePass conf =
  case op conf of
    Map {} -> map
      (\ (varName, exp) -> StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident varName) exp])
      [ ( "d_n"
        , toAssignExp $ FuncCall
          (toPostfixExp $ Ident "min")
          (ArgExpList
            [ toAssignExp $ Ident "maxPerPass"
            , toAssignExp $ Sub
              (toAddExp $ Ident "n") (toMulExp $ Ident "i")]))
      , ( "d_nBlocks"
        , toAssignExp $ FuncCall
          (toPostfixExp $ Ident "max")
          (ArgExpList
            [ toAssignExp $ IntegerConst 1
            , toAssignExp $ TyCast
              (TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing)
              (toCastExp $ FuncCall
                (toPostfixExp $ Ident "ceil")
                (ArgExpList
                  [toAssignExp $ Div
                    ( toMulExp $ TyCast
                      (TyName [SpecQualTySpec Double] Nothing)
                      (toCastExp $ Ident "d_n"))
                    ( toCastExp $ DoubleConst $ fromIntegral
                    $ nPerThread conf * ctaSize conf)]))]))]
    _ ->
      [ StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident "level") (toAssignExp $ IntegerConst 0)]
      , StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ toArrayElem (Ident "d_n") (Ident "level"))
        (toAssignExp $ FuncCall
          (toPostfixExp $ Ident "min")
          (ArgExpList
            [ toAssignExp $ Ident "maxPerPass"
            , toAssignExp $ Sub
              (toAddExp $ Ident "n") (toMulExp $ Ident "i")]))]
      , StmtItem $ IterStmt $ DoWhile
        (CompStmt $ Blk
          [ StmtItem $ ExpStmt $ Just $ Exp [Assign
            (toUnaryExp $ toArrayElem (Ident "d_nBlocks") (Ident "level"))
            (toAssignExp $ FuncCall
              (toPostfixExp $ Ident "max")
              (ArgExpList
                [ toAssignExp $ IntegerConst 1
                , toAssignExp $ TyCast
                  (TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing)
                  (toCastExp $ FuncCall
                    (toPostfixExp $ Ident "ceil")
                    (ArgExpList
                      [toAssignExp $ Div
                        ( toMulExp $ TyCast
                          (TyName [SpecQualTySpec Double] Nothing)
                          (toCastExp $ toArrayElem
                            (Ident "d_n") (Ident "level")))
                        ( toCastExp $ DoubleConst $ fromIntegral
                        $ nPerThread conf * ctaSize conf)]))]))]])
        (Exp [toAssignExp $ LgcAnd
          (LgcAnd
            (toLgcAndExp $ Gt
              (toRelExp $ toArrayElem (Ident "d_nBlocks") (Ident "level"))
              (toShftExp $ IntegerConst 1))
            (toOrExp $ NestedExp $ Exp [Assign
              (toUnaryExp $ toArrayElem
                (Ident "d_n")
                (Add (toAddExp $ Ident "level") (toMulExp $ IntegerConst 1)))
              (toAssignExp $ toArrayElem
                (Ident "d_nBlocks") (Ident "level"))]))
          (toOrExp $ UnaryInc $ toUnaryExp $ Ident "level")])]

--
-- memcpyHtoD generates the code to transfer the input parameters to the
-- device global memory.
--
-- cuMemcpyHtoD(d_..., &...[i], _n * sizeof (t));
-- where ... is the name of the parameter, i is the index of the first element
-- for the pass, _n is the number of elements to be processed for the pass,
-- and t is the type of the elements in ....
--
memcpyHtoD :: HostCodeConfig -> [BlkItem]
memcpyHtoD conf = map
  (\ (ty, varName) -> StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuMemcpyHtoD")
    (ArgExpList
      [ toAssignExp $ Ident $ "d_" ++ varName
      , toAssignExp $ AddrOf $ toCastExp $ toArrayElem
        (Ident varName) (Ident "i")
      , toAssignExp $ Mul
        (case op conf of
          Map {} -> toMulExp $ Ident "d_n"
          _      -> toMulExp $ toArrayElem (Ident "d_n") (IntegerConst 0))
        (toCastExp $ TySize $ TyName [SpecQualTySpec ty] Nothing)])])
  [(paramType param, paramName param) | param@(Array {}) <- inParams conf]

--
-- exec generates the executions of kernels, depending on the execution mode.
--
exec :: HostCodeConfig -> Operation -> [BlkItem]
exec conf op@(Map {}) =
  -- cuFuncSetBlockShape(cuFunction[kernelNo], ctaSize, 1, 1);
  [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuFuncSetBlockShape")
    (ArgExpList
      [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
      , toAssignExp $ IntegerConst $ fromIntegral $ ctaSize conf
      , toAssignExp $ IntegerConst 1, toAssignExp $ IntegerConst 1])]
  -- offset = 0;
  , StmtItem $ ExpStmt $ Just $ Exp [Assign
    (toUnaryExp $ Ident "offset") (toAssignExp $ IntegerConst 0)]]
  ++
  -- cuParamSeti(cuFunction[kernelNo], offset, ...), offset += sizeof(...);
  map
    (\ (funcName, tySize, exp, append) -> StmtItem $ ExpStmt $ Just $ Exp
      [ toAssignExp $ FuncCall
        (toPostfixExp $ Ident funcName)
        (ArgExpList
          ( [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
            , toAssignExp $ Ident "offset", toAssignExp $ exp]
          ++if append then [tySize] else []))
      , AddAssign (toUnaryExp $ Ident "offset") tySize])
    ( [ case param of
        Array {} ->
          ( "cuParamSeti"
          , toAssignExp $ TySize $ TyName
            [SpecQualTySpec $ TypedefName $ Ident "CUdeviceptr"]
            Nothing
          , toAssignExp $ Ident $ "d_" ++ paramName param
          , False)
        Variable {} ->
          ( "cuParamSetv"
          , toAssignExp $ TySize $ TyName
            [SpecQualTySpec $ paramType param] Nothing
          , toAssignExp $ AddrOf $ toCastExp $ Ident $ paramName param
          , True)
      | param <- inParams conf ++ [outParam conf]]
    ++[ ( "cuParamSeti"
        , toAssignExp $ TySize $ TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing
        , toAssignExp $ Ident "d_n", False)
      , ( "cuParamSeti"
        , toAssignExp $ TySize $ TyName [SpecQualTySpec (Int $ Just Unsigned)] Nothing
        , toAssignExp $ Eq
          (toEqExp $ Ident "d_n")
          (toRelExp $ Mul
            ( toMulExp $ Ident "d_nBlocks")
            ( toCastExp $ IntegerConst $ fromIntegral
            $ nPerThread conf * ctaSize conf))
        , False)])
  ++
  [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuParamSetSize")
    (ArgExpList
      [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
      , toAssignExp $ Ident "offset"])]
  , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuLaunchGrid")
    (ArgExpList
      [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
      , toAssignExp $ FuncCall
        (toPostfixExp $ Ident "min")
        (ArgExpList
          [toAssignExp $ IntegerConst 65535, toAssignExp $ Ident "d_nBlocks"])
      , toAssignExp $ Add
        (toAddExp $ IntegerConst 1)
        (Div (toMulExp $ Ident "d_nBlocks") (toCastExp $ IntegerConst 65536))])]
  , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuCtxSynchronize") (ArgExpList [])]]
exec conf op@(Fold {}) =
  -- for (j = 0; j < level; ++j) {
  [ StmtItem $ IterStmt $ For
    (Just $ Exp [Assign (toUnaryExp $ Ident "j") (toAssignExp $ IntegerConst 0)])
    (Just $ Exp [toAssignExp $ Lt
      (toRelExp $ Ident "j") (toShftExp $ Ident "level")])
    (Just $ Exp [toAssignExp $ UnaryInc $ toUnaryExp $ Ident "j"])
    (CompStmt $ Blk $ exec' False)
  -- }
  , StmtItem $ CompStmt $ Blk $ exec' True]
  where
    exec' :: Bool -> [BlkItem]
    exec' last =
      -- cuFuncSetBlockShape(cuFunction[0], ctaSize, 1, 1);
      [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuFuncSetBlockShape")
        (ArgExpList
          [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
          , toAssignExp $ IntegerConst $ fromIntegral $ ctaSize conf
          , toAssignExp $ IntegerConst 1, toAssignExp $ IntegerConst 1])]
      -- cuFuncSetSharedSize(cuFunction[0], sharedSize);
      , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuFuncSetSharedSize")
        (ArgExpList
          [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
          , toAssignExp $ Ident "sharedMemSize"])]
      -- offset = 0;
      , StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident "offset") (toAssignExp $ IntegerConst 0)]
      -- if (j == 0) {
      , StmtItem $ SelectStmt $ IfElse
        (Exp [toAssignExp $ Eq
          (toEqExp $ Ident "j") (toRelExp $ IntegerConst 0)])
        --   cuParamSeti(cuFunction[0], offset, ...),
        --   offset += sizeof(...);
        (CompStmt $ Blk $
          ( case identity' of -- This is for isExclusive.
            Just _  -> case associativity op of
              Left1  -> isExclusive False
              Right1 -> isExclusive False
              _                  -> []
            Nothing -> case associativity op of
              Left _  -> isExclusive True
              Right _ -> isExclusive True
              _                   -> [])
          ++
          map
            (\ (funcName, tySize, exp, append) ->
              StmtItem $ ExpStmt $ Just $ Exp
                [ toAssignExp $ FuncCall
                  (toPostfixExp $ Ident funcName)
                  (ArgExpList
                    ( [ toAssignExp $ toArrayElem
                        (Ident "cuFunction") (IntegerConst 0)
                      , toAssignExp $ Ident "offset", toAssignExp $ exp]
                    ++if append then [tySize] else []))
                , AddAssign (toUnaryExp $ Ident "offset") tySize])
            [ case param of
              Array {} ->
                ( "cuParamSeti"
                , toAssignExp $ TySize $ TyName
                  [SpecQualTySpec $ TypedefName $ Ident "CUdeviceptr"]
                  Nothing
                , toAssignExp $ Ident $ "d_" ++ paramName param, False)
              Variable {} ->
                ( "cuParamSetv"
                , toAssignExp $ TySize $ TyName
                  [SpecQualTySpec $ paramType param] Nothing
                , toAssignExp $ AddrOf $ toCastExp $ Ident
                $ case associativity op of
                  Left leftVarName ->
                    if leftVarName == paramName param
                      then "startingVal"
                      else paramName param
                  Right rightVarName ->
                    if rightVarName == paramName param
                      then "startingVal"
                      else paramName param
                  _ -> paramName param
                , True)
            | param <- inParams conf ++ [outParam conf]]
          ++
          map
            (\ (ty, exp) -> StmtItem $ ExpStmt $ Just $ Exp
              [ toAssignExp $ FuncCall
                (toPostfixExp $ Ident "cuParamSeti")
                (ArgExpList
                  [ toAssignExp $ toArrayElem
                    (Ident "cuFunction") (IntegerConst 0)
                  , toAssignExp $ Ident "offset", toAssignExp $ exp])
              , AddAssign
                (toUnaryExp $ Ident "offset")
                (toAssignExp $ TySize $ TyName
                  [SpecQualTySpec ty] Nothing)])
            [ ( TypedefName $ Ident "CUdeviceptr"
              , if last == False
                  then toAssignExp $ toArrayElem
                    (Ident $ "d_blockSums") (Ident "j")
                  else toAssignExp $ IntegerConst 0)
            , ( Int $ Just Unsigned
              , toAssignExp $ toArrayElem (Ident "d_n") (Ident "j"))
            , ( Int $ Just Unsigned, toAssignExp $ IntegerConst $ if last then 0 else 1)
            , ( Int $ Just Unsigned
              , toAssignExp $ Eq
                (toEqExp $ toArrayElem (Ident "d_n") (Ident "j"))
                (toRelExp $ Mul
                  ( toMulExp $ toArrayElem (Ident "d_nBlocks") (Ident "j"))
                  ( toCastExp $ IntegerConst $ fromIntegral
                  $ nPerThread conf * ctaSize conf)))])
      -- } else {
        --   cuParamSeti(cuFunction[0], offset, ...),
        --   offset += sizeof(...);
        (CompStmt $ Blk $
          ( case identity' of -- This is for isExclusive.
            Just _  -> case associativity op of
              Left1  -> isExclusive True
              Right1 -> isExclusive True
              _                  -> []
            Nothing -> case associativity op of
              Left _  -> isExclusive False
              Right _ -> isExclusive False
              _                   -> [])
          ++
          map
            (\ (funcName, tySize, exp, append) ->
              StmtItem $ ExpStmt $ Just $ Exp
                [ toAssignExp $ FuncCall
                  (toPostfixExp $ Ident funcName)
                  (ArgExpList
                    ( [ toAssignExp $ toArrayElem
                        (Ident "cuFunction") (IntegerConst 0)
                      , toAssignExp $ Ident "offset", toAssignExp $ exp]
                    ++if append then [tySize] else []))
                , AddAssign (toUnaryExp $ Ident "offset") tySize])
            [ case param of
              Array {} ->
                ( "cuParamSeti"
                , toAssignExp $ TySize $ TyName
                  [SpecQualTySpec $ TypedefName $ Ident "CUdeviceptr"]
                  Nothing
                , toAssignExp $ toArrayElem
                  (Ident $ "d_blockSums")
                  (Sub (toAddExp $ Ident "j") (toMulExp $ IntegerConst 1)), False)
              Variable {} ->
                ( "cuParamSetv"
                , toAssignExp $ TySize $ TyName
                  [SpecQualTySpec $ paramType param] Nothing
                , toAssignExp $ AddrOf $ toCastExp $ Ident
                $ case associativity op of
                  Left leftVarName ->
                    if leftVarName == paramName param
                      then "startingVal"
                      else paramName param
                  Right rightVarName ->
                    if rightVarName == paramName param
                      then "startingVal"
                      else paramName param
                  _ -> paramName param
                , True)
            | param <- inParams conf ++ [outParam conf]]
          ++
          map
            (\ (ty, exp) -> StmtItem $ ExpStmt $ Just $ Exp
              [ toAssignExp $ FuncCall
                (toPostfixExp $ Ident "cuParamSeti")
                (ArgExpList
                  [ toAssignExp $ toArrayElem
                    (Ident "cuFunction") (IntegerConst 0)
                  , toAssignExp $ Ident "offset", toAssignExp $ exp])
              , AddAssign
                (toUnaryExp $ Ident "offset")
                (toAssignExp $ TySize $ TyName
                  [SpecQualTySpec ty] Nothing)])
            ( [ ( TypedefName $ Ident "CUdeviceptr"
                , if last == False
                    then toAssignExp $ toArrayElem
                      (Ident $ "d_blockSums") (Ident "j")
                    else toAssignExp $ IntegerConst 0)
              , ( Int $ Just Unsigned
                , toAssignExp $ toArrayElem (Ident "d_n") (Ident "j"))
              , ( Int $ Just Unsigned, toAssignExp $ IntegerConst $ if last then 0 else 1)
              , ( Int $ Just Unsigned
                , toAssignExp $ Eq
                  (toEqExp $ toArrayElem (Ident "d_n") (Ident "j"))
                  (toRelExp $ Mul
                    ( toMulExp $ toArrayElem (Ident "d_nBlocks") (Ident "j"))
                    ( toCastExp $ IntegerConst $ fromIntegral
                    $ nPerThread conf * ctaSize conf)))]))]
      -- }
      ++
      -- cuParamSetSize(cuFunction[0], offset);
      -- cuLaunchGrid(cuFunction[0], numBlocks[j], 1);
      -- cuCtxSynchronize();
      [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuParamSetSize")
        (ArgExpList
          [ toAssignExp $ toArrayElem
            (Ident "cuFunction") (IntegerConst 0)
          , toAssignExp $ Ident "offset"])]
      , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuLaunchGrid")
        (ArgExpList
          [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 0)
          , toAssignExp $ toArrayElem (Ident "d_nBlocks") (Ident "j")
          , toAssignExp $ IntegerConst 1])]
      , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuCtxSynchronize") (ArgExpList [])]]
    isExclusive flag = [StmtItem $ ExpStmt $ Just $ Exp
      [ toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuParamSeti")
        (ArgExpList
          [ toAssignExp $ toArrayElem
            (Ident "cuFunction") (IntegerConst 0)
          , toAssignExp $ Ident "offset"
          , toAssignExp $ IntegerConst $ if flag then 1 else 0])
      , AddAssign
        (toUnaryExp $ Ident "offset")
        (toAssignExp $ TySize $ TyName
          [SpecQualTySpec (Int $ Just Unsigned)] Nothing)]]
    identity' =  case scalar conf of
      Just scalar'  -> identity scalar'
      Nothing       -> Nothing
exec conf op@(Scan {}) =
  exec conf op'
  ++
  [ StmtItem $ IterStmt $ For
    (Just $ Exp [Assign
      (toUnaryExp $ Ident "j")
      (toAssignExp $ Sub (toAddExp $ Ident "level") (toMulExp $ IntegerConst 1))])
    (Just $ Exp [toAssignExp $ Ge
      (toRelExp $ Ident "j") (toShftExp $ IntegerConst 0)])
    (Just $ Exp [toAssignExp $ UnaryDec $ toUnaryExp $ Ident "j"])
    (CompStmt $ Blk $
      -- cuFuncSetBlockShape(cuFunction[1], ctaSize, 1, 1);
      [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuFuncSetBlockShape")
        (ArgExpList
          [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 1)
          , toAssignExp $ IntegerConst $ fromIntegral $ ctaSize conf
          , toAssignExp $ IntegerConst 1, toAssignExp $ IntegerConst 1])]
      -- offset = 0;
      , StmtItem $ ExpStmt $ Just $ Exp [Assign
        (toUnaryExp $ Ident "offset") (toAssignExp $ IntegerConst 0)]
      -- if (j == 0) {
      , StmtItem $ SelectStmt $ IfElse
        (Exp [toAssignExp $ Eq
          (toEqExp $ Ident "j") (toRelExp $ IntegerConst 0)])
        --   cuParamSeti(cuFunction[1], offset, ...),
        --   offset += sizeof(...);
        (ExpStmt $ Just $ Exp
          [ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "cuParamSeti")
            (ArgExpList
              [ toAssignExp $ toArrayElem
                (Ident "cuFunction") (IntegerConst 1)
              , toAssignExp $ Ident "offset"
              , toAssignExp $ Ident
              $ "d_" ++ (paramName $ head $ [outParam conf])])
          , AddAssign
            (toUnaryExp $ Ident "offset")
            (toAssignExp $ TySize $ TyName
              [SpecQualTySpec $ TypedefName $ Ident "CUdeviceptr"]
              Nothing)])
      -- } else {
        --   cuParamSeti(cuFunction[1], offset, ...),
        --   offset += sizeof(...);
        (ExpStmt $ Just $ Exp
          [ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "cuParamSeti")
            (ArgExpList
              [ toAssignExp $ toArrayElem
                (Ident "cuFunction") (IntegerConst 1)
              , toAssignExp $ Ident "offset"
              , toAssignExp $ toArrayElem
                (Ident $ "d_blockSums")
                (Sub (toAddExp $ Ident "j") (toMulExp $ IntegerConst 1))])
          , AddAssign
            (toUnaryExp $ Ident "offset")
            (toAssignExp $ TySize $ TyName
              [SpecQualTySpec $ TypedefName $ Ident "CUdeviceptr"]
              Nothing)])]
      -- }
      ++
      -- cuParamSeti(cuFunction[1], offset, ...),
      -- offset += sizeof(...);
      map
        (\ (ty, exp) -> StmtItem $ ExpStmt $ Just $ Exp
          [ toAssignExp $ FuncCall
            (toPostfixExp $ Ident "cuParamSeti")
            (ArgExpList
              [ toAssignExp $ toArrayElem
                (Ident "cuFunction") (IntegerConst 1)
              , toAssignExp $ Ident "offset", toAssignExp $ exp])
          , AddAssign
            (toUnaryExp $ Ident "offset")
            (toAssignExp $ TySize $ TyName [SpecQualTySpec ty] Nothing)])
        [ ( TypedefName $ Ident "CUdeviceptr"
          , toAssignExp $ toArrayElem (Ident $ "d_blockSums") (Ident "j"))
        , ( Int $ Just Unsigned
          , toAssignExp $ toArrayElem (Ident "d_n") (Ident "j"))]
      ++
      -- cuParamSetSize(cuFunction[1], offset);
      -- cuLaunchGrid(cuFunction[1], numBlocks[j], 1);
      -- cuCtxSynchronize();
      [ StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuParamSetSize")
        (ArgExpList
          [ toAssignExp $ toArrayElem
            (Ident "cuFunction") (IntegerConst 1)
          , toAssignExp $ Ident "offset"])]
      , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuLaunchGrid")
        (ArgExpList
          [ toAssignExp $ toArrayElem (Ident "cuFunction") (IntegerConst 1)
          , toAssignExp $ toArrayElem (Ident "d_nBlocks") (Ident "j")
          , toAssignExp $ IntegerConst 1])]
      , StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
        (toPostfixExp $ Ident "cuCtxSynchronize") (ArgExpList [])]])]
  where
    op' :: Operation
    op' = Fold
      { kernel = reduceKernel op
      , associativity = associativity op}

--
-- memcpyDtoH generates the code to transfer the output parameters from the
-- device global memory.
--
-- cuMemcpyDtoH(&...[i], d_..., _n * sizeof (t));
-- where ... is the name of the parameter, i is the index of the first element
-- for the pass, _n is the number of elements to be processed for the pass,
-- and t is the type of the elements in ....
--
memcpyDtoH :: HostCodeConfig -> [BlkItem]
memcpyDtoH conf = map
  (\ (ty, varName) -> StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuMemcpyDtoH")
    (ArgExpList
      [ case op conf of
        Fold {} -> toAssignExp $ Ident varName
        _         -> toAssignExp $ AddrOf $ toCastExp $ toArrayElem
          (Ident varName) (Ident "i")
      , toAssignExp $ Ident $ "d_" ++ varName
      , toAssignExp $ Mul
        (case op conf of
          Map           {} -> toMulExp $ Ident "d_n"
          Fold        {} -> toMulExp $ IntegerConst 1
          Scan {} -> toMulExp $ toArrayElem
            (Ident "d_n") (IntegerConst 0))
        (toCastExp $ TySize $ TyName [SpecQualTySpec ty] Nothing)])])
  [(paramType param, paramName param) | param <- [outParam conf]]

--
-- adjust generates the code that adjusts parameters for the next pass using
-- the result of the current pass.
--
adjust :: HostCodeConfig -> Operation -> [BlkItem]
adjust conf op@(Map {}) = []
adjust conf op@(Fold {}) = []
adjust conf op@(Scan {associativity = Left _}) =
  [StmtItem $ ExpStmt $ Just $ Exp [Assign
    (toUnaryExp $ Ident "startingVal")
    (toAssignExp $ FuncCall
      (toPostfixExp $ Ident $ "_" ++ progName conf ++ "Scalar")
      (ArgExpList
        [ toAssignExp $ toArrayElem
          (Ident $ paramName param)
          (Sub
            (Add
              (toAddExp $ Ident "i")
              (toMulExp $ toArrayElem (Ident "d_n") (IntegerConst 0)))
            (toMulExp $ IntegerConst 1))
        | param@(Array {}) <- (outParam conf):(inParams conf)]))]]
adjust conf op@(Scan {associativity = Left1}) =
  [StmtItem $ ExpStmt $ Just $ Exp [Assign
    (toUnaryExp $ toArrayElem (Ident "xs")
      (Add
        (toAddExp $ Ident "i")
        (toMulExp $ toArrayElem (Ident "d_n") (IntegerConst 0))))
    (toAssignExp $ FuncCall
      (toPostfixExp $ Ident $ "_" ++ progName conf ++ "Scalar")
      (ArgExpList $
        ( toAssignExp $ toArrayElem
          (Ident $ paramName $ outParam conf)
          (Sub
            (Add
              (toAddExp $ Ident "i")
              (toMulExp $ toArrayElem (Ident "d_n") (IntegerConst 0)))
            (toMulExp $ IntegerConst 1)))
        :[toAssignExp $ toArrayElem
          (Ident $ paramName param)
          (Add
            (toAddExp $ Ident "i")
            (toMulExp $ toArrayElem (Ident "d_n") (IntegerConst 0)))
        | param@(Array {}) <- inParams conf]))]]

--
-- free generates the cuMemFree statements to free the memory allocated on
-- the device global memory.
--
-- cuMemFree(d_..);
--
free :: HostCodeConfig -> [BlkItem]
free conf = map
  (\ arg -> StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
    (toPostfixExp $ Ident "cuMemFree")
    (ArgExpList [toAssignExp $ Ident arg])])
  [ "d_" ++ paramName param
  | param@(Array {}) <- inParams conf ++ [outParam conf]]

--
-- finalise generates the code to finalise the context.
--
-- cuCtxPopCurrent(NULL);
-- cuCtxDestroy(cuContext);
--
finalise :: [BlkItem]
finalise = map
  (\ (funcName, arg) ->
    StmtItem $ ExpStmt $ Just $ Exp [toAssignExp $ FuncCall
      (toPostfixExp $ Ident funcName) (ArgExpList [toAssignExp $ Ident arg])])
  [("cuCtxPopCurrent", "NULL"), ("cuCtxDestroy", "cuContext")]

--
main :: IO ()
main =
  let conf :: HostCodeConfig
      conf = HostCodeConfig
        { progName = "Test"
        , ctaSize = 64
        , nPerThread = 8
        , sharedMemSize = Nothing
        , inParams = [Array "xs" Float]
        , outParam = Array "out" Float
        , scalar = Nothing
        , op = Map {kernel = DeviceCodeGen.Map}}
  in  putStrLn $ show $ hostCodeGen conf
