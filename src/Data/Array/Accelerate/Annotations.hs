{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Annotations for Accelerate's abstract syntax trees.
--
-- * TODOs
--
-- TODO: Document what exactly we are annotating and how we do that once this
--       has been fleshed out a little more.
-- TODO: Add the same file header used in all other modules
-- TODO: Reformat all of the changes from this branch to the usual Accelerate
--       style. There's no style guide or formatter config anywhere, so I just
--       run most things through Brittany to keep me sane.
-- TODO: Document the process around freezing call stacks
-- TODO: Figure out the let/var conversion in sharing recovery
-- TODO: Take another look at fusion, optimization propagation doesn't have to
--       be perfect yet but I'm sure there are also issues elsewhere
-- TODO: See if we can clean up the pretty printer a bit, and also add the
--       information to the graphviz export (there's already a todo for that)
-- TODO: Expose the pretty printer verbosity option somehow
-- TODO: Tests! Tests? Can we test this, and how? We can probably at least fake
--       call stacks, generate random ASTs with smart constructors, and check
--       `getAnn`.
--
-- There are a bunch more todos sprinkled around the code. Use the following
-- mystical one-liner to find them:
--
-- @
-- git diff -U0 master...feature/annotations | grep '^+.*\(TODO\|HACK\|FIXME\)' | cut -c2- | git grep -nFf- feature/annotations
-- @
--
-- * Annotations
--
-- The general idea is that almost all AST data type constructors have been
-- extended with an 'Ann' field, which stores annotation data for the AST node.
-- Presently, we use this to store both source mapping information and flags
-- that can be used by the optimizer. These annotations are populated in the
-- smart constructors using the 'mkAnn' function, so they end up being
-- completely transparent to the user.
--
-- ** Capturing call stacks
--
-- 'mkAnn' will try to capture source information in the form of GHC Call
-- Stacks. This ends up being a breaking change to any library building on
-- Accelerate. In short, the changes that need to be made are as follows:
--
--   1. All smart constructors or library functions that are exposed to the
--      user, as well as all auxiliary functions that either directly or
--      indirectly end up calling 'mkAnn', need to be annotated with the
--      'HasCallStack' constraint.
--   2. Top-level smart constructors or library functions that are exported from
--      a module need to freeze the call stack at the entry point. See below for
--      more details on how to do this correctly.
--   3. When translating between different AST types (such as the internal De
--      Bruijn AST to a backend-specific AST), the annotations need to be
--      propgated to that new AST nodes. If needed, multiple annotations can be
--      merged with the '(<>)' operator.
--
-- If either (1) or (2) is omitted, then 'mkAnn' will throw an assertion
-- failure. These steps are required in order to access the source location in
-- the user's code where the toplevel library function was called from.
--
-- ** Freezing call stacks
--
-- A function builds a compile-time call stack with information about its caller
-- when it is annotated with the 'HasCallStack' constraint. The call stack can
-- be frozen with the 'withFrozenCallStack' function, which causes its argument
-- to be evaluated in a way that prevents it from accumulating additional stack
-- frames in its call stack. GHC Call Stacks are implemented using implicit
-- parameters. This means that the location in the source code a variable was
-- defined in - and this was the point where I realized we can cut a lot of
-- complexity by using our own implicit parameter for the source mapping.
--
-- FIXME: Continue updating the module documentation where I left off, I got
--        kind of sidetracked by the implicit parameters at this point
--
-- TODO: Mention pattern synonyms
-- TODO: Mention RTS call stacks
--
-- TODO: See what we still need from the below comments, get rid of the rest
--
-- ** Annotations in the smart AST
--
--   * At the moment only a handful of 'PreSmartExp' and 'PreSmartAcc'
--     constructors have annotation fields.
--   * Call stacks are frozen in all of the exposed frontend functions and in
--     the (generated) pattern synonyms. This allows us to capture them in
--     'mkAnn' so they can be used later to map an AST back to the original
--     source location. This does require the 'HasCallStack' constraint to be
--     added to every function that either directly or indirectly calls 'mkAnn'.
--
-- TODO: Pattern synonyms using 'Pattern' should probably pop another layer of
--       call stacks since those are never used directly (there's another TODO
--       for this). Also check if this works for index pattern synonyms like I2
--       and I3.
-- TODO: Instead of relying on 'HasCallStack', since we already freeze the call
--       stacks at the top level we can also use our own implicit parameter.
--       This would at least alleviate the need to litter every frontend
--       function with 'HasCallStack'.
--
-- ** Annotations in the de Bruijn AST
--
--   * The internal AST also contains fields in the constructors that correspond
--     to the annotated constructors of the smart AST.
--   * Annotations are propagated through the entire transformation pipeline.
--     When the smart AST gets transformed into the internal AST during sharing
--     recovery the annotations passed through as is.
--   * When AST nodes get combined into a new node, for instance during the
--     simplification and constant folding processes, the new node's annotation
--     is created by joining the annotations of all involved nodes.
--
--     TODO: Elaborate in this
--
-- TODO: Annotations are completely ignored in 'Match' and 'Hash' at the moment.
--       We should probably at least consider the optimizations of not the
--       entire annotation.
--
-- ** Annotations in the delayed representation
--
--   * In the fusion process some of the original nodes will disappear as they
--     are replaced with cunctations and eventually end up as delayed array
--     computations.
--   * During this process we will preserve the annotations of the delayed and
--     fused AST nodes in the constructors of those cunctations an delayed
--     arrays.
--
-- TODO: Figure out what to do with conflicting optimization flags in the fusion
--       process. If we fuse an unrolled map into a fold, should:
--
--       a) The resulting fold be unrolled as well? To make things easier this
--          is the current behaviour.
--       b) The optimization flag be ignored (with a warning for the user)?
--       c) We throw a hard error and tell the user to fix this themselves?
-- TODO: Code gen changes
--
-- ** Annotating ASTs
--
-- AST nodes will automatically contain source mapping information because of
-- the use of smart constructors. The user can specify optimization flags for an
-- AST node by using the optimization functions exposed from
-- @Data.Array.Accelerate@.
--
-- The annotation type stores source mapping information in a set so we can
-- easily merge and transform AST nodes in the optimization process while still
-- preserving information about the node's origins.
--
-- TODO: Rewrite the above, this was written a while back and it's now both
--       missing bits and also repeating other information
-- XXX: Right now it would be possible to specify some nonsensible flags, like
--      setting loop unrolling for a constant value. Should we just silently
--      ignore these things like we do now, or should be printing warnings? I
--      don't think Accelerate has any other non-fatal compiler diagnostics.
module Data.Array.Accelerate.Annotations
    ( -- * Annotations
      Ann(..)
    , Optimizations(..)
    , HasAnnotations(..)
    , withOptimizations
    , extractAnn
    , alwaysInline
    , unrollIters
      -- * Source mapping
    , SourceMapped
    , sourceMap
    , sourceMapRuntime
    , sourceMapPattern
      -- * Internals
    , FieldAnn(..)
    , mkAnn
    , mkDummyAnn
    , rnfAnn
    , liftAnn
      -- * Re-exported for convenience
    , HasCallStack
    ) where

import           Data.Array.Accelerate.Orphans  ( )

import           Control.DeepSeq                ( rnf )
import qualified Data.HashSet                  as S
import           Data.Maybe                     ( mapMaybe )
import qualified GHC.ExecutionStack            as ES
import           GHC.IO                         ( unsafePerformIO )
import           GHC.Stack
import           GHC.Stack.Types                ( CallStack(..) )
import           Lens.Micro
import           Lens.Micro.Extras              ( view )
import           Language.Haskell.TH            ( Q
                                                , TExp
                                                )


-- | This annotation type stores any auxiliary data attached to an AST node.
-- This includes source mapping information if available, as well as any local
-- optimization flags.
--
-- The locations field contains a call stack pointing to the location in the
-- user's code where the AST node was created. During the transformation
-- pipeline multiple AST nodes may be merged, in which case 'locations' can
-- contain multiple (but likely adjacent) call stacks.
--
-- See 'SourceMapped', 'sourceMap', and 'mkAnn' for more information.
--
-- TODO: The set of optimizations is now the same for 'Exp' and 'Acc'. Should we
--       have separate sets of 'Optimizations' flags? Ideally we would only
--       allow adding optimization flags for constructs that make sense (e.g.
--       allow unrolling @awhile@, but not @acond@), but since this would
--       involve adding another type index to 'Exp' that's not going to be a
--       feasible approach.
data Ann = Ann
    { locations     :: S.HashSet CallStack
    , optimizations :: Optimizations
    }

-- | Some example annotations. These do not actually do anything yet. Having
-- these as a record makes it possible to easily pattern match on them without
-- having to do list or set lookups everywhere. Because of record wild cards we
-- can still easily add additional annotations without having to modify all uses
-- of this type.
--
-- TODO: After the source mapping is done, we should add the rest of the
--       optimizations here and actually make them do something.
data Optimizations = Optimizations
    { optAlwaysInline :: Bool
    , optUnrollIters  :: Maybe Int
    }


-- * Annotation functions
--
-- These are exposed to the user so they can annotate AST nodes.

-- | Instruct the compiler to always inline this expression and to not perform
-- any sharing recovery. This will allow inexpensive calculations whose values
-- are used in multiple places to be fused, potentially increasing performance
-- since the values don't have to be written to memory anymore.
alwaysInline :: HasAnnotations a => a -> a
alwaysInline = withOptimizations $ \opts -> opts { optAlwaysInline = True }

-- | Instruct the compiler to unroll a loop in chunks of @n@ iterations.
--
-- TODO: Should we add validation for these kinds of functions? (i.e. reject
--       negative values for @n@)
unrollIters :: HasAnnotations a => Int -> a -> a
unrollIters n = withOptimizations $ \opts -> opts { optUnrollIters = Just n }


-- * Source mapping

-- | This indicates that a function requires source mapping.
--
-- Every function that directly or indirectly ends up creating source mapping
-- annotations through 'mkAnn' needs to either be annotated with this
-- constraint, or if it's a top-level library function then it needs to evaluate
-- anything requiring source mapping through 'sourceMap'. This uses the type
-- checker to reduce the likelihood of making a mistake by enforcing call stacks
-- in all functions that either directly or indirectly need them. See the module
-- description for more information.
type SourceMapped = (?requiresSourceMapping :: ReadTheDocs, HasCallStack)

-- | A tag type that exists only to enforce the source mapping constraint
-- through the type checker.
data ReadTheDocs = TakenCareOf

-- | Evaluate the provided form with source mapping enabled. See the module
-- documentation for a more in-depth explanation on how to use this. The
-- function calling this should be a top-level library function or smart
-- constructor that has annotated with the 'HasCallStack' constraint. This will
-- cause the call stack at that function (which thus includes that function's
-- caller) to be used within the 'SourceMapped' context of the form.
--
-- /NOTE:/
--
-- This abstraction exists to prevent mistakes, as 'mkAnn' and any function
-- calling it need to either be 'SourceMapped', or they need to evaluate things
-- through this 'sourceMap' function. However, there are still two ways to make
-- a mistake here:
--
--   1. Since we want to know the location in the user's source code a library
--      function was called from, 'sourceMap' should only ever be called from
--      top-level functions that are exposed directly to the user. Every other
--      place should simply propagate the 'SourceMapped' constraint.
--   2. This mechanism cannot prevent against mistakes when calling a smart
--      constructor or library function from another smart constructor or
--      library function. When this happens and the first function doesn't call
--      the second smart constructor through 'sourceMapped', then the source
--      mapping annotation will contain the location of that first function
--      rather that of its caller.
sourceMap :: HasCallStack => (SourceMapped => a) -> a
sourceMap dewit =
    let ?requiresSourceMapping = TakenCareOf
        -- Same definition as in 'withFrozenCallStack'
        ?callStack             = freezeCallStack (popCallStack callStack)
    in
    if isEmptyStack ?callStack
        then printError
        else dewit
  where
    -- This error will be printed using the old call stack, which should include
    -- the caller of this function if the call stack has not yet been frozen.
    printError = error
      $  "Functions calling 'sourceMap' need to be annotated with 'HasCallStack'. "
      <> "If that's not possible, then you should use 'sourceMapRuntime' instead."

-- | Performs the same duty as 'sourceMap', but for top-level functions that do
-- not have the 'HasCallStack' constraint. If it is possible to add that
-- constraint, then 'sourceMap' should be used instead as these run time call
-- stacks are not guaranteed to be available. In practice, this is only used as
-- a fallback for prelude type class implementations.
--
-- This will transform the RTS Execution Stack into a frozen GHC Call Stack so
-- it can interact with our other call stack-based machinery. If an execution
-- stack frame is not available, then the computation will be evaluated with an
-- empty call stack instead.
--
-- /NOTE:/
--
-- Execution stacks __only__ works when GHC has been built with libdw:
--
-- > ghc --info | grep libdw
--
-- You can build a version of GHC with DWARF call stacks enabled using:
--
-- > ghcup compile ghc -b INSTALLED_GHC_VERSION -v 9.2.0.20210422 -j $(nproc) -- --enable-dwarf-unwind
--
-- TODO: Test whether this actually uses the correct stack frame
-- FIXME: This will need some more work. The main issues are that the stack
--        contains a lot of frames at the top and the bottom that would need to
--        be stripped, and that tail call optimization interferes with these
--        execution stacks.
sourceMapRuntime :: HasCallStack => (SourceMapped => a) -> a
sourceMapRuntime dewit =
    let ?requiresSourceMapping = TakenCareOf
        -- Only create a frozen call stack if we do not already have a valid
        -- frozen call stack
        ?callStack = case ?callStack of
            x@(FreezeCallStack _) -> x
            _ -> freezeCallStack . toCallStack $ unsafePerformIO ES.getStackTrace
     in dewit
  where
    -- We don't want the two uppermost stack frames, since those will be in our
    -- own library code
    -- TODO: Is this correct? Should we drop only one stack frame?
    toCallStack :: Maybe [ES.Location] -> CallStack
    toCallStack (Just (_ : _ : locs)) = fromCallSiteList $ mapMaybe locToCallSite locs
    toCallStack _                     = emptyCallStack

    locToCallSite :: ES.Location -> Maybe (String, SrcLoc)
    locToCallSite (ES.Location _ fn (Just loc)) = Just
        ( fn
        , SrcLoc { srcLocPackage   = ""
                 , srcLocModule    = ""
                 , srcLocFile      = ES.sourceFile loc
                 , srcLocStartLine = ES.sourceLine loc
                 , srcLocStartCol  = ES.sourceColumn loc
                 , srcLocEndLine   = ES.sourceLine loc
                 , srcLocEndCol    = ES.sourceColumn loc
                 }
        )
    locToCallSite (ES.Location _ _ Nothing) = Nothing

-- | Workaround for pattern synonyms and call stacks not working as expected in
-- GHC versions 9.0.x and below. Performs the same duty as 'sourceMap'. On the
-- unsupported GHC versions, this will freeze an empty call stack if the current
-- call stack isn't already frozen. Otherwise we would capture the wrong call
-- stacks.
--
-- /HACK:/
--
-- Call stacks didn't play nicely with pattern synonyms in GHC version before
-- 9.2, so to prevent incorrect source annotations we'll prevent them from being
-- generated completely.
--
-- https://gitlab.haskell.org/ghc/ghc/-/issues/19289
--
-- TODO: Since 'Pattern' isn't meant to be used directly, should we strip off
--       two layers of call stack? Check how call stacks interact with pattern
--       synonyms in GHC 9.2.0.
sourceMapPattern :: HasCallStack => (SourceMapped => a) -> a
sourceMapPattern dewit =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
  let ?requiresSourceMapping = TakenCareOf
      ?callStack =
        -- Same definition as in 'withFrozenCallStack'
        case freezeCallStack (popCallStack callStack) of
          stack | isEmptyStack stack -> printError
          stack                      -> stack
     in dewit
  where
    -- This error will be printed using the old call stack, which should include
    -- the caller of this function if the call stack has not yet been frozen.
    -- We'll obviously on do this on GHC 9.2 and up.
    printError = error
      $  "Functions calling 'sourceMapPattern' need to be annotated with "
      <> "'HasCallStack'. If that's not possible, then you should use "
      <> "'sourceMapRuntime' instead."
#else
  let ?requiresSourceMapping = TakenCareOf
      ?callStack =
        -- Only freeze an empty call stack of the call stack isn't already
        -- frozen, i.e. when it is used internally within Accelerate's front end
        -- standard library
        case ?callStack of
          x@(FreezeCallStack _) -> x
          _                     -> freezeCallStack emptyCallStack
     in dewit
#endif

-- | We'll throw an error when 'sourceMap' or 'sourceMapPattern' gets called
-- from a function that hasn't been annotated with 'HasCallStack'. Frozen empty
-- call stacks are okay, because that indicates that we've already taken care of
-- it (in either 'sourceMapRuntime' or 'sourceMapPattern').
isEmptyStack :: CallStack -> Bool
isEmptyStack EmptyCallStack = True
isEmptyStack _              = False


-- * Internal types and functions

-- | Used for accessing annotation fields in ASTs. 'HasAnnotations' defines
-- convenience functions for working with types that have such an annotation
-- field and for functions that
--
-- TODO: Think of a better name
class FieldAnn a where
    -- | A lens for accessing @a@'s annotation field, if it has one. By defining
    -- this as a lens we can get rid of some duplication.
    _ann :: Lens' a (Maybe Ann)

-- | Used for modifying an AST node's annotations. Types with annotation fields
-- should have an instance of 'FieldAnn' instead of implementing this class
-- directly.
class HasAnnotations a where
    -- | Modify the annotation stored in an AST node. This may not do anything
    -- when the AST node doesn't support annotations.
    modifyAnn :: (Ann -> Ann) -> a -> a
    -- | Extract the annotation from an AST node, if it has one. This is used
    -- during some of the transformations when we may no longer have access to the
    -- original AST nodes.
    getAnn :: a -> Maybe Ann

-- | Lenses make accessing these annotations for different ASTs much cleaner,
-- but these speciality functions are much nicer to work with.
instance {-# OVERLAPPING #-} FieldAnn a => HasAnnotations a where
    modifyAnn f = over _ann $ \case
      Just ann -> Just (f ann)
      Nothing  -> Nothing
    getAnn = view _ann

-- | Being able to directly annotate functions makes using this annotation
-- functionality much more ergonomic.
instance {-# OVERLAPPING #-} HasAnnotations r => HasAnnotations (a -> r) where
    modifyAnn f f' x = modifyAnn f (f' x)
    -- You cannot get the annotation without evaluating the function first. This
    -- is kind of an edge cases where getAnn doesn't make any sense.
    getAnn _ = Nothing

-- | Change the optimization flags for an AST node.
withOptimizations :: HasAnnotations a => (Optimizations -> Optimizations) -> a -> a
withOptimizations f = modifyAnn $ \(Ann src opts) -> Ann src (f opts)

-- | A helper to extract an annotation from an expression, or to return an empty
-- annotation if the expression doesn't contain one.
extractAnn :: HasAnnotations a => a -> Ann
extractAnn (getAnn -> Just ann) = ann
extractAnn _                    = mkDummyAnn

-- | Create an empty annotation with call site information if available. This
-- only works when all smart constructors have the 'HasCallStack' constraint.
-- This function __must__ be called with 'withFrozenCallStack'.
--
-- TODO: Update the documentation after the SourceMapped change. Also consider
--       removing the assertion.
--
-- TODO: When Accelerate has a logger, this assertion should be replaced by a
--       warning. If the call stacks are not frozen, then we'll just treat it as
--       an empty call stack. When running the test suite this should still
--       count as a hard error though.
mkAnn :: SourceMapped => Ann
mkAnn = Ann (maybeCallStack callStack) defaultOptimizations
  where
    -- If we encounter a frozen empty call stack, then this means that the
    -- caller of 'getAnn' explicitly stated that there is no source information
    -- available. We can get nested frozen call stacks when top level functions
    -- call other top level functions. In that case we'll recursively strip the
    -- frozen call stack parts until we get something useful.
    maybeCallStack (FreezeCallStack EmptyCallStack           ) = S.empty
    maybeCallStack (FreezeCallStack stack@(FreezeCallStack _)) = maybeCallStack stack
    maybeCallStack (FreezeCallStack stack)                     = S.singleton stack
    -- This would only be reachable when bypassing the `SourceMapped` constraint
    -- with a bottom value, since the @sourceMapped*@ always freeze the call
    -- stack
    maybeCallStack _ = error "Nice try, but no cigar"

    defaultOptimizations =
        Optimizations { optAlwaysInline = False, optUnrollIters = Nothing }

-- | Create a new 'Ann' without any source information.
--
-- TODO: Once everything has been implemented, check whether the uses of this
--       are justified and if we're not throwing away any existing annotations
--       when reconstructing ASTs
mkDummyAnn :: Ann
mkDummyAnn = let ?requiresSourceMapping = TakenCareOf
                 ?callStack             = freezeCallStack emptyCallStack
             in  mkAnn


instance Semigroup Ann where
    (Ann src1 opts1) <> (Ann src2 opts2) = Ann (src1 <> src2) (opts1 <> opts2)

instance Monoid Ann where
    mempty = mkDummyAnn

instance Semigroup Optimizations where
    a <> b = Optimizations
        { optAlwaysInline = optAlwaysInline a || optAlwaysInline b
        , optUnrollIters  = (max `maybeOn` optUnrollIters) a b
        }
      where
        -- 'on' from 'Data.Function' but for comparing 'Maybe' values.
        maybeOn f on' x y = case (on' x, on' y) of
            (Just x', Just y') -> Just $ f x' y'
            (Just x', _      ) -> Just x'
            (_      , Just y') -> Just y'
            _                  -> Nothing

-- * Internal
--
-- ** Normal form data
--
-- Used as part of 'rnfPreOpenAcc'.

rnfAnn :: Ann -> ()
rnfAnn (Ann src opts) = rnf src `seq` rnfOptimizations opts

rnfOptimizations :: Optimizations -> ()
rnfOptimizations Optimizations { optAlwaysInline, optUnrollIters } =
    optAlwaysInline `seq` rnf optUnrollIters

-- ** Quotation
--
-- Used as part of 'liftOpenExp' when quoting an AST.

liftAnn :: Ann -> Q (TExp Ann)
liftAnn (Ann src opts) =
    [|| Ann $$(liftCallStacks src) $$(liftOptimizations opts) ||]

liftOptimizations :: Optimizations -> Q (TExp Optimizations)
liftOptimizations Optimizations { .. } = [|| Optimizations { .. } ||]

liftCallStacks :: S.HashSet CallStack -> Q (TExp (S.HashSet CallStack))
liftCallStacks stacks = [|| S.fromList $$(liftStacks $ S.toList stacks) ||]
  where
    -- TODO: Is there some combinator for this transformation?
    liftStacks :: [CallStack] -> Q (TExp [CallStack])
    liftStacks (x : xs) = [|| $$(liftCallStack x) : $$(liftStacks xs) ||]
    liftStacks []       = [|| [] ||]

liftCallStack :: CallStack -> Q (TExp CallStack)
liftCallStack EmptyCallStack = [|| EmptyCallStack ||]
liftCallStack (PushCallStack fn loc stack) =
    [|| PushCallStack fn $$(liftSrcLoc loc) $$(liftCallStack stack) ||]
liftCallStack (FreezeCallStack stack) =
    [|| FreezeCallStack $$(liftCallStack stack) ||]

liftSrcLoc :: SrcLoc -> Q (TExp SrcLoc)
liftSrcLoc SrcLoc {..} = [|| SrcLoc { .. } ||]
