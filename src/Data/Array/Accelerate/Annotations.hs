{-# LANGUAGE CPP             #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotations for Accelerate's abstract syntax trees.
--
-- TODO: Document what exactly we are annotations, how, and with what once this
--       has been fleshed out a little more.
--
-- TODO: Add the same file header used in all other modules
-- TODO: Reformat all of the changes from this branch to the usual Accelerate
--       style. There's no style guide or formatter config anywhere, so I just
--       run most things through Brittany to keep me sane.
--
-- * Implementation status
--
-- There are more todos sprinkled around the code. Use the following mystical
-- one-liner to find them:
--
-- @
-- git diff -U0 master...feature/annotations | grep '^+.*\(TODO\|HACK\|FIXME\)' | cut -c2- | git grep -nFf- feature/annotations
-- @
--
-- TODO: Instead of deriving show, we should modify the pretty printer to show
--       these annotations in a nice way. Should we also present source mapping
--       information? That would be very useful but it also adds a lot of noise,
--       is there some 'verbose' pretty printing mode?
-- TODO: Tests!
--
-- ** Annotations in the smart AST
--
-- TODO: Only a few @PreSmartExp@ constructors have an annotation field right
--       now
-- TODO: There are no annotations in @PreSmartAcc@ yet
-- TODO: Annotations for product pattern synnoyms using @Pattern@/@IsPattern@
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
-- TODO: Add the same annotations as in the Smart ASTs, and make sure that they
--       are propagated properly through the transformations.
-- TODO: Annotations are completely ignored in 'Match' and 'Hash' at the moment.
--       We should probably at least consider the optimizations of not the
--       entire annotation.
--
-- TODO: The rest of the process up until codegen
--
-- ** Annotations the transformations
--
-- TODO: There are a lot of TODOs, FIXMEs and dummy annotations there left
--
-- ** Annotations in the delayed representation
--
-- TODO: Figure out how this is going to work. Currently all array level
--       annotations are thrown out when we reach fusion.
--
-- ** Annotating ASTs
--
-- AST nodes will automatically contain source mapping information because of
-- the use of smart constructors. The user can specify optimization flags for an
-- AST node by using the optimization functions exposed from
-- @Data.Array.Accelerate.Smart@.
--
-- The annotation type stores source mapping information in a set so we can
-- easily merge and transform AST nodes in the optimization process while still
-- preserving information about the node's origins.
--
-- XXX: Right now it would be possible to specify some nonsensible flags, like
--      setting loop unrolling for a constant value. Should we just silently
--      ignore these things like we do now, or should be printing warnings? I
--      don't think Accelerate has any other non-fatal compiler diagnostics.
--
-- TODO: Call stacks should be frozen for any function exposed to the user,
--       right now this is not yet the case.
--
-- ** AST transformations
--
-- When doing transformations over the AST, for instance when applying
-- simplification rules, then we'll combine existing annotation fields to create
-- new annotations for any new artificially generated AST nodes. This allows
-- both optimization flags and source mapping information to be preserved.
module Data.Array.Accelerate.Annotations
    ( Ann(..)
    , Optimizations(..)
    , HasAnnotations(..)
    , alwaysInline
    , unRollIters
    , withOptimizations
    , mkAnn
    , mkDummyAnn
    , withEmptyCallStack
    , withEmptyOrFrozenCallStack
    , withExecutionStackAsCallStack
      -- Re-exported for convenience
    , HasCallStack
    , withFrozenCallStack
      -- Internals
    , rnfAnn
    , liftAnn
    ) where

import           Data.Array.Accelerate.Orphans  ( )

import           Control.DeepSeq                ( rnf )
import           Control.Exception              ( assert )
import qualified Data.HashSet                  as S
import           Data.Maybe                     ( mapMaybe )
import qualified GHC.ExecutionStack            as ES
import           GHC.IO                         ( unsafePerformIO )
import           GHC.Stack
import           GHC.Stack.Types                ( CallStack(FreezeCallStack) )
import           Language.Haskell.TH            ( Q
                                                , TExp
                                                )


-- | This annotation type would store source information if available and any
-- additional annotation types added by the programmer.
--
-- TODO: The set of optimizations is now the same for 'Exp' and 'Acc'. Should we
--       have separate sets of 'Optimizations' flags? Ideally we would only
--       allow adding optimization flags for constructs that make sense (e.g.
--       allow unrolling @awhile@, but not @acond@), but since this would
--       involve adding another type index to 'Exp' that's not going to be a
--       feasible approach.
-- TODO: We store plain 'SrcLoc's now. Is there a situation where we might want
--       to store the entire call stack? For instance, for use in error messages
--       and assertion failures.
data Ann = Ann
    { locations     :: S.HashSet SrcLoc
    , optimizations :: Optimizations
    }
    deriving Show

-- | Some example annotations. These do not actually do anything yet. Having
-- these as a record makes it possible to easily pattern match on them without
-- having to do list or set lookups everywhere. Because of record wild cards we
-- can still easily add additional annotations without having to modify all uses
-- of this type.
data Optimizations = Optimizations
    { optAlwaysInline :: Bool
    , optUnrollIters  :: Maybe Int
    }
    deriving Show


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
unRollIters :: HasAnnotations a => Int -> a -> a
unRollIters n = withOptimizations $ \opts -> opts { optUnrollIters = Just n }


-- * Internal types and functions

-- | Used for modifying an AST node's annotations.
--
-- TODO: We could define 'modifyAnn' in terms of 'getAnn' and a 'putAnn', but I
--       don't think a 'putAnn' on its own is really useful anywhere.
-- TODO: This require some duplication between 'getAnn' and 'modifyAnn'. A lens
--       for accessing the annotation of course would get rid of this, but then
--       you'd have to use lenses.
class HasAnnotations a where
  -- | Modify the annotation stored in an AST node. This may not do anything
  -- when the AST node doesn't support annotations.
  modifyAnn :: (Ann -> Ann) -> a -> a
  -- | Extract the annotation from an AST node, if it has one. This is used
  -- during some of the transformations when we may no longer have access to the
  -- original AST nodes.
  getAnn :: a -> Maybe Ann

withOptimizations :: HasAnnotations a => (Optimizations -> Optimizations) -> a -> a
withOptimizations f = modifyAnn $ \(Ann src opts) -> Ann src (f opts)

-- | Create an empty annotation with call site information if available. This
-- only works when all smart constructors have the 'HasCallStack' constraint.
-- This function __must__ be called with 'withFrozenCallStack'.
--
-- TODO: When Accelerate has a logger, this assertion should be replaced by a
--       warning. If the call stacks are not frozen, then we'll just treat it as
--       an empty call stack. When running the test suite this should still
--       count as a hard error though.
mkAnn :: HasCallStack => Ann
mkAnn = assert callStackIsFrozen
    $ Ann (callerLoc $ getCallStack callStack) defaultOptimizations
  where
    -- To prevent incorrect usage of this API, we assert that the call stacks
    -- are frozen before this function is called. In most simple use cases we
    -- could also have looked at the second entry in the call stack but that
    -- would be very error prone when recursion starts getting involved.
    callStackIsFrozen = case callStack of
        (FreezeCallStack _) -> True
        _                   -> False

    callerLoc ((_, loc) : _) = S.singleton loc
    callerLoc _              = S.empty

    defaultOptimizations =
        Optimizations { optAlwaysInline = False, optUnrollIters = Nothing }

-- | Create a new 'Ann' without any source information.
--
-- TODO: Ever everything has been implemented, check whether the uses of this
--       are justified and if we're not throwing away any existing annotations
--       when reconstructing ASTs
mkDummyAnn :: Ann
mkDummyAnn = withEmptyCallStack mkAnn

-- | Compute the argument (which can be either a term or a function) without
-- collecting call stacks. This can be useful when dealing with situations where
-- we might want to collect call stacks but can't, so we don't end up capturing
-- some unrelated call stacks instead.
withEmptyCallStack :: (HasCallStack => a) -> a
withEmptyCallStack dewit =
    let ?callStack = freezeCallStack emptyCallStack in dewit

-- | Workaround for pattern synonyms and call stacks not working as expected in
-- GHC versions 9.0.x and below. See the issue linked below. On this versions we
-- will freeze an empty call stack instead of the call stack wasn't already
-- frozen. This function is implemented in the same way as the regular
-- 'withFrozenCallStack'.
--
-- HACK: Call stacks didn't play nicely with pattern synonyms in GHC version
--       before 9.2, so to prevent incorrect source annotations we'll prevent
--       them from being generated completely.
--
--       https://gitlab.haskell.org/ghc/ghc/-/issues/19289
-- TODO: Since 'Pattern' isn't meant to be used directly, should we strip off
--       two layers of call stack?
withEmptyOrFrozenCallStack :: HasCallStack => (HasCallStack => a) -> a
withEmptyOrFrozenCallStack dewit =
  let ?callStack =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
        -- Same definition as in 'withFrozenCallStack'
        freezeCallStack (popCallStack callStack)
#else
        -- Only freeze an empty call stack of the call stack isn't already
        -- frozen, i.e. when it is used internally within Accelerate's front end
        -- standard library
        case ?callStack of
          x@(FreezeCallStack _) -> x
          _                     -> freezeCallStack emptyCallStack
#endif
  in  dewit

-- | Evaluate a computation after transforming the RTS execution stack into a
-- frozen GHC call stack so it can interact with our other call stack based
-- machinery. This is necessary when implementing prelude and other external
-- type classes since those will not contain the 'HasCallStack' constraint. If
-- an execution stack frame is not available, then the computation will be
-- evaluated with an empty call stack instead.
--
-- NOTE: Execution stacks __only__ works when GHC has been built with libdw:
--
--       $ ghc --info | grep libdw
--
-- TODO: Test whether this actually uses the correct stack frame
-- FIXME: This may still be incorrect for default implementations in prelude
--        classes, since the default implementation doesn't use
--        'withExecutionStackAsCallStack'
withExecutionStackAsCallStack :: HasCallStack => (HasCallStack => a) -> a
withExecutionStackAsCallStack dewit =
  -- Only create a frozen call stack if we do not already have a frozen call
  -- stack
    let
        ?callStack = case ?callStack of
            x@(FreezeCallStack _) -> x
            _ ->
                freezeCallStack . toCallStack $ unsafePerformIO ES.getStackTrace
    in  dewit
  where
    -- We don't want the two uppermost stack frames, since those will be in our
    -- own library code
    -- TODO: Is this correct? Should we drop only one stack frame?
    toCallStack :: Maybe [ES.Location] -> CallStack
    toCallStack (Just (_ : _ : locs)) =
        fromCallSiteList $ mapMaybe locToCallSite locs
    toCallStack _ = emptyCallStack

    locToCallSite :: ES.Location -> Maybe (String, SrcLoc)
    locToCallSite (ES.Location obj fn (Just loc)) = Just
        ( obj ++ ": " ++ fn
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
    [|| Ann $$(liftLocations src) $$(liftOptimizations opts) ||]

liftOptimizations :: Optimizations -> Q (TExp Optimizations)
liftOptimizations Optimizations { .. } = [|| Optimizations { .. } ||]

liftLocations :: S.HashSet SrcLoc -> Q (TExp (S.HashSet SrcLoc))
liftLocations locs = [|| S.fromList $$(liftLocs $ S.toList locs) ||]
  where
    -- TODO: Is there some combinator for this transformation?
    liftLocs :: [SrcLoc] -> Q (TExp [SrcLoc])
    liftLocs (x : xs) = [|| $$(liftSrcLoc x) : $$(liftLocs xs) ||]
    liftLocs []       = [|| [] ||]

liftSrcLoc :: SrcLoc -> Q (TExp SrcLoc)
liftSrcLoc SrcLoc { .. } = [|| SrcLoc { .. } ||]
