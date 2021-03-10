{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
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
-- TODO: Insert @withFrozenCallStack@ at the right location in the view pattern
--       of the pattern synonyms generated using @mkPattern@. This should use an
--       empty frozen call stack with GHC 9.0.x and below instead.
--
-- ** Annotations in the de Bruijn AST
--
-- TODO: Add the same annotations as in the Smart ASTs, and make sure that they
--       are propagated properly through the transformations.
--
-- TODO: The rest of the process up until codegen
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
    , mkAnn
    , mkDummyAnn
    , withEmptyCallStack
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
import           GHC.Stack
import           GHC.Stack.Types                ( CallStack(FreezeCallStack) )
import           Language.Haskell.TH            ( Q
                                                , TExp
                                                )


-- * Internal types and functions

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

-- | Create an empty annotation with call site information if available. This
-- only works when all smart constructors have the 'HasCallStack' constraint.
-- This function __must__ be called with 'withFrozenCallStack'.
--
-- XXX: Should there be some convenience wrapper for @withFrozenCallStack
--      mkAnn@? That could get rid of some noise, but it also sort of defeats
--      the purpose of asserting that the call stack is frozen.
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
