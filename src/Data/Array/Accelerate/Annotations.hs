{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}

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
-- XXX: Right now it would be possible to specify some nonsensible flags, like
--      setting loop unrolling for a constant value. Should we just silently
--      ignore these things like we do now, or should be printing warnings? I
--      don't think Accelerate has any other non-fatal compiler diagnostics.
--
-- TODO: Call stacks should be frozen for any function exposed to the user,
--       right now this is not yet the case.
module Data.Array.Accelerate.Annotations
    ( Ann(..)
    , Optimizations(..)
    , mkAnn
    , mkDummyAnn
    , withEmptyCallStack
      -- Re-exported for convenience
    , HasCallStack
    , withFrozenCallStack
    ) where

import           Control.Exception              ( assert )
import           GHC.Stack
import           GHC.Stack.Types                ( CallStack(FreezeCallStack) )


-- * Internal types and functions

-- | This annotation type would store source information if available and any
-- additional annotation types added by the programmer sdderiv.
--
-- TODO: We might need to replace location with @Set SrcLoc@ as we'll likely need
--       to combine multiple smart AST nodes in later steps of the compilation
--       process. With that setup we could even merge adjacent 'SrcLoc's.
-- TODO: The set of optimizations is now the same for 'Exp' and 'Acc'. Should we
--       have separate sets of 'Optimizations' flags? Ideally we would only
--       allow adding optimization flags for constructs that make sense (e.g.
--       allow unrolling @awhile@, but not @acond@), but since this would
--       involve adding another type index to 'Exp' that's not going to be a
--       feasible approach.
data Ann = Ann
    { location      :: Maybe SrcLoc
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

    callerLoc ((_, loc) : _) = Just loc
    callerLoc _              = Nothing

    defaultOptimizations =
        Optimizations { optAlwaysInline = False, optUnrollIters = Nothing }

-- | Create a new 'Ann' without any source information.
--
-- TODO: This should not be necessary anymore once we add the remaining
--       annotation fields
mkDummyAnn :: Ann
mkDummyAnn = withEmptyCallStack mkAnn

-- | Compute the argument (which can be either a term or a function) without
-- collecting call stacks. This can be useful when dealing with situations where
-- we might want to collect call stacks but can't, so we don't end up capturing
-- some unrelated call stacks instead.
withEmptyCallStack :: (HasCallStack => a) -> a
withEmptyCallStack dewit =
    let ?callStack = freezeCallStack emptyCallStack in dewit
