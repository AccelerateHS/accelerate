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
-- TODO: Instead of deriving show, we should modify the pretty printer to show
--       these annotations in a nice way. Should we also present source mapping
--       information? That would be very useful but it also adds a lot of noise,
--       is there some 'verbose' pretty printing mode?
-- TODO: Tests!
--
module Data.Array.Accelerate.Annotations where

-- TODO: Add an explicit export list

import           Control.Exception              ( assert )
import           GHC.Stack
import           GHC.Stack.Types                ( CallStack(FreezeCallStack) )


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
