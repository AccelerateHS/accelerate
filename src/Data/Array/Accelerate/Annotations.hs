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
-- TODO: Add the same file header used in all other modules
-- TODO: Reformat all of the changes from this branch to the usual Accelerate
--       style. There's no style guide or formatter config anywhere, so I just
--       run most things through a formatter to keep me sane.
-- TODO: Take another look at fusion, optimization propagation doesn't have to
--       be perfect yet but I'm sure there are also issues elsewhere
-- TODO: Annotations are completely ignored in 'Match' and 'Hash' at the moment.
--       We should probably at least consider the optimizations of not the
--       entire annotation.
-- TODO: See if we can clean up the pretty printer a bit, and also add the
--       information to the graphviz export (there's already a todo for that)
-- TODO: Expose the pretty printer verbosity option somehow
-- TODO: Tests! Tests? Can we test this, and how? We can probably at least fake
--       call stacks, generate random ASTs with smart constructors, and check
--       `getAnn`.
-- XXX: Right now it would be possible to specify some nonsensible flags, like
--      setting loop unrolling for a constant value. Should we just silently
--      ignore these things like we do now, or should be printing warnings? I
--      don't think Accelerate has any other non-fatal compiler diagnostics.
--
-- There are a bunch more todos sprinkled around the code. Use the following
-- mystical one-liner to find them:
--
-- @
-- git diff -U0 master...feature/annotations | grep '^+.*\(TODO\|HACK\|FIXME\)' | cut -c2- | git grep -nFf- feature/annotations
-- @
--
-- __Annotations__
--
-- The general idea is that almost all AST data type constructors have been
-- extended with an 'Ann' field, storing annotation data for the AST node. At
-- the moment we use this to store both source mapping information and flags
-- that can be used by the optimizer. These annotations are automatically
-- populated in the smart constructors using the 'mkAnn' function, so they end
-- up being completely transparent to the user.
--
-- /Source mapping/
--
-- 'mkAnn' will try to capture source information in the form of GHC Call
-- Stacks. This ends up being a breaking change to any library building on
-- Accelerate. In short, the changes that need to be made are as follows:
--
--   1. All top-level smart constructors or library functions that are exposed
--      directly to the user need to be annotated with the 'HasCallStack'
--      constraint. That way those functions can know where the function was
--      called from.
--   2. These top-level functions then need to enable source mapping by
--      evaluating their contents using either the 'sourceMap',
--      'sourceMapRuntime' or 'sourceMapPattern' functions. Calling 'sourceMap'
--      or 'sourceMapPattern' when the function does not have the 'HasCallStack'
--      constraint will result in a run-time error. See below for more
--      information on how to use these functions correctly and which one to
--      use.
--   3. Any auxiliary function that directly or indirectly calls 'mkAnn' needs
--      to be annotated with the 'SourceMapped' constraint. This is enforced by
--      the type checker, so you will get a GHC compiler error when you forget
--      to do this. 'SourceMapped' functions can only be evaluated through one
--      of the 'sourceMap' functions mentioned above.
--   4. When translating between different AST types (such as the internal De
--      Bruijn AST and a backend-specific AST), the annotations need to be
--      propgated to that new AST nodes. If needed, multiple annotations can be
--      merged using the '(<>)' operator.
--
-- These steps are necessary to be able to access the source location in the
-- user's code where the top-level library function was called from. There is
-- some degree of safety built into this mechanism, but there are still some
-- ways to do it wrong that you should be aware of. The 'SourceMapped'
-- constraint and the accompanying 'sourceMap' function enforce that every
-- function in the chain from a smart constructor a call to 'mkAnn' contains the
-- correct call stack pointing to the caller's source code. However, this
-- mechanism cannot protect you from mistakes when calling top-level smart
-- constructors or library functions directly from other top-level smart
-- constructors or library functions. In that case, forgetting either step (2)
-- or both steps (1) and (2) will not print any errors, but will instead cause
-- the source mapping annotation to contain the wrong source location.
--
-- /'sourceMap' and friends/
--
-- The idea is that top-level functions are annotated with 'HasCallStack' so the
-- call stack contains information about where those functions were called from.
-- 'sourceMap' then freezes this call stack information, and allows
-- 'SourceMapped' functions to access it. This system uses /implicit
-- parameters/. That means that the location in the source code where a term is
-- defined determines whether it inherits this 'SourceMapped' context. For
-- auxiliary functions, the only change that needs to be made is to add the
-- 'SourceMapped' constraint, but top-level functions may need to be
-- restructured a bit:
--
--   - The entire function body needs to be wrapped in either 'sourceMap',
--     'sourceMapRuntime', or 'sourceMapPattern'. 'sourceMap' should always be
--     used unless it is not possible to add the 'HasCallStack' constraint (such
--     as when implementing third party type classes), or when manually creating
--     bidirectional pattern synonyms. In those cases 'sourceMapRuntime' and
--     'sourceMapPattern' should be used instead. To evaluate the function's
--     body through these functions these functions, you just need to make sure
--     the function definition starts with @sourceMap $@, or just 'sourceMap' if
--     the function previously only consisted of a single term.
--   - Any /terms/ defined in a where-clause need to be moved to a let-binding
--     that is evaluated under 'sourceMap'. Otherwise they will not inherit the
--     source mapping information.
--   - Any /functions/ defined in the where-clause either need to be explicitly
--     annotated with the 'SourceMapped' constraint, or they can also be moved
--     to a let-binding.
--   - If the top-level constructor uses a pattern synonym to deconstruct one of
--     its arguments, then this needs to be moved to a lambda inside of
--     'sourceMap'. This is needed because these pattern synonyms use a view
--     pattern function behind the scenes, and this function would otherwise be
--     evaluated outside of the 'SourceMapped' context.
--
-- As an example, the following functions:
--
-- > magnitude :: RealFloat a => Exp (Complex a) -> Exp a
-- > magnitude (r ::+ i) = scaleFloat k (sqrt (sqr (scaleFloat mk r) + sqr (scaleFloat mk i)))
-- >   where
-- >     k     = max (exponent r) (exponent i)
-- >     mk    = -k
-- >     sqr z = z * z
-- >
-- > boolToInt :: Exp Bool -> Exp Int
-- > boolToInt = mkFromIntegral . mkCoerce @_ @Word8
-- >
-- > mkFromIntegral :: (Elt a, Elt b, IsIntegral (EltR a), IsNum (EltR b)) => Exp a -> Exp b
-- > mkFromIntegral = mkPrimUnary $ PrimFromIntegral integralType numType
-- >
-- > -- ...and the rest of 'mkPrimUnary', 'mkCoerce', etc.
--
-- Would have to be rewritten as follows:
--
-- > magnitude :: (HasCallStack, RealFloat a) => Exp (Complex a) -> Exp a
-- > magnitude = sourceMap $ \(r ::+ i) ->
-- >     let k  = max (exponent r) (exponent i)
-- >         mk = -k
-- >         sqr z = z * z
-- >     in  scaleFloat k (sqrt (sqr (scaleFloat mk r) + sqr (scaleFloat mk i)))
-- >
-- > boolToInt :: HasCallStack => Exp Bool -> Exp Int
-- > boolToInt = sourceMap $ mkFromIntegral . mkCoerce @_ @Word8
-- >
-- > mkFromIntegral :: (SourceMapped, Elt a, Elt b, IsIntegral (EltR a), IsNum (EltR b)) => Exp a -> Exp b
-- > mkFromIntegral = mkPrimUnary $ PrimFromIntegral integralType numType
--
module Data.Array.Accelerate.Annotations (
  -- * Annotations
  Ann (..),
  Optimizations (..),
  HasAnnotations (..),
  TraverseAnnotations (..),
  context,
  alwaysInline,
  unrollIters,

  -- * Source mapping
  SourceMapped,
  sourceMap,
  sourceMapRuntime,
  sourceMapPattern,
  mergeLocs,
  mergeLocsSingle,

  -- * Internals
  FieldAnn (..),
  withOptimizations,
  extractAnn,
  mkAnn,
  mkDummyAnn,
  rnfAnn,
  liftAnn,

  -- * Re-exported for convenience
  HasCallStack,
) where

import Data.Array.Accelerate.Orphans ()

import Control.DeepSeq (rnf)
import qualified Data.HashSet as S
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Stack
import GHC.Stack.Types (CallStack (..))
import Language.Haskell.TH.Extra (CodeQ)
import Lens.Micro
import Lens.Micro.Extras (view)

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
  { -- | When displaying these to the user, use 'mergeLocs' instead of
    -- 'fromCallSiteList' so nearby source locations are merged.
    locations :: S.HashSet CallStack
  , optimizations :: Optimizations
  }

-- | Some example annotations. These do not actually do anything yet. Having
-- these as a record makes it possible to easily pattern match on them without
-- having to do list or set lookups everywhere. Because of record wild cards we
-- can still easily add additional annotations without having to modify all uses
-- of this type.
--
-- TODO: After the source mapping is done, we should add the rest of the
--       optimizations here and actually make them do something. Currently
--       they're just placeholders.
data Optimizations = Optimizations
  { optAlwaysInline :: Bool
  , optUnrollIters :: Maybe Int
  }


-- * Annotation functions
--
-- These are exposed to the user so they can annotate AST nodes.

-- | Add context to a scalar expression or an array operation. This will insert
-- the current call stack into the expression and all of its subtrees along with
-- the provided context string.
context :: (HasCallStack, TraverseAnnotations a) => String -> a -> a
context label = traverseAnns (\(Ann src opts) -> Ann (S.insert (modifyStack callStack) src) opts)
  where
    -- Because we're using hashsets, we actually don't have to worry about
    -- duplicates showing up after the simplification and fusion transformations
    modifyStack (getCallStack -> ((_, loc) : rest)) = fromCallSiteList ((label, loc) : rest)
    modifyStack stack                               = stack

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
-- Nested 'sourceMap' calls will keep the source mapping information from the
-- outermost call in tact.
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
      ?callStack = freezeCallStack (popCallStack callStack)
   in if isEmptyStack ?callStack
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
sourceMapRuntime :: HasCallStack => (SourceMapped => a) -> a
sourceMapRuntime dewit =
  let ?requiresSourceMapping = TakenCareOf
      -- Only create a frozen call stack if we do not already have a valid
      -- frozen call stack
      ?callStack =
        case ?callStack of
          x@(FreezeCallStack _) -> x
          _ -> freezeCallStack emptyCallStack
          -- FIXME: This will need some more work. The main issues are that the
          --        stack contains a lot of frames at the top and the bottom
          --        that would need to be stripped, and that tail call
          --        optimization interferes with these execution stacks.
          -- _ -> freezeCallStack . toCallStack $ unsafePerformIO ES.getStackTrace
   in dewit
 --  where
 --    -- We don't want the two uppermost stack frames, since those will be in our
 --    -- own library code
 --    -- TODO: Is this correct? Should we drop only one stack frame?
 --    toCallStack :: Maybe [ES.Location] -> CallStack
 --    toCallStack (Just (_ : _ : locs)) = fromCallSiteList $ mapMaybe locToCallSite locs
 --    toCallStack _ = emptyCallStack

 --    locToCallSite :: ES.Location -> Maybe (String, SrcLoc)
 --    locToCallSite (ES.Location _ fn (Just loc)) =
 --      Just
 --        ( fn
 --        , SrcLoc
 --            { srcLocPackage = ""
 --            , srcLocModule = ""
 --            , srcLocFile = ES.sourceFile loc
 --            , srcLocStartLine = ES.sourceLine loc
 --            , srcLocStartCol = ES.sourceColumn loc
 --            , srcLocEndLine = ES.sourceLine loc
 --            , srcLocEndCol = ES.sourceColumn loc
 --            }
 --        )
 --    locToCallSite (ES.Location _ _ Nothing) = Nothing

-- | Workaround for pattern synonyms and call stacks not working as expected in
-- GHC versions 9.0.x and below. Performs the same duty as 'sourceMap'. On the
-- unsupported GHC versions, this will freeze an empty call stack if the current
-- call stack isn't already frozen. Otherwise we would capture the wrong call
-- stacks.
--
-- /NOTE:/
--
-- The pattern and the expression parts of a bidirectional pattern synonym both
-- count as a function call. If a pattern synonym is meant to be used directly,
-- then the @nestingDepth@ parameter should be set to 0. If the pattern synonym
-- is meant to be aliased using a simply-bidirectional pattern synonym (e.g. the
-- @Pattern@ and @Vector@ pattern synonyms), then the nesting depth should be
-- set to 1.
--
-- /HACK:/
--
-- Call stacks didn't play nicely with pattern synonyms in GHC version before
-- 9.2, so to prevent incorrect source annotations we'll prevent them from being
-- generated completely.
--
-- https://gitlab.haskell.org/ghc/ghc/-/issues/19289
sourceMapPattern :: HasCallStack => Int -> (SourceMapped => a) -> a
sourceMapPattern _nestingDepth dewit =
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
  let ?requiresSourceMapping = TakenCareOf
      ?callStack = freezeCallStack (iterate popCallStack callStack !! (_nestingDepth + 1))
   in if isEmptyStack ?callStack
        then printError
        else dewit
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
      -- Only freeze an empty call stack of the call stack isn't already
      -- frozen, i.e. when it is used internally within Accelerate's front end
      -- standard library
      ?callStack = case ?callStack of
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
class FieldAnn s where
  -- | A lens for accessing @a@'s annotation field, if it has one. By defining
  -- this as a lens we can get rid of some duplication.
  _ann :: Lens' s (Maybe Ann)

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

-- | AST types that allow modifying every annotation stored in their subtrees.
-- Because this can only be implemented for concrete types, this is separate
-- from 'HasAnnotations'.
class TraverseAnnotations a where
  -- | A traversal over all annotations in @a@ and all of its subtrees that
  -- modifies all of those annotations. This is essentially a limited, modify
  -- only version of 'Traversal' a Ann' because it's not possible to traverse
  -- functions (in the higher order smart AST) with lenses.
  --
  -- Even though this is not a traversal as the term is normally used in
  -- Haskell, naming this 'modifyAnns' would make it easy to accidentally use
  -- the wrong function.
  traverseAnns :: (Ann -> Ann) -> a -> a

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

instance TraverseAnnotations r => TraverseAnnotations (a -> r) where
  traverseAnns f f' x = traverseAnns f (f' x)

-- | Change the optimization flags for an AST node.
withOptimizations :: HasAnnotations a => (Optimizations -> Optimizations) -> a -> a
withOptimizations f = modifyAnn $ \(Ann src opts) -> Ann src (f opts)

-- | A helper to extract an annotation from an expression, or to return an empty
-- annotation if the expression doesn't contain one.
extractAnn :: HasAnnotations a => a -> Ann
extractAnn (getAnn -> Just ann) = ann
extractAnn _                    = mkDummyAnn

-- | Create a new annotation, capturing any available source mapping information
-- from the current 'SourceMapped' context. Check the module's documentation for
-- more information.
  --
--- XXX: Inlining on -O1 and higher causes the result of this function to be
---      shared even if any of the 'SourceMapped' implicit parameters are
---      different, at least on GHC 8.10.4. This would result in every
---      invocation of this function returning the same (incorrect)) value.
{-# NOINLINE mkAnn #-}
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
mkDummyAnn =
  let ?requiresSourceMapping = TakenCareOf
      ?callStack = freezeCallStack emptyCallStack
   in mkAnn

-- | Merge adjacent source locations stored in an annotation, returning a list
-- of source locations ordered by filename, line number, and column. This is a
-- list of call stacks in the same format as `getCallStack`. When the source
-- location set is disjoint, the resulting list will contain multiple entries.
--
-- TODO: Right now we use a simple heuristic and consider regions on the same
--       line or on adjacent lines to be adjacent. This will definitely need
--       some tweaking.
mergeLocs :: S.HashSet CallStack -> [[(String, SrcLoc)]]
mergeLocs =
  mergeAdjacent
    . sortBy cmpLoc
    . S.foldl' (\acc (getCallStack -> stack) -> stack : acc) []
  where
    -- We need the locations sorted by the file they're in, and their place in
    -- that file so we can then merge adjacent regions
    cmpLoc :: [(String, SrcLoc)] -> [(String, SrcLoc)] -> Ordering
    cmpLoc ((_, locA) : _) ((_, locB) : _) =
      (comparing srcLocFile <> comparing srcLocStartLine <> comparing srcLocStartCol) locA locB
    -- These call stacks should never be empty, but the exhaustiveness checker
    -- obviously doesn't know that
    cmpLoc ((_, _) : _)    []              = LT
    cmpLoc []              ((_, _) : _)    = GT
    cmpLoc []              []              = EQ

    -- TODO: We only look at and modify the topmost stack frame. This won't
    --       cause weird inconsistencies, righty?
    mergeAdjacent :: [[(String, SrcLoc)]] -> [[(String, SrcLoc)]]
    mergeAdjacent (x@((fnX, locX) : restX) : y@((fnY, locY) : _) : cs)
      | srcLocFile locX == srcLocFile locY
      -- Since the list is already sorted, we know that if region X's end is
      -- after region Y's start, then the regions overlap. We'll also allow
      -- consider a region Y that starts on the line after region X to be
      -- adjacent.
      , srcLocEndLine locX - srcLocStartLine locY >= -1
        -- TODO: Merging these function names this way can grow out of hand very
        --       quickly. On the other hand, just taking @fnX@ and not doing
        --       anything else kind of hides the fact that regions have been
        --       merged.
      = ((fnX <> ", " <> fnY, locX { srcLocEndLine = srcLocEndLine locY
                                   , srcLocEndCol  = srcLocEndCol locY }) : restX)
                      : mergeAdjacent cs
      | otherwise = x : mergeAdjacent (y:cs)
    mergeAdjacent cs = cs

-- | Merge a set of source locations as described in 'mergeLocs', and return the
-- first entry along with all top level function names in the merged call stack
-- set separated by commas. During fusion the names of fused functions are
-- already merged in a similar way, so this should give a good indication of
-- where this source location came from. This is used when formatting source
-- locations for the debug information in the backends.
mergeLocsSingle :: S.HashSet CallStack -> Maybe (SrcLoc, String)
mergeLocsSingle locs
  | (((firstNm, firstLoc) : _) : rest) <- mergeLocs locs
  = Just (firstLoc, firstNm <> mconcat [", " <> nm | ((nm, _) : _) <- rest])
  | otherwise
  = Nothing


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

liftAnn :: Ann -> CodeQ Ann
liftAnn (Ann src opts) =
  [|| Ann $$(liftCallStacks src) $$(liftOptimizations opts) ||]

liftOptimizations :: Optimizations -> CodeQ Optimizations
liftOptimizations Optimizations { .. } = [|| Optimizations { .. } ||]

liftCallStacks :: S.HashSet CallStack -> CodeQ (S.HashSet CallStack)
liftCallStacks stacks = [|| S.fromList $$(liftStacks $ S.toList stacks) ||]
  where
    -- TODO: Is there some combinator for this transformation?
    liftStacks :: [CallStack] -> CodeQ [CallStack]
    liftStacks (x : xs) = [|| $$(liftCallStack x) : $$(liftStacks xs) ||]
    liftStacks []       = [|| [] ||]

liftCallStack :: CallStack -> CodeQ CallStack
liftCallStack EmptyCallStack = [|| EmptyCallStack ||]
liftCallStack (PushCallStack fn loc stack) =
    [|| PushCallStack fn $$(liftSrcLoc loc) $$(liftCallStack stack) ||]
liftCallStack (FreezeCallStack stack) =
    [|| FreezeCallStack $$(liftCallStack stack) ||]

liftSrcLoc :: SrcLoc -> CodeQ SrcLoc
liftSrcLoc SrcLoc {..} = [|| SrcLoc { .. } ||]
