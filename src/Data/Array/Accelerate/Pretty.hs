{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.Pretty
-- Copyright   : [2008..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Pretty (

  -- ** Pretty printing
  PrettyAcc, ExtractAcc,
  prettyPreOpenAcc,
  prettyPreOpenAfun,
  prettyOpenExp,
  prettyOpenFun,

  -- ** Graphviz
  Graph,
  PrettyGraph(..), Detail(..),
  graphDelayedAcc, graphDelayedAfun,

) where

import Data.Array.Accelerate.AST                                    hiding ( Acc, Exp )
import Data.Array.Accelerate.Debug.Internal.Flags
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Pretty.Graphviz
import Data.Array.Accelerate.Pretty.Print                           hiding ( Keyword(..) )
import Data.Array.Accelerate.Smart                                  ( Acc, Exp )
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Trafo.Delayed

import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Text.Lazy                                     as T
import qualified System.Console.ANSI                                as Term
import qualified System.Console.Terminal.Size                       as Term

#if ACCELERATE_DEBUG
import Control.DeepSeq
import Data.Array.Accelerate.Debug.Internal.Stats
#endif


instance Arrays arrs => Show (Acc arrs) where
  show = withSimplStats . show . convertAcc

instance Afunction (Acc a -> f) => Show (Acc a -> f) where
  show = withSimplStats . show . convertAfun

instance Elt e => Show (Exp e) where
  show = withSimplStats . show . convertExp

instance Function (Exp a -> f) => Show (Exp a -> f) where
  show = withSimplStats . show . convertFun

-- instance Typeable a => Show (Seq a) where
--   show = withSimplStats . show . convertSeq


-- Note: [Show instances]
--
-- Explicitly enumerate Show instances for the Accelerate array AST types.
-- If we instead use a generic instance of the form:
--
--   instance Kit acc => Show (acc aenv a) where
--
-- This matches any type of kind (* -> * -> *), which can cause problems
-- interacting with other packages. See Issue #108.
--

instance PrettyEnv aenv => Show (OpenAcc aenv a) where
  show = renderForTerminal . prettyOpenAcc configPlain context0 (prettyEnv (pretty 'a'))

instance PrettyEnv aenv => Show (OpenAfun aenv f) where
  show = renderForTerminal . prettyPreOpenAfun configPlain prettyOpenAcc (prettyEnv (pretty 'a'))

instance PrettyEnv aenv => Show (DelayedOpenAcc aenv a) where
  show = let config = if shouldPrintHash then configWithHash else configPlain
         in renderForTerminal . prettyDelayedOpenAcc config context0 (prettyEnv (pretty 'a'))

instance PrettyEnv aenv => Show (DelayedOpenAfun aenv f) where
  show = let config = if shouldPrintHash then configWithHash else configPlain
         in renderForTerminal . prettyPreOpenAfun config prettyDelayedOpenAcc (prettyEnv (pretty 'a'))

instance (PrettyEnv env, PrettyEnv aenv) => Show (OpenExp env aenv e) where
  show = renderForTerminal . prettyOpenExp context0 (prettyEnv (pretty 'x')) (prettyEnv (pretty 'a'))

instance (PrettyEnv env, PrettyEnv aenv) => Show (OpenFun env aenv e) where
  show = renderForTerminal . prettyOpenFun (prettyEnv (pretty 'x')) (prettyEnv (pretty 'a'))


-- Internals
-- ---------

renderForTerminal :: Adoc  -> String
renderForTerminal = render . layoutSmart terminalLayoutOptions
  where
    fancy = terminalSupportsANSI && terminalColourAllowed
    render
      | fancy     = T.unpack . renderLazy . reAnnotateS ansiKeyword
      | otherwise = renderString

{-# NOINLINE terminalColourAllowed #-}
terminalColourAllowed :: Bool
terminalColourAllowed = unsafePerformIO $ isNothing <$> lookupEnv "NO_COLOR"

{-# NOINLINE terminalSupportsANSI #-}
terminalSupportsANSI :: Bool
terminalSupportsANSI = unsafePerformIO $ Term.hSupportsANSI stdout

{-# NOINLINE terminalLayoutOptions #-}
terminalLayoutOptions :: LayoutOptions
terminalLayoutOptions
  = unsafePerformIO
  $ do term <- Term.size
       return $ case term of
                  Nothing -> defaultLayoutOptions
                  Just t  -> LayoutOptions { layoutPageWidth = AvailablePerLine (min w 120) f }
                    where
                      w = Term.width t
                      f | w <= 80   = 1
                        | w <= 100  = 0.9
                        | otherwise = 0.8

prettyOpenAcc :: PrettyAcc OpenAcc
prettyOpenAcc config context aenv (OpenAcc pacc) =
  prettyPreOpenAcc config context prettyOpenAcc extractOpenAcc aenv pacc

extractOpenAcc :: OpenAcc aenv a -> PreOpenAcc OpenAcc aenv a
extractOpenAcc (OpenAcc pacc) = pacc


prettyDelayedOpenAcc :: HasCallStack => PrettyAcc DelayedOpenAcc
prettyDelayedOpenAcc config context aenv (Manifest pacc)
  = prettyPreOpenAcc config context prettyDelayedOpenAcc extractDelayedOpenAcc aenv pacc
prettyDelayedOpenAcc _      _       aenv (Delayed _ _ sh f _)
  = parens
  $ nest shiftwidth
  $ sep [ delayed "delayed"
        ,          prettyOpenExp app Empty aenv sh
        , parens $ prettyOpenFun     Empty aenv f
        ]

extractDelayedOpenAcc :: HasCallStack => DelayedOpenAcc aenv a -> PreOpenAcc DelayedOpenAcc aenv a
extractDelayedOpenAcc (Manifest pacc) = pacc
extractDelayedOpenAcc Delayed{}       = internalError "expected manifest array"


-- Unfortunately, using unsafePerformIO here means that the getFlag will be
-- evaluated only once when the first 'show' is performed on a Delayed value;
-- afterwards, the thunk will have been evaluated, and all future pretty-print
-- outputs will use the same result.
-- This cannot be prevented using a NOINLINE pragma, since then the function
-- itself is still a thunk that will only be evaluated once.
--
-- The practical result of this is that @setFlag verbose@ will not change
-- anything after a Delayed has already been printed once.
shouldPrintHash :: Bool
shouldPrintHash = unsafePerformIO $ getFlag verbose


-- Debugging
-- ---------

-- Attach simplifier statistics to the tail of the given string. Since the
-- statistics rely on fully evaluating the expression this is difficult to do
-- generally (without an additional deepseq), but easy enough for our show
-- instances.
--
-- For now, we just reset the statistics at the beginning of a conversion, and
-- leave it to a backend to choose an appropriate moment to dump the summary.
--
withSimplStats :: String -> String
#ifdef ACCELERATE_DEBUG
withSimplStats x = unsafePerformIO $ do
  when dump_simpl_stats $ x `deepseq` dumpSimplStats
  return x
#else
withSimplStats x = x
#endif

