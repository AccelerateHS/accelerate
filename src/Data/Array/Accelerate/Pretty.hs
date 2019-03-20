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
-- Copyright   : [2008..2019] The Accelerate Team
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
  prettyPreOpenExp,
  prettyPreOpenFun,

  -- ** Graphviz
  Graph,
  PrettyGraph(..), Detail(..),
  graphDelayedAcc, graphDelayedAfun,

) where

-- libraries
import Control.DeepSeq
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.IO
import System.IO.Unsafe
import qualified Data.Text.Lazy                                     as T
import qualified System.Console.ANSI                                as Term
import qualified System.Console.Terminal.Size                       as Term

-- friends
import Data.Array.Accelerate.Smart                                  ( Acc, Exp )
import Data.Array.Accelerate.AST                                    hiding ( Acc, Exp, Val(..) )
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Error
import Data.Array.Accelerate.Pretty.Print                           hiding ( Keyword(..) )
import Data.Array.Accelerate.Trafo
import Data.Array.Accelerate.Pretty.Graphviz
import Data.Array.Accelerate.Debug.Flags
import Data.Array.Accelerate.Debug.Stats



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
  show = renderForTerminal . prettyOpenAcc context0 (prettyEnv (pretty 'a'))

instance PrettyEnv aenv => Show (OpenAfun aenv f) where
  show = renderForTerminal . prettyPreOpenAfun prettyOpenAcc (prettyEnv (pretty 'a'))

instance PrettyEnv aenv => Show (DelayedOpenAcc aenv a) where
  show = renderForTerminal . prettyDelayedOpenAcc context0 (prettyEnv (pretty 'a'))

instance PrettyEnv aenv => Show (DelayedOpenAfun aenv f) where
  show = renderForTerminal . prettyPreOpenAfun prettyDelayedOpenAcc (prettyEnv (pretty 'a'))

instance (PrettyEnv env, PrettyEnv aenv) => Show (PreOpenExp OpenAcc env aenv e) where
  show = renderForTerminal . prettyPreOpenExp context0 prettyOpenAcc extractOpenAcc (prettyEnv (pretty 'x')) (prettyEnv (pretty 'a'))

instance (PrettyEnv env, PrettyEnv aenv) => Show (PreOpenExp DelayedOpenAcc env aenv e) where
  show = renderForTerminal . prettyPreOpenExp context0 prettyDelayedOpenAcc extractDelayedOpenAcc (prettyEnv (pretty 'x')) (prettyEnv (pretty 'a'))

instance (PrettyEnv env, PrettyEnv aenv) => Show (PreOpenFun OpenAcc env aenv e) where
  show = renderForTerminal . prettyPreOpenFun prettyOpenAcc extractOpenAcc (prettyEnv (pretty 'x')) (prettyEnv (pretty 'a'))

instance (PrettyEnv env, PrettyEnv aenv) => Show (PreOpenFun DelayedOpenAcc env aenv e) where
  show = renderForTerminal . prettyPreOpenFun prettyDelayedOpenAcc extractDelayedOpenAcc (prettyEnv (pretty 'x')) (prettyEnv (pretty 'a'))


-- Internals
-- ---------

renderForTerminal :: Adoc  -> String
renderForTerminal = render . layoutSmart terminalLayoutOptions
  where
    render | terminalSupportsANSI = T.unpack . renderLazy . reAnnotateS ansiKeyword
           | otherwise            = renderString

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
                        | w <= 100  = 0.8
                        | otherwise = 0.6

prettyOpenAcc :: PrettyAcc OpenAcc
prettyOpenAcc context aenv (OpenAcc pacc) =
  prettyPreOpenAcc context prettyOpenAcc extractOpenAcc aenv pacc

extractOpenAcc :: OpenAcc aenv a -> PreOpenAcc OpenAcc aenv a
extractOpenAcc (OpenAcc pacc) = pacc


prettyDelayedOpenAcc :: PrettyAcc DelayedOpenAcc
prettyDelayedOpenAcc context aenv (Manifest pacc)
  = prettyPreOpenAcc context prettyDelayedOpenAcc extractDelayedOpenAcc aenv pacc
prettyDelayedOpenAcc context aenv (Delayed sh f _)
  | Shape a   <- sh
  , Just Refl <- match f (Lam (Body (Index a (Var ZeroIdx))))
  = prettyDelayedOpenAcc context aenv a
  --
  -- If we detect that the delayed array is simply accessing an array
  -- variable, then just print the variable name. That is:
  --
  -- > let a0 = <...> in map f (Delayed (shape a0) (\x0 -> a0!x0))
  --
  -- becomes
  --
  -- > let a0 = <...> in map f a0
  --
  | otherwise
  = parens
  $ nest shiftwidth
  $ sep [ delayed "delayed"
        ,          prettyPreOpenExp app prettyDelayedOpenAcc extractDelayedOpenAcc Empty aenv sh
        , parens $ prettyPreOpenFun     prettyDelayedOpenAcc extractDelayedOpenAcc Empty aenv f
        ]

extractDelayedOpenAcc :: DelayedOpenAcc aenv a -> PreOpenAcc DelayedOpenAcc aenv a
extractDelayedOpenAcc (Manifest pacc) = pacc
extractDelayedOpenAcc Delayed{}       = $internalError "extractDelayedOpenAcc" "expected manifest array"


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

