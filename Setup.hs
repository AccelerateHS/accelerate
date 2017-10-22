{-# LANGUAGE CPP #-}

module Main where

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)
import Distribution.Extra.Doctest

main :: IO ()
main = defaultMainWithDoctests "doctest"

#else
import Distribution.Simple

main :: IO ()
main = defaultMain
#endif

