{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal.TestFramework.Config
-- Copyright    : [2014..2020] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Examples.Internal.TestFramework.Config (

  -- ** Test framework options
  module Examples.Internal.TestFramework.Config

) where

import Data.Monoid
import Data.Label
import Data.Label.Derive
import Test.Framework                           ( RunnerOptions, RunnerOptions', TestOptions', optionsDescription, SuppliedRunnerOptions )
import System.Console.GetOpt
import Prelude

import Examples.Internal.Backend

$(mkLabelsNamed id [''RunnerOptions'])
$(mkLabelsNamed id [''TestOptions'])

type Config = RunnerOptions

defaultConfig :: Backend -> Config
defaultConfig b
  = set ropt_threads (concurrentBackends b)
  $ set ropt_test_options
    ( Just $ set topt_maximum_generated_tests            (Just 1000)
           $ set topt_maximum_unsuitable_generated_tests (Just 1000)
           $ mempty)
  $ mempty

defaultOptions :: [OptDescr SuppliedRunnerOptions]
defaultOptions = optionsDescription

