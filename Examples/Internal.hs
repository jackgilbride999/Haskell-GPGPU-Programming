{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module:      : Data.Array.Accelerate.Examples.Internal
-- Copyright    : [2014..2020] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--
-- This module exposes some internal infrastructure that is useful and common to
-- the accelerate-examples test and benchmark program suite. This is not
-- intended for general consumption.
--

module Examples.Internal (

  module Examples.Internal.ParseArgs,
  module Examples.Internal.Criterion,
  module Examples.Internal.Interactive,
  module Examples.Internal.TestFramework,
  module Examples.Internal.Monitoring,
  module Examples.Internal.Util,

) where

import Examples.Internal.Criterion
import Examples.Internal.Interactive
import Examples.Internal.Monitoring
import Examples.Internal.ParseArgs
import Examples.Internal.TestFramework
import Examples.Internal.Util

