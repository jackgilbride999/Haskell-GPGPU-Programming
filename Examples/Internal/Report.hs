{-# LANGUAGE CPP #-}
-- |
-- Module:      : Examples.Internal.Report
-- Copyright    : [2014..2020] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Examples.Internal.Report (

  processReports,

) where

import Criterion.Types
import Examples.Internal.ParseArgs

#ifdef ACCELERATE_ENABLE_CODESPEED
import Examples.Internal.Codespeed
#endif


-- | Post-process the benchmark reports.
--
processReports :: Options -> [Report] -> IO ()
processReports _opt _reports = do
#ifdef ACCELERATE_ENABLE_CODESPEED
  uploadReports _opt _reports
#endif

  return ()

