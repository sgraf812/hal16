module Paths
  ( srcPath
  ) where

import Paths_lambdamon_types
import GHC.IO (unsafePerformIO)
import System.FilePath (takeDirectory)

srcPath :: FilePath
srcPath = takeDirectory (unsafePerformIO (getDataFileName "src/Types.hs"))

{-# NOINLINE srcPath #-}
