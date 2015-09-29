module NoLimits.Types (
  -- * Options
  SetupOpts(..),
  BuildOpts(..),
  -- * Re-exports
  module NoLimits.Types.PackageConfig
  ) where

import NoLimits.Types.PackageConfig
import Path

data SetupOpts = SetupOpts
  { soPath :: Path Abs Dir
  }
  deriving (Show)

data BuildOpts = BuildOpts
  { boPath :: Path Abs Dir
  }
  deriving (Show)
