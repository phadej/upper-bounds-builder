{-# LANGUAGE TemplateHaskell #-}
module NoLimits.Paths where

import Control.Monad.Catch
import Control.Monad.Reader
import Distribution.Package
import Distribution.Extra
import Path

class HasSourceDir env where
  getSourceDir :: env -> Path Abs Dir

class HasBuildRootDir env where
  getBuildRootDir :: env -> Path Abs Dir

getLogDir :: HasBuildRootDir env => env -> Path Abs Dir
getLogDir env = getBuildRootDir env </> $(mkRelDir "log")

askLogDir :: (MonadReader env m, HasBuildRootDir env) => m (Path Abs Dir)
askLogDir = reader getLogDir

-- TODO: use package identifier?
askPackageLogDir :: (MonadReader env m, HasBuildRootDir env, MonadThrow m) => PackageIdentifier -> m (Path Abs Dir)
askPackageLogDir pi = do
  logDir <- askLogDir
  packageDir <- parseRelDir (packageIdentifierString pi)
  return (logDir </> packageDir)
