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

askSourceDir :: (MonadReader env m, HasSourceDir env) => m (Path Abs Dir)
askSourceDir = reader getSourceDir

askInstallDir :: (MonadReader env m, HasBuildRootDir env) => m (Path Abs Dir)
askInstallDir = do
  buildRootDir <- reader getBuildRootDir
  return $ buildRootDir </> $(mkRelDir "install")

askPackageDbDir :: (MonadReader env m, HasBuildRootDir env) => m (Path Abs Dir)
askPackageDbDir = do
  buildRootDir <- reader getBuildRootDir
  return $ buildRootDir </> $(mkRelDir "pkgdb") 

askPackageLogDir :: (MonadReader env m, HasBuildRootDir env, MonadThrow m) => PackageIdentifier -> m (Path Abs Dir)
askPackageLogDir pi = do
  logDir <- askLogDir
  packageDir <- parseRelDir (packageIdentifierString pi)
  return (logDir </> packageDir)

askPackageSourceDir :: (MonadReader env m, HasSourceDir env, MonadThrow m) => PackageIdentifier -> m (Path Abs Dir) 
askPackageSourceDir pi = do
  sourceDir <- askSourceDir
  packageDir <- parseRelDir (packageIdentifierString pi)
  return (sourceDir </> packageDir)

askPackageBuildDir :: (MonadReader env m, HasBuildRootDir env, MonadThrow m) => PackageIdentifier -> m (Path Abs Dir)
askPackageBuildDir pi = do
  buildRootDir <- reader getBuildRootDir
  packageDir <- parseRelDir (packageIdentifierString pi)
  return (buildRootDir </> $(mkRelDir "dist") </> packageDir)
