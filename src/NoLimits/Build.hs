{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Build where

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text as T
import           Data.Traversable
import           Data.Yaml
import           Distribution.Extra
import           Distribution.PackDeps
import           Distribution.Package
import           Path
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(ExitSuccess))
import qualified System.Process as Process
import           System.Process.ByteString (readCreateProcessWithExitCode)

import           NoLimits.Batch.Job
import           NoLimits.Batch.SeqEval (evalBatch)
import           NoLimits.Paths
import           NoLimits.Types.BuildStep
import           NoLimits.Types.Result

data BuildEnv = BuildEnv
  { beBuildRootDir :: Path Abs Dir
  , beSourceDir    :: Path Abs Dir
  }
  deriving (Show)

instance HasBuildRootDir BuildEnv where
  getBuildRootDir = beBuildRootDir

instance HasSourceDir BuildEnv where
  getSourceDir = beSourceDir

type Tag = (BuildStep, PackageIdentifier)
type Action = Job Tag (LoggingT IO) Result

-- | Run command
command :: Path Abs Dir -- ^ workind directory
        -> String       -- ^ command
        -> [String]     -- ^ arguments
        -> IO (ExitCode, ByteString, ByteString)
command cwd cmd args = readCreateProcessWithExitCode cp' BS.empty
  where cp  = Process.proc cmd args
        cp' = cp { Process.cwd  = Just (toFilePath cwd )}

askLogFile :: (MonadReader env m, HasBuildRootDir env, MonadThrow m)
            => BuildStep          --
            -> PackageIdentifier  --
            -> String             -- ^ Extension
            -> m (Path Abs File)
askLogFile step pkgId ext = do
  logDir <- askPackageLogDir pkgId
  logFile <- parseRelFile (buildStepString step <> "." <> ext)
  return $ logDir </> logFile

showTag :: Tag -> Text
showTag (step, pkgId) = T.pack $
  buildStepString step <> " " <> packageIdentifierString pkgId


genericStep :: (MonadReader env m, HasBuildRootDir env, MonadThrow m)
            => Set Tag                                                       -- ^ Prerequisites
            -> (PackageIdentifier -> IO (ExitCode, ByteString, ByteString))  -- ^ The action
            -> Tag
            -> m Action
genericStep prereq action tag@(step, pkgId) = do
  outFile <- askLogFile step pkgId "out"
  errFile <- askLogFile step pkgId "err"
  resFile <- askLogFile step pkgId "res"
  -- Actual action
  let action'' = do (c, out, err) <- action pkgId
                    createDirectoryIfMissing True $ toFilePath $ parent outFile
                    BS.writeFile (toFilePath outFile) out
                    BS.writeFile (toFilePath errFile) err
                    let result = exitCodeToResult c
                    encodeFile (toFilePath resFile) result
                    return result
  -- Short circuiting action
  let action' _ =
        do prevResult <- liftIO $ tryIO $ decodeFileEither (toFilePath resFile)
           case prevResult of
             Right (Right result) -> do $(logInfo) $ showTag tag <> " cached"
                                        return result
             _ -> do $(logInfo) $ showTag tag
                     liftIO action''
  return $ Job
    { jobPrereq = prereq
    , jobResult = tag
    , jobAction = action'
    }
  where exitCodeToResult ExitSuccess = ResultOk
        exitCodeToResult _           = errorResult
        errorResult                  = ResultError [(step, pkgId)]

tryIO :: IO a -> IO (Either IOError a)
tryIO = try

stepGetAction :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => DescInfo -> m Action
stepGetAction di = do
  sourceDir <- askSourceDir
  let action pkgId = command sourceDir "cabal" ["get", "-v2", packageIdentifierString pkgId]
  genericStep Set.empty action (StepGet, (diPackage di))



makeBuild :: BuildEnv -> [DescInfo] -> IO ()
makeBuild benv dis = do
  jobs <- runReaderT (traverse stepGetAction dis) benv
  res <- evalBatch (fmap (transformJobAction runStderrLoggingT) jobs)
  print $ Prelude.map snd $ Prelude.filter ((/= ResultOk) . snd) $ Map.toList res
