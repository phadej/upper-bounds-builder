{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module NoLimits.Build (makeBuild, BuildEnv(..)) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson (withObject)
import           Data.ByteString as BS
import           Data.Data (Typeable)
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text as T
import           Data.Traversable
import           Data.Yaml
import           Distribution.Extra
import           Distribution.Package
import           Path
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(ExitSuccess))
import           System.FilePath (dropTrailingPathSeparator)
import qualified System.Process as Process
import           System.Process.ByteString (readCreateProcessWithExitCode)

import           NoLimits.Batch.Job
import           NoLimits.Batch.SeqEval (evalBatch)
import           NoLimits.Paths
import           NoLimits.Types.BuildStep
import           NoLimits.Types.BuildInfo
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

data Tag = Tag !BuildStep !PackageIdentifier
  deriving (Eq, Ord, Show)
type Action = Job Tag (LoggingT IO) Result

instance ToJSON Tag where
  toJSON (Tag step name) = object [ "step" .= step, "package" .= name ]

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \obj -> 
    Tag <$> obj .: "step"
        <*> obj .: "package"

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
showTag (Tag step pkgId) = T.pack $
  buildStepString step <> " " <> packageIdentifierString pkgId

data BuildError = BuildError
  deriving (Show, Typeable)

instance Exception BuildError

genericStep :: (MonadReader env m, HasBuildRootDir env, MonadThrow m)
            => Set Tag                                -- ^ Prerequisites
            -> IO (ExitCode, ByteString, ByteString)  -- ^ The action
            -> BuildStep
            -> PackageIdentifier
            -> m Action
genericStep prereq action step pkgId = do
  outFile <- askLogFile step pkgId "out"
  errFile <- askLogFile step pkgId "err"
  resFile <- askLogFile step pkgId "res"
  -- Actual action
  let action'' = do (c, out, err) <- action
                    when (c /= ExitSuccess) $ do BS.putStr out
                                                 BS.putStr err
                                                 throwM BuildError
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
        tag                          = Tag step pkgId

tryIO :: IO a -> IO (Either IOError a)
tryIO = try

stepGetAction :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => BuildInfo -> m Action
stepGetAction di = do
  sourceDir <- askSourceDir
  let action = command sourceDir "cabal" ["get", "-v2", packageIdentifierString pkgId]
  genericStep Set.empty action StepGet pkgId
  where pkgId = biPackage di

simpleStepAction :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m)
                 => BuildStep  -- ^ Previous step
                 -> BuildStep  -- ^ This step
                 -> String     -- ^ Cabal command
                 -> [String]   -- ^ Additional cabal flags
                 -> BuildInfo
                 -> m Action
simpleStepAction prevStep currStep cabalCommand cabalFlags bi = do
  pkgSourceDir <- askPackageSourceDir pkgId
  pkgBuildDir  <- askPackageBuildDir pkgId
  let args = [ cabalCommand ] ++ cabalFlags ++
             [ "-v2"
             , "--builddir=" <> toFilePathNoTrailingSlash pkgBuildDir
             ]
  let action = command pkgSourceDir "cabal" args
  genericStep dependencies action currStep pkgId
  where pkgId = biPackage bi
        dependencies = Set.singleton $ Tag prevStep pkgId

stepConfigureAction:: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => BuildInfo -> m Action
stepConfigureAction bi = do
  pkgSourceDir <- askPackageSourceDir pkgId
  pkgBuildDir  <- askPackageBuildDir pkgId
  installDir   <- askInstallDir
  packageDbDir <- askPackageDbDir
  let args = [ "configure"
             , "-v2"
             , "--allow-newer"
             -- Build directory
             , "--builddir=" <> toFilePathNoTrailingSlash pkgBuildDir
             -- Package databases
             , "--package-db=clear"
             , "--package-db=global"
             , "--package-db=" <> toFilePathNoTrailingSlash packageDbDir
             -- Install directories
             , "--libdir="     <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "lib"))
             , "--bindir="     <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "bin"))
             , "--datadir="    <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "share"))
             , "--libexecdir=" <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "libexec"))
             , "--sysconfdir=" <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "etc"))
             , "--docdir="     <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "doc"))
             , "--htmldir="    <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "html"))
             , "--haddockdir=" <> toFilePathNoTrailingSlash (installDir </> $(mkRelDir "haddock"))
             ] ++ bcExtraFlags (biConfig bi)
  let action = command pkgSourceDir "cabal" args
  genericStep dependencies action StepConfigure pkgId
  where pkgId = biPackage bi
        dependencies = Set.fromList $ fmap (Tag StepRegister) $ biLibDeps bi

stepBuildAction :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => BuildInfo -> m Action
stepBuildAction = simpleStepAction StepConfigure StepBuild "build" ["-j1"]

stepCopyAction :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => BuildInfo -> m Action
stepCopyAction = simpleStepAction StepBuild StepCopy "copy" []

stepRegisterAction :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => BuildInfo -> m Action
stepRegisterAction = simpleStepAction StepBuild StepRegister "register" []

-- TODO: Move to Path.Extra
toFilePathNoTrailingSlash :: Path Abs Dir -> FilePath  
toFilePathNoTrailingSlash = dropTrailingPathSeparator . toFilePath

allActions :: (MonadReader env m, HasBuildRootDir env, HasSourceDir env, MonadThrow m) => BuildInfo -> m [Action]
allActions bi = do
  a0 <- stepGetAction bi
  a1 <- stepConfigureAction bi
  a2 <- stepBuildAction bi
  a3 <- stepCopyAction bi
  a4 <- stepRegisterAction bi
  return [ a0, a1, a2, a3, a4 ]

makeBuild :: BuildEnv -> [BuildInfo] -> IO ()
makeBuild benv dis = do
  jobs <- Prelude.concat <$> runReaderT (traverse allActions dis) benv
  res <- evalBatch (fmap (transformJobAction runStderrLoggingT) jobs)
  print $ Prelude.map snd $ Prelude.filter ((/= ResultOk) . snd) $ Map.toList res
