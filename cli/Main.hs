{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

-- TODO: skip-benchmarks
-- TODO: expected-test-failure

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer.Strict
import           Data.Bifunctor
import           Data.Binary.Tagged.Extra
import           Data.Foldable
import           Data.Function (on)
import           Data.List as L (sort, sortBy, partition)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Traversable
import           Data.Version
import           Data.Yaml.Extra
import           Distribution.Extra
import           Distribution.Package
import           Options.Applicative as O
import           Path
import           Prelude hiding (elem, all, notElem)
import           System.Directory
import           System.FilePath hiding ((</>))
import           System.IO (stderr, hPutStrLn, hPutStr)

import qualified Bourne
import           Distribution.PackDeps

import           NoLimits.Build
import           NoLimits.CheckDeps
import           NoLimits.Options
import           NoLimits.Paths
import           NoLimits.Plan
import           NoLimits.Setup
import           NoLimits.Types
import           NoLimits.Types.BuildInfo

import           Debug.Trace

optionParser :: O.Parser (IO ())
optionParser = subparser $ mconcat
  [ O.command "setup" $ info (helper <*> (cmdSetup <$> setupOptsParser)) $ progDesc "Run setup commands"
  , O.command "build" $ info (helper <*> (cmdBuildTmp <$> setupOptsParser)) $ progDesc "Run the build"
  ]

main :: IO ()
main = join (execParser opts)
  where
    opts = info (helper <*> optionParser)
      ( fullDesc
     <> header "upper-bounds-builder - building without limits" )

cmdBuildTmp :: SetupOpts -> IO ()
cmdBuildTmp SetupOpts {..} = do
  pkgcfg <- decodeFileThrow "config.yaml" :: IO PackageConfigs
  newest <- cached (soPath </> $(mkRelFile "cache/newest")) loadNewest
  alldeps <- plan newest pkgcfg
  makeBuild benv $ sortBy (compare `on` biPackage) alldeps
  where
    benv = BuildEnv
      { beBuildRootDir = soPath
      , beSourceDir    = soPath </> $(mkRelDir "src")
      }

{-
cmdBuild :: BuildOpts -> IO ()
cmdBuild bo = do
  pkgcfg <- decodeFileThrow "config.yaml" :: IO PackageConfigs
  newest <- cached (boPath bo </> $(mkRelFile "cache/newest")) loadNewest
  alldeps <- plan newest pkgcfg

  let buildOrderDeps = buildOrder alldeps
  let alphaOrderDeps = sortBy (compare `on` packageIdentifierName . diPackage) alldeps
  -- Print package closure
  hPutStrLn stderr "Package to be built:"
  traverse_ (hPutStrLn stderr) . sort . fmap (packageIdentifierName . diPackage) $ alldeps
  -- Check upper bounds
  hPutStr stderr "---"
  hPutStr stderr $ checkDepsCli newest alldeps
  -- Output test build script
  putStrLn . Bourne.showScript $ do
    let aptPkgs = fmap getAptPackage . Prelude.concatMap pcAptPackages . Map.elems $ pkgcfg
    Bourne.cmd "set" ["-ex"]
    -- Create directories
    Bourne.mkdirp "/app/src"
    Bourne.mkdirp "/app/log"
    Bourne.mkdirp "/app/dist"
    -- Install system dependencies
    Bourne.test (Bourne.fileNotExists aptGetInstallLog) $ do
      Bourne.touch aptGetInstallLog
      Bourne.cmd "apt-get" $ words "-yq --no-install-suggests --no-install-recommends --force-yes install" ++ aptPkgs ++ ["2>&1", ">", aptGetInstallLog]
    -- Download packages
    Bourne.cd "/app/src"
    traverse_ downloadScript alphaOrderDeps
    -- Install
    traverse_ (installScript pkgcfg) buildOrderDeps
    -- Test
    traverse_ (testScript pkgcfg) alphaOrderDeps

aptGetInstallLog :: FilePath
aptGetInstallLog = "/app/log/apt-get-install.log"

buildOrder :: [DescInfo] -> [DescInfo]
buildOrder = go []
  where
    go visited []   = visited
    go visited left = go (visited ++ curr) next
      where (curr, next) = L.partition p left
            visitedNames :: [String]
            visitedNames = fmap (packageIdentifierName . diPackage) visited
            p :: DescInfo -> Bool
            p di         = all (`elem` (alwaysPresent ++ visitedNames)) deps
              where deps :: [String]
                    deps = L.sort $ fmap dependencyName $ diLibDeps di

-- Scripts

downloadScript :: DescInfo -> Bourne.Script
downloadScript di = Bourne.test (Bourne.dirNotExists namever) (Bourne.cmd "cabal" ["get", "-v0", namever])
  where namever = packageIdentifierString . diPackage $ di

installScript :: PackageConfigs -> DescInfo -> Bourne.Script
installScript pkgcfg di =
  Bourne.test (Bourne.dirNotExists distDir) $ do
    Bourne.cd srcDir
    Bourne.mkdirp logDir
    cabalCmd "configure" distDir (logDir <> "/configure.log") $ allowNewer ++ extraFlags
    cabalCmd "build"     distDir (logDir <> "/build.log")     $ []
    cabalCmd "copy"      distDir (logDir <> "/copy.log")      $ []
    cabalCmd "register"  distDir (logDir <> "/register.log")  $ []
  where namever    = packageIdentifierString . diPackage $ di
        name       = packageIdentifierName . diPackage $ di
        srcDir     = "/app/src/" <> namever
        distDir    = "/app/dist/" <> namever
        logDir     = "/app/log/" <> namever
        pc         = lookupPackageConfig name pkgcfg
        allowNewer = if pcAllowNewer pc then ["--allow-newer"] else []
        extraFlags = getCliFlag <$> pcExtraFlags pc

testScript :: PackageConfigs -> DescInfo -> Bourne.Script
testScript pkgcfg di
  | pcSkipTests pc = return ()
  | otherwise      = testScript' pkgcfg di
  where pc    =  lookupPackageConfig name pkgcfg
        name = packageIdentifierName . diPackage $ di

testScript' :: PackageConfigs -> DescInfo -> Bourne.Script
testScript' pkgcfg di = Bourne.test (Bourne.fileNotExists testLogFile) $ do
                          Bourne.cd srcDir
                          Bourne.touch testLogFile
                          -- TODO: add benchmarks back
                          cabalCmd "configure" distDir (logDir <> "/test-configure.log") $ ["--enable-tests" ] ++ allowNewer ++ extraFlags
                          cabalCmd "build"     distDir (logDir <> "/test-build.log")     $ []
                          -- doctest hack, it requires dist/ 
                          Bourne.test (Bourne.dirNotExists "dist") $ Bourne.cmd "ln" ["-s", distDir, "dist"]
                          cabalCmd "test"      distDir testLogFile                       $ []
                                   -- TODO: remove dist link (test -h)
  where namever    = packageIdentifierString . diPackage $ di
        name       = packageIdentifierName . diPackage $ di
        srcDir     = "/app/src/" <> namever
        distDir    = "/app/dist/" <> namever
        logDir     = "/app/log/" <> namever
        testLogFile = logDir <> "/test.log"
        pc         = lookupPackageConfig name pkgcfg
        allowNewer = if pcAllowNewer pc then ["--allow-newer"] else []
        extraFlags = getCliFlag <$> pcExtraFlags pc

cabalCmd :: String -> FilePath -> FilePath -> [String] -> Bourne.Script
cabalCmd cmd buildDir logFile params = Bourne.cmd "cabal" $ [ cmd, buildDirFlag, "-v2" ] ++ params ++ ["2>&1", ">", logFile]
  where buildDirFlag = "--builddir=" <> buildDir

-}

