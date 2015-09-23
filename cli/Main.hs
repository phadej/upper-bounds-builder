{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

-- TODO: skip-benchmarks
-- TODO: expected-test-failure

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Binary.Orphans
import           Data.Binary.Tagged
import           Data.Foldable
import           Data.Function (on)
import           Data.List as L (sort, sortBy, partition)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Traversable
import           Data.Version
import           Data.Yaml
import           Distribution.PackDeps
import           Distribution.Package
import           Prelude hiding (elem, all, notElem)
import           System.Directory
import           System.FilePath
import           System.IO (stderr, hPutStrLn)

import qualified Bourne

import           Debug.Trace

cached :: (Binary a, HasStructuralInfo a, HasSemanticVersion a) => FilePath -> IO a -> IO a
cached path mx = do
  e <- taggedDecodeFileOrFail path `catch` onIOError
  case e of
    Right x -> return x
    Left _  -> do print ("Error decoding " ++ path)
                  x <- mx
                  createDirectoryIfMissing True $ takeDirectory path
                  taggedEncodeFile path x
                  return x
  where
    onIOError :: IOError -> IO (Either a b)
    onIOError _ = print ("Error loading " ++ path) >> return (Left undefined)

decodeFileThrow :: (MonadThrow m, MonadIO m) => FromJSON a => FilePath -> m a
decodeFileThrow path = do
  e <- liftIO $ decodeFileEither path
  case e of
    Right x   -> return x
    Left exc  -> throwM exc

lookupPackageConfig :: String -> Map String PackageConfig -> PackageConfig
lookupPackageConfig name m = fromMaybe defPackageConfig (Map.lookup name m)

type PackageConfigs = Map String PackageConfig

type CheckDeps = Newest -> DescInfo -> (PackageName, Data.Version.Version, CheckDepsRes)

checkDepsCli :: CheckDeps -> Newest -> DescInfo -> IO ()
checkDepsCli cd newest di =
    case cd newest di of
        (_, _, AllNewest) -> return ()
        (PackageName pn, v, WontAccept p _) -> do
           hPutStrLn stderr $ mconcat
              [ pn
              , "-"
              , showVersion v
              , ": Cannot accept the following packages"
              ]
           flip traverse_ p $ \(x, y) -> hPutStrLn stderr $ "- " <> x <> "-" <> y

main :: IO ()
main = do
  pkgcfg <- decodeFileThrow "config.yaml" :: IO PackageConfigs
  newest <- cached ".cache/newest" loadNewest
  -- reverses <- cached ".cache/reverses" (return (getReverses newest))
  let packageInput = Map.keys pkgcfg
  case traverse (flip Map.lookup newest >=> piDesc) packageInput of
    Nothing  -> print $ "not such packages: " ++ show packageInput
    Just dis -> do
      let skippable = fst <$> filter (pcSkip . snd) (Map.toList pkgcfg)
      let alldeps = removeUnexisting newest skippable . deepDeps newest $ dis -- Consider all dependencies!
      let buildOrderDeps = buildOrder alldeps
      let alphaOrderDeps = sortBy (compare `on` packageIdentifierName . diPackage) alldeps
      -- Print package closure
      hPutStrLn stderr "Package to be built:"
      traverse_ (hPutStrLn stderr) . sort . fmap (packageIdentifierName . diPackage) $ alldeps
      -- Check upper bounds
      hPutStrLn stderr "---"
      traverse_ (checkDepsCli checkDeps newest) alldeps
      -- Output test build script
      putStrLn . Bourne.showScript $ do
        let aptPkgs = fmap getAptPackage . Prelude.concatMap pcAptPackages . Map.elems $ pkgcfg
        Bourne.cmd "set" ["-ex"]
        -- Install system dependencies
        Bourne.test (Bourne.fileNotExists aptGetInstallLog) $ do
          Bourne.touch aptGetInstallLog
          Bourne.cmd "apt-get" $ words "-yq --no-install-suggests --no-install-recommends --force-yes install" ++ aptPkgs ++ ["2>&1", ">", aptGetInstallLog]
        -- Create directories
        Bourne.mkdirp "/app/src"
        Bourne.mkdirp "/app/log"
        Bourne.mkdirp "/app/dist"
        -- Download packages
        Bourne.cd "/app/src"
        traverse_ downloadScript alphaOrderDeps
        -- Install
        traverse_ (installScript pkgcfg) buildOrderDeps
        -- Test
        traverse_ (testScript pkgcfg) alphaOrderDeps

aptGetInstallLog :: FilePath
aptGetInstallLog = "/app/log/apt-get-install.log"

removeUnexisting :: Newest -> [String] -> [DescInfo] -> [DescInfo]
removeUnexisting newest toSkip = map filterDeps . filter pDescInfo
  where pDescInfo di     = pName (packageIdentifierName . diPackage $ di)
        filterDeps di    = di { diDeps     = filter (pSelf di) . filter pDependency $ diDeps di
                              , diLibDeps  = filter (pSelf di) . filter pDependency $ diLibDeps di
                              }
        pSelf di dep     = packageIdentifierName (diPackage di) /= dependencyName dep
        pDependency dep  = pName (dependencyName dep)
        pName name       = Map.member name newest && notElem name toSkip'
        toSkip'          = alwaysPresent ++ toSkip

-- | See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory>
alwaysPresent :: [String]
alwaysPresent = words "Cabal Win32 array base bin-package-db binary bytestring containers deepseq directory filepath ghc ghc-prim haskeline hoopl hpc integer-gmp pretty process rts template-haskell terminfo time transformers unix xhtml"

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
  where namever = showPackageIdentifier . diPackage $ di

installScript :: PackageConfigs -> DescInfo -> Bourne.Script
installScript pkgcfg di = Bourne.test (Bourne.dirNotExists distDir) $ do
                            Bourne.cd srcDir
                            Bourne.mkdirp logDir
                            cabalCmd "configure" distDir (logDir <> "/configure.log") $ allowNewer ++ extraFlags
                            cabalCmd "build"     distDir (logDir <> "/build.log")     $ []
                            cabalCmd "copy"      distDir (logDir <> "/copy.log")      $ []
                            cabalCmd "register"  distDir (logDir <> "/register.log")  $ []
  where namever    = showPackageIdentifier . diPackage $ di
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
  where namever    = showPackageIdentifier . diPackage $ di
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

-- -> String functions

dependencyName :: Dependency -> String
dependencyName (Dependency (PackageName name) _) = name

packageIdentifierName :: PackageIdentifier -> String
packageIdentifierName (PackageIdentifier (PackageName name) _) = name

showPackageIdentifier :: PackageIdentifier -> String
showPackageIdentifier (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version

-- Config

instance FromJSON PackageName where
  parseJSON = fmap PackageName . parseJSON

newtype AptPackage = AptPackage { getAptPackage :: String }
  deriving (Eq, Ord, Show)

instance FromJSON AptPackage where
  parseJSON = fmap AptPackage . parseJSON

newtype CliFlag = CliFlag { getCliFlag :: String }
  deriving (Eq, Ord, Show)

instance FromJSON CliFlag where
  parseJSON = fmap CliFlag . parseJSON

data PackageConfig = PackageConfig
  { pcSkip           :: Bool
  , pcAllowNewer     :: Bool
  , pcExtraFlags     :: [CliFlag]
  , pcAptPackages    :: [AptPackage]
  , pcSkipTests      :: Bool
  , pcSkipBenchmarks :: Bool
  , pcExpectTestFail :: Bool
  }
  deriving (Eq, Ord, Show)

defPackageConfig :: PackageConfig
defPackageConfig = PackageConfig False True [] [] False False False

instance FromJSON PackageConfig where
  parseJSON = withObject "Package config" $ \obj ->
    PackageConfig <$> obj .:? "skip" .!= False
                  <*> obj .:? "allow-newer" .!= True
                  <*> obj .:? "extra-flags" .!= []
                  <*> obj .:? "apt-packages" .!= []
                  <*> obj .:? "skip-tests" .!= False
                  <*> obj .:? "skip-benchmarks" .!= False
                  <*> obj .:? "expected-test-failure" .!= False
