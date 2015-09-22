{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Binary.Orphans
import           Data.Binary.Tagged
import           Data.Data (Typeable)
import           Data.List as L
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable
import           Data.Version
import           Data.Yaml
import           Distribution.PackDeps
import           Distribution.Package
import           System.Directory
import           System.FilePath

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

main :: IO ()
main = do
  pkgcfg <- decodeFileThrow "config.yaml" :: IO (Map String PackageConfig)
  print pkgcfg
  newest <- cached ".cache/newest" loadNewest
  reverses <- cached ".cache/reverses" (return (getReverses newest))
  let packageInput = Map.keys pkgcfg -- TODO: filter skippable
  print packageInput
  case traverse (flip Map.lookup newest >=> piDesc) packageInput of
    Nothing  -> print $ "not such packages: " ++ show packageInput
    Just dis -> do let alldeps = deepDeps newest dis -- Consider all dependencies!
                   putStrLn "set -ex"
                   putStrLn ""
                   --mapM_ putStrLn (concatMap buildLine (buildOrder alldeps))
                   -- mapM_ putStrLn (concatMap buildLineTest (buildOrder alldeps))
                   --mapM_ putStrLn (concatMap buildLine' alldeps)
                   mapM_ putStrLn (concatMap buildLine' (buildOrder alldeps))

showPackageIdentifier :: PackageIdentifier -> String
showPackageIdentifier (PackageIdentifier (PackageName name) version) = name ++ "-" ++ showVersion version

-- | See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/VersionHistory>
alwaysPresent :: [String]
alwaysPresent = ["rts", "ghc", "ghc-prim", "base", "bin-package-db", "hpc", "Win32", "integer", "integer-gmp", "integer-simple", "integer-gmp2", "par-classes"]
  ++ words "Cabal array base bin-package-db binary bytestring containers deepseq directory filepath ghc ghc-prim haskeline hoopl hpc integer-gmp pretty process rts template-haskell terminfo time transformers unix xhtml"

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

--

type BuildLine = String

buildLine' :: DescInfo -> [BuildLine]
buildLine' di
  | name `elem` alwaysPresent = []
  | otherwise = [ namever ]
  where namever = showPackageIdentifier . diPackage $ di
        name = packageIdentifierName . diPackage $ di

buildLine :: DescInfo -> [BuildLine]
buildLine di
  | name `elem` alwaysPresent = []
  | otherwise = [ "cd /app"
                , "if [ ! -d " <> namever <> " ]; then"
                , "  cabal get " <> namever <> " || true"
                , "  cd /app/" <> namever
                , "  cabal configure -v2 --allow-newer"
                , "  cabal install -v2 --allow-newer"
                , "fi"
                ]
  where namever = showPackageIdentifier . diPackage $ di
        name = packageIdentifierName . diPackage $ di

buildLineTest :: DescInfo -> [BuildLine]
buildLineTest di
  | name `elem` alwaysPresent = []
  | otherwise = [ "cd /app/" <> namever
                , "if [ ! -f /app/.results/" <> namever <> " ]; then"
                , "  cabal configure -v2 --allow-newer --enable-tests"
                , "  touch /app/.results/" <> namever
                , "  cabal test --log /app/.results/" <> namever
                ,  "fi"
                ]
  where namever = showPackageIdentifier . diPackage $ di
        name = packageIdentifierName . diPackage $ di

dependencyName :: Dependency -> String
dependencyName (Dependency (PackageName name) _) = name

packageIdentifierName :: PackageIdentifier -> String
packageIdentifierName (PackageIdentifier (PackageName name) _) = name

-- Config

instance FromJSON PackageName where
  parseJSON = fmap PackageName . parseJSON

newtype AptPackage = AptPackage String
  deriving (Eq, Ord, Show)

instance FromJSON AptPackage where
  parseJSON = fmap AptPackage . parseJSON

newtype CliFlag = CliFlag String
  deriving (Eq, Ord, Show)

instance FromJSON CliFlag where
  parseJSON = fmap CliFlag . parseJSON

data PackageConfig = PackageConfig
  { pcBuild          :: Bool
  , pcAllowNewer     :: Bool
  , pcExtraFlags     :: [CliFlag]
  , pcAptPackages    :: [AptPackage]
  , pcExpectTestFail :: Bool
  }
  deriving (Eq, Ord, Show)

instance FromJSON PackageConfig where
  parseJSON = withObject "Package config" $ \obj ->
    PackageConfig <$> obj .:? "skip" .!= True
                  <*> obj .:? "allow-newer" .!= True
                  <*> obj .:? "extra-flags" .!= []
                  <*> obj .:? "apt-packages" .!= []
                  <*> obj .:? "expected-test-failure" .!= False
