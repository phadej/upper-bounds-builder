module Data.Binary.Tagged.Extra (
  cached,
  module Data.Binary.Tagged
  ) where

import Control.Monad.Catch
import Data.Binary
import Data.Binary.Tagged
import Path
import System.Directory

cached :: (Binary a, HasStructuralInfo a, HasSemanticVersion a) => Path Abs File -> IO a -> IO a
cached path mx = do
  e <- tryIO $ taggedDecodeFileOrFail (toFilePath path)
  case e of
    Right (Right x)  -> return x
    _                -> do print ("Error decoding " ++ show path)
                           x <- mx
                           createDirectoryIfMissing True $ toFilePath $ Path.parent path
                           taggedEncodeFile (toFilePath path) x
                           return x

tryIO :: IO a -> IO (Either IOError a)
tryIO = try
