module Data.Binary.Tagged.Extra (
  cached,
  module Data.Binary.Tagged
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Binary
import Data.Binary.Tagged
import Path
import System.Directory

cached :: (Binary a, HasStructuralInfo a, HasSemanticVersion a, MonadIO m) => Path Abs File -> m a -> m a
cached path mx = do
  e <- liftIO $ tryIO $ taggedDecodeFileOrFail (toFilePath path)
  case e of
    Right (Right x)  -> return x
    _                -> do -- print ("Error decoding " ++ show path)
                           x <- mx
                           liftIO $ createDirectoryIfMissing True $ toFilePath $ Path.parent path
                           liftIO $ taggedEncodeFile (toFilePath path) x
                           return x

tryIO :: IO a -> IO (Either IOError a)
tryIO = try
