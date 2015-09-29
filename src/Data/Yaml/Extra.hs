module Data.Yaml.Extra (
  decodeFileThrow,
  module Data.Aeson,
  module Data.Yaml,
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson (withObject)
import Data.Yaml

decodeFileThrow :: (MonadThrow m, MonadIO m) => FromJSON a => FilePath -> m a
decodeFileThrow path = do
  e <- liftIO $ decodeFileEither path
  case e of
    Right x   -> return x
    Left exc  -> throwM exc
