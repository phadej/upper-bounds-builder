{-# LANGUAGE OverloadedStrings #-}
module NoLimits.Types.Result where

import Control.Applicative
import Data.Aeson.Compat hiding (Result)
import Data.Monoid
import Data.Text
import Distribution.Package

import NoLimits.Types.BuildStep
import NoLimits.Orphans ()

data Result = ResultOk
            | ResultError [(BuildStep, PackageIdentifier)]
  deriving (Eq, Ord, Show, Read)

instance Monoid Result where
  mempty = ResultOk
  ResultOk `mappend` x = x
  x `mappend` ResultOk = x
  ResultError x `mappend` ResultError y = ResultError $ x <> y

instance ToJSON Result where
  toJSON ResultOk          = object [ "status" .= ("ok" :: Text) ]
  toJSON (ResultError err) = object
    [ "status" .= ("error" :: Text)
    , "errors" .= err
    ]

instance FromJSON Result where
  parseJSON = withObject "Result" $ \obj -> do
    status <- obj .: "status"
    case status of
      "ok"    -> pure ResultOk
      "error" -> ResultError <$> obj .: "errors"
      _       -> fail $ "Invalid status: " <> status

resultLeftBias :: Result -> Result -> Result
resultLeftBias ResultOk _                            = ResultOk
resultLeftBias (ResultError err) ResultOk            = ResultError err
resultLeftBias (ResultError err) (ResultError err')  = ResultError (err <> err')
