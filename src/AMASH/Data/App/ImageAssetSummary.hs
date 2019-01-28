{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App.ImageAssetSummary where

import Data.Text
import Data.Aeson
import GHC.Generics

newtype ImageAssetSummary = ImageAssetSummary { _links :: Maybe ImageAssetLinks } deriving (Show, Eq, Generic)

instance FromJSON ImageAssetSummary
instance ToJSON ImageAssetSummary

data ImageAssetLinks = ImageAssetLinks { self :: ImageAssetLink
                                       , image :: ImageAssetLink
                                       , unscaled :: ImageAssetLink
                                       } deriving (Show, Eq, Generic)

instance FromJSON ImageAssetLinks
instance ToJSON ImageAssetLinks

newtype ImageAssetLink = ImageAssetLink { href :: Text } deriving (Show, Eq, Generic)

instance FromJSON ImageAssetLink
instance ToJSON ImageAssetLink