{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AMASH.Types.App.ImageAssetSummary where

import Data.Text
import Data.Aeson
import GHC.Generics

data ImageAssetSummary = ImageAssetSummary { _links :: Maybe ImageAssetLinks } deriving (Show, Eq, Generic)

instance FromJSON ImageAssetSummary
instance ToJSON ImageAssetSummary

data ImageAssetLinks = ImageAssetLinks { self :: ImageAssetLink
                                       , image :: ImageAssetLink
                                       , unscaled :: ImageAssetLink
                                       } deriving (Show, Eq, Generic)

instance FromJSON ImageAssetLinks
instance ToJSON ImageAssetLinks

data ImageAssetLink = ImageAssetLink { href :: Text } deriving (Show, Eq, Generic)

instance FromJSON ImageAssetLink
instance ToJSON ImageAssetLink