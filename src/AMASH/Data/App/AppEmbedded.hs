{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App.AppEmbedded where

import Data.Text
import Data.Aeson
import GHC.Generics

import AMASH.Data.App.ImageAssetSummary
import AMASH.Data.App.AddonCategorySummary

data AppEmbedded = AppEmbedded { banner :: Maybe ImageAssetSummary
                               , categories :: Maybe [AddonCategorySummary]
                               , logo :: Maybe ImageAssetSummary
                               , titleLogo :: Maybe ImageAssetSummary
                               } deriving (Show, Eq, Generic)

instance FromJSON AppEmbedded
instance ToJSON AppEmbedded