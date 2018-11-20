{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App.AppEmbedded where

import Data.Text
import Data.Aeson
import GHC.Generics

import AMASH.Data.App.ImageAssetSummary
import AMASH.Data.App.AddonCategorySummary
import AMASH.Data.App.AddonDistributionSummary

data AppEmbedded = AppEmbedded { banner :: Maybe ImageAssetSummary
                               , categories :: Maybe [AddonCategorySummary]
                               , logo :: Maybe ImageAssetSummary
                               , titleLogo :: Maybe ImageAssetSummary
                               , distribution :: Maybe AddonDistributionSummary
                               --, reviews :: Maybe ReviewCollectionSummary TODO: explicit reviews call
                               --, vendor :: Maybe VendorSummary TODO: get ID + make explicit vendor call (cut off /rest/2/vendors/{ID})
                               --, version :: Maybe AddonVersion TODO: explicit version call?
                               } deriving (Show, Eq, Generic)

instance FromJSON AppEmbedded
instance ToJSON AppEmbedded