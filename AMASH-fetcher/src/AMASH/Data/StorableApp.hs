{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.StorableApp where

import Data.Text
import Data.Aeson
import GHC.Generics

import qualified AMASH.Data.App as App
import AMASH.Data.App.VendorLinks
import AMASH.Data.App.AddonDistributionSummary
import qualified AMASH.Data.App.AppEmbedded as AppEmbedded
import qualified AMASH.Data.App.AddonCategorySummary as AddonCategorySummary
import qualified AMASH.Data.App.StorableImageAssetSummary as StorableImageAssetSummary

data StorableApp = StorableApp { name :: Text
               , summary :: Text
               , tagLine :: Text
               , vendorLinks :: VendorLinks
               , lastModified :: Text -- format: date-time e.g.: "2018-10-03T00:17:34.548Z"
               , banner :: Maybe StorableImageAssetSummary.StorableImageAssetSummary
               , categories :: [Text]
               , logo :: Maybe StorableImageAssetSummary.StorableImageAssetSummary
               , titleLogo :: Maybe StorableImageAssetSummary.StorableImageAssetSummary
               , distribution :: Maybe AddonDistributionSummary
               } deriving (Show, Eq, Generic)

instance FromJSON StorableApp
instance ToJSON StorableApp

createStorableApp :: App.App -> StorableApp
createStorableApp app = StorableApp {
    name            = App.name app,
    summary         = App.summary app,
    tagLine         = App.tagLine app,
    vendorLinks     = App.vendorLinks app,
    lastModified    = App.lastModified app,

    distribution    = AppEmbedded.distribution embeddedApp,
    categories      = unpackCategories $ AppEmbedded.categories embeddedApp,

    logo            = createImageLinks $ AppEmbedded.logo embeddedApp,
    titleLogo       = createImageLinks $ AppEmbedded.titleLogo embeddedApp,
    banner          = createImageLinks $ AppEmbedded.banner embeddedApp
} where embeddedApp      = App._embedded app
        createImageLinks = StorableImageAssetSummary.createStorableImageAssetSummary

unpackCategories :: Maybe [AddonCategorySummary.AddonCategorySummary] -> [Text]
unpackCategories Nothing = []
unpackCategories (Just categories) = Prelude.map AddonCategorySummary.name categories
