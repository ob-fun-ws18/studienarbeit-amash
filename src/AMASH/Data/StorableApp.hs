{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.StorableApp where

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock
import GenericBson

import qualified AMASH.Data.App as App
import qualified AMASH.Data.App.VendorLinks as VendorLinks
import qualified AMASH.Data.App.AppEmbedded as AppEmbedded
import qualified AMASH.Data.App.AddonCategorySummary as AddonCategorySummary
import qualified AMASH.Data.App.StorableImageAssetSummary as StorableImageAssetSummary

data StorableApp = StorableApp { name :: Text
               , summary :: Text
               , tagLine :: Text
               , vendorLinks :: VendorLinks.VendorLinks
               , lastModified :: UTCTime
               , banner :: Maybe StorableImageAssetSummary.StorableImageAssetSummary
               , categories :: [Text]
               , logo :: Maybe StorableImageAssetSummary.StorableImageAssetSummary
               , titleLogo :: Maybe StorableImageAssetSummary.StorableImageAssetSummary
               } deriving (Show, Eq, Generic)

instance FromJSON StorableApp
instance ToJSON StorableApp
instance FromBSON StorableApp
instance ToBSON StorableApp

createStorableApp :: App.App -> StorableApp
createStorableApp app = StorableApp {
    name            = App.name app,
    summary         = App.summary app,
    tagLine         = App.tagLine app,
    vendorLinks     = App.vendorLinks app,
    lastModified    = App.lastModified app,

    categories      = unpackCategories $ AppEmbedded.categories embeddedApp,

    logo            = createImageLinks $ AppEmbedded.logo embeddedApp,
    titleLogo       = createImageLinks $ AppEmbedded.titleLogo embeddedApp,
    banner          = createImageLinks $ AppEmbedded.banner embeddedApp
} where embeddedApp      = App._embedded app
        createImageLinks = StorableImageAssetSummary.createStorableImageAssetSummary

unpackCategories :: Maybe [AddonCategorySummary.AddonCategorySummary] -> [Text]
unpackCategories Nothing = []
unpackCategories (Just categories) = Prelude.map AddonCategorySummary.name categories
