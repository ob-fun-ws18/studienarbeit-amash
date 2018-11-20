{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App.StorableImageAssetSummary where

import Data.Text
import Data.Aeson
import GHC.Generics

import qualified AMASH.Data.App.ImageAssetSummary as ImageAssetSummary

data StorableImageAssetSummary = StorableImageAssetSummary { self :: Text
                                                           , image :: Text
                                                           , unscaled :: Text
                                                           } deriving (Show, Eq, Generic)

instance FromJSON StorableImageAssetSummary
instance ToJSON StorableImageAssetSummary

createStorableImageAssetSummary :: Maybe ImageAssetSummary.ImageAssetSummary -> Maybe StorableImageAssetSummary
createStorableImageAssetSummary Nothing = Nothing
createStorableImageAssetSummary (Just imageAssetSummary) =
    let maybeLinks = ImageAssetSummary._links imageAssetSummary
    in case maybeLinks of
        Nothing      -> Nothing
        (Just links) -> Just StorableImageAssetSummary {
                self = ImageAssetSummary.href $ ImageAssetSummary.self links,
                image = ImageAssetSummary.href $ ImageAssetSummary.image links,
                unscaled = ImageAssetSummary.href $ ImageAssetSummary.unscaled links
            }