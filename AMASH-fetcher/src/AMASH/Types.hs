{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AMASH.Types where

import Data.Text
import Data.Aeson
import GHC.Generics

data AppInfo = AppInfo { name :: Text
                       , key :: Text
                       , summary :: Text
                       , tagLine :: Text
                       , vendorLinks :: VendorLinks
                       , lastModified :: Text -- format: date-time e.g.: "2018-10-03T00:17:34.548Z"
                       } deriving (Show, Eq, Generic)

instance FromJSON AppInfo
instance ToJSON AppInfo

data VendorLinks = VendorLinks { issueTracker :: Maybe Text
                               , supportTicketSystem :: Maybe Text
                               , appStatusPage :: Maybe Text
                               , forums :: Maybe Text
                               , privacy :: Maybe Text
                               } deriving (Show, Eq, Generic)

instance FromJSON VendorLinks
instance ToJSON VendorLinks

data AddonEmbedded = AddonEmbedded
