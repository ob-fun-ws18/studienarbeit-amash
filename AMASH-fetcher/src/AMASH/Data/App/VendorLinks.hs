{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App.VendorLinks where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

data VendorLinks = VendorLinks { issueTracker :: Maybe Text
                               , supportTicketSystem :: Maybe Text
                               , appStatusPage :: Maybe Text
                               , forums :: Maybe Text
                               , privacy :: Maybe Text
                               } deriving (Show, Eq, Generic)

instance FromJSON VendorLinks
instance ToJSON VendorLinks
instance FromBSON VendorLinks
instance ToBSON VendorLinks