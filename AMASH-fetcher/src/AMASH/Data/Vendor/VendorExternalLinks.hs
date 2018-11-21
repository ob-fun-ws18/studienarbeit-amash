{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.VendorExternalLinks where

import Data.Text
import Data.Aeson
import GHC.Generics

data VendorExternalLinks = VendorExternalLinks { homePage :: Maybe Text
                                               , sla :: Maybe Text
                                               } deriving (Show, Eq, Generic)

instance FromJSON VendorExternalLinks
instance ToJSON VendorExternalLinks