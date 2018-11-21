{-# LANGUAGE DeriveGeneric #-}
module AMASH.Data.Vendor.VendorPrograms where

import Data.Text
import Data.Aeson
import GHC.Generics

newtype VendorPrograms = VendorPrograms { topVendor :: Maybe TopVendor } deriving (Show, Eq, Generic)

instance FromJSON VendorPrograms
instance ToJSON VendorPrograms

newtype TopVendor = TopVendor { status :: Text } deriving (Show, Eq, Generic)

instance FromJSON TopVendor
instance ToJSON TopVendor
