{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

import AMASH.Data.Vendor.Address
import AMASH.Data.Vendor.VendorExternalLinks
import AMASH.Data.Vendor.VendorSupportDetails
import AMASH.Data.Vendor.VendorPrograms
import AMASH.Data.Vendor.VendorCloudSecurity
import AMASH.Data.Vendor.VendorSolutionPartner

data Vendor = Vendor { name :: Text
                     , description :: Maybe Text
                     , address :: Maybe Address
                     , email :: Text
                     , phone :: Maybe Text
                     , vendorLinks :: VendorExternalLinks
                     , supportDetails :: VendorSupportDetails
                     , otherContactDetails :: Maybe Text
                     , programs :: Maybe VendorPrograms
                     , cloudSecurity :: Maybe VendorCloudSecurity
                     , solutionPartner :: Maybe VendorSolutionPartner
                     } deriving (Show, Eq, Generic)

instance FromJSON Vendor
instance ToJSON Vendor
instance FromBSON Vendor
instance ToBSON Vendor
