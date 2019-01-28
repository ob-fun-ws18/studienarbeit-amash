{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.VendorSolutionPartner where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

newtype VendorSolutionPartner = VendorSolutionPartner { partnerLevel :: Text } deriving (Show, Eq, Generic)

instance FromJSON VendorSolutionPartner
instance ToJSON VendorSolutionPartner
instance FromBSON VendorSolutionPartner
instance ToBSON VendorSolutionPartner

