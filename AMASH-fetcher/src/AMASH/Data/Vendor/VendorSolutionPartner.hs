{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.VendorSolutionPartner where

import Data.Text
import Data.Aeson
import GHC.Generics

newtype VendorSolutionPartner = VendorSolutionPartner { partnerLevel :: Text } deriving (Show, Eq, Generic)

instance FromJSON VendorSolutionPartner
instance ToJSON VendorSolutionPartner

