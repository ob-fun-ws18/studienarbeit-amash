{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.VendorCloudSecurity where

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock

data VendorCloudSecurity = VendorCloudSecurity { approved  :: Bool
                                               , issueKey :: Maybe Text
                                               , approvalDate :: Maybe UTCTime
                                               , expiryDate :: Maybe UTCTime
                                               } deriving (Show, Eq, Generic)

instance FromJSON VendorCloudSecurity
instance ToJSON VendorCloudSecurity
