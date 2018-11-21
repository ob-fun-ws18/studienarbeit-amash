{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.VendorSupportDetails where

import Data.Text
import Data.Aeson
import GHC.Generics

data VendorSupportDetails = VendorSupportDetails { supportOrg :: Maybe SupportOrganization
                                                 , targetResponseTime :: Maybe Int
                                                 , supportHours :: Maybe SupportHours
                                                 , emergencyContact :: Maybe Text
                                                 } deriving (Show, Eq, Generic)

instance FromJSON VendorSupportDetails
instance ToJSON VendorSupportDetails

data SupportOrganization = SupportOrganization { name :: Text
                                               , supportEmail :: Maybe Text
                                               , supportUrl :: Maybe Text
                                               , supportPhone :: Maybe Text
                                               } deriving (Show, Eq, Generic)

instance FromJSON SupportOrganization
instance ToJSON SupportOrganization


data SupportHours = SupportHours { range :: Maybe TimeRange
                                 , timezone :: Text
                                 , days  :: [Text]
                                 , holidays :: [Holiday]
                                 } deriving (Show, Eq, Generic)

instance FromJSON SupportHours
instance ToJSON SupportHours

data TimeRange = TimeRange { from :: Text
                           , until :: Text
                           } deriving (Show, Eq, Generic)

instance FromJSON TimeRange
instance ToJSON TimeRange

data Holiday = Holiday { title :: Text
                       , date :: Text
                       , repeatAnnually :: Bool
                       } deriving (Show, Eq, Generic)

instance FromJSON Holiday
instance ToJSON Holiday