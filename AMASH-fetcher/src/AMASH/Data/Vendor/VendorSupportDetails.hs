{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.VendorSupportDetails where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

data VendorSupportDetails = VendorSupportDetails { supportOrg :: Maybe SupportOrganization
                                                 , targetResponseTime :: Maybe Int
                                                 , supportHours :: Maybe SupportHours
                                                 , emergencyContact :: Maybe Text
                                                 } deriving (Show, Eq, Generic)

instance FromJSON VendorSupportDetails
instance ToJSON VendorSupportDetails
instance FromBSON VendorSupportDetails
instance ToBSON VendorSupportDetails

data SupportOrganization = SupportOrganization { name :: Text
                                               , supportEmail :: Maybe Text
                                               , supportUrl :: Maybe Text
                                               , supportPhone :: Maybe Text
                                               } deriving (Show, Eq, Generic)

instance FromJSON SupportOrganization
instance ToJSON SupportOrganization
instance FromBSON SupportOrganization
instance ToBSON SupportOrganization


data SupportHours = SupportHours { range :: Maybe TimeRange
                                 , timezone :: Text
                                 , days  :: [Text]
                                 , holidays :: [Holiday]
                                 } deriving (Show, Eq, Generic)

instance FromJSON SupportHours
instance ToJSON SupportHours
instance FromBSON SupportHours
instance ToBSON SupportHours

data TimeRange = TimeRange { from :: Text
                           , until :: Text
                           } deriving (Show, Eq, Generic)

instance FromJSON TimeRange
instance ToJSON TimeRange
instance FromBSON TimeRange
instance ToBSON TimeRange

data Holiday = Holiday { title :: Text
                       , date :: Text
                       , repeatAnnually :: Bool
                       } deriving (Show, Eq, Generic)

instance FromJSON Holiday
instance ToJSON Holiday
instance FromBSON Holiday
instance ToBSON Holiday