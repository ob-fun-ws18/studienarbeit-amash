{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AMASH.Data.Vendor.VendorContacts where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

data VendorContacts = VendorContacts { count :: Integer
                                     , contacts :: [VendorContact]
                                     } deriving (Show, Eq, Generic)

data VendorContact = VendorContact { displayName :: Text
                                   , _links :: VendorContactLinks
                                   } deriving (Show, Eq, Generic)

newtype VendorContactLinks = VendorContactLinks { alternate :: VendorContactLinksAlternate } deriving (Show, Eq, Generic)

newtype VendorContactLinksAlternate = VendorContactLinksAlternate { href :: Text } deriving (Show, Eq, Generic)

instance FromJSON VendorContacts
instance ToJSON VendorContacts
instance FromBSON VendorContacts
instance ToBSON VendorContacts

instance FromJSON VendorContact
instance ToJSON VendorContact
instance FromBSON VendorContact
instance ToBSON VendorContact

instance FromJSON VendorContactLinks
instance ToJSON VendorContactLinks
instance FromBSON VendorContactLinks
instance ToBSON VendorContactLinks

instance FromJSON VendorContactLinksAlternate
instance ToJSON VendorContactLinksAlternate
instance FromBSON VendorContactLinksAlternate
instance ToBSON VendorContactLinksAlternate