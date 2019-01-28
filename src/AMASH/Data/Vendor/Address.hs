{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.Vendor.Address where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

data Address = Address { line1 :: Text
                       , line2 :: Maybe Text
                       , city :: Maybe Text
                       , state :: Maybe Text
                       , postCode :: Maybe Text
                       , country :: Maybe Text
                       } deriving (Show, Eq, Generic)

instance FromJSON Address
instance ToJSON Address

instance FromBSON Address
instance ToBSON Address