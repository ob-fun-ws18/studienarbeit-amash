{-# LANGUAGE DeriveGeneric #-}

module AMASH.Types.App where

import Data.Text
import Data.Aeson
import GHC.Generics

import AMASH.Types.App.VendorLinks
import AMASH.Types.App.AppEmbedded

data App = App { name :: Text
               , key :: Text
               , summary :: Text
               , tagLine :: Text
               , vendorLinks :: VendorLinks
               , _embedded :: AppEmbedded
               , lastModified :: Text -- format: date-time e.g.: "2018-10-03T00:17:34.548Z"
               } deriving (Show, Eq, Generic)

instance FromJSON App
instance ToJSON App
