{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App where

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock

import AMASH.Data.App.VendorLinks
import AMASH.Data.App.AppEmbedded

data App = App { name :: Text
               , key :: Text
               , summary :: Text
               , tagLine :: Text
               , vendorLinks :: VendorLinks
               , _embedded :: AppEmbedded
               , lastModified :: UTCTime
               } deriving (Show, Eq, Generic)

instance FromJSON App
instance ToJSON App
