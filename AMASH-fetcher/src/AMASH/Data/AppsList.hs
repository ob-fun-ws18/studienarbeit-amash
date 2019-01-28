{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AMASH.Data.AppsList where

import Data.Text
import Data.Aeson
import GHC.Generics

data AppsListResponse = AppsListResponse { count :: Int
                                         , _embedded :: AppsList
                                         } deriving (Show, Eq, Generic)

newtype AppsList = AppsList { addons :: [App] } deriving (Show, Eq, Generic)

data App = App { key :: Text
               , _links :: AppLinks
               } deriving (Show, Eq, Generic)

newtype AppLinks = AppLinks { vendor :: AppVendor } deriving (Show, Eq, Generic)
newtype AppVendor = AppVendor { href :: Text } deriving (Show, Eq, Generic)

instance FromJSON AppsListResponse
instance ToJSON AppsListResponse

instance FromJSON AppsList
instance ToJSON AppsList

instance FromJSON App
instance ToJSON App

instance FromJSON AppLinks
instance ToJSON AppLinks

instance FromJSON AppVendor
instance ToJSON AppVendor

appsResponseToAppKeys :: AppsListResponse -> [Text]
appsResponseToAppKeys appsResponse = Prelude.map (strip . key) (addons $ _embedded appsResponse)

appsResponseToVendorKeys :: AppsListResponse -> [Text]
appsResponseToVendorKeys appsResponse = Prelude.map (strip . replace "/rest/2/vendors/" "" . href . vendor . _links) (addons $ _embedded appsResponse)
