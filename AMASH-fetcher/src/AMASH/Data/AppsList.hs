{-# LANGUAGE DeriveGeneric #-} -- TODO: remove

module AMASH.Data.AppsList where

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock -- FÜR UTCTime ? TODO: Remove if not needed (it probably isnt)

data AppsListResponse = AppsListResponse { count :: Int
                                         , _embedded :: AppsList
                                         } deriving (Show, Eq, Generic)

newtype AppsList = AppsList { addons :: [App] } deriving (Show, Eq, Generic)

newtype App = App { key :: Text } deriving (Show, Eq, Generic)

instance FromJSON AppsListResponse
instance ToJSON AppsListResponse

instance FromJSON AppsList
instance ToJSON AppsList

instance FromJSON App
instance ToJSON App

appsResponseToTextList :: AppsListResponse -> [Text]
appsResponseToTextList appsResponse = Prelude.map key (addons $ _embedded appsResponse)

