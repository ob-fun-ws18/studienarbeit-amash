{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.AppMetrics where

import Data.Text
import Data.Aeson
import GHC.Generics
import GenericBson

data AppMetrics = AppMetrics { downloads :: Integer
                             , totalInstalls :: Maybe Integer
                             , totalUsers :: Maybe Integer
                             } deriving (Show, Eq, Generic)

instance FromJSON AppMetrics
instance ToJSON AppMetrics
instance FromBSON AppMetrics
instance ToBSON AppMetrics