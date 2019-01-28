{-# LANGUAGE DeriveGeneric #-}

module AMASH.Data.App.AddonCategorySummary where

import Data.Text
import Data.Aeson
import GHC.Generics

newtype AddonCategorySummary = AddonCategorySummary { name :: Text } deriving (Show, Eq, Generic)

instance FromJSON AddonCategorySummary
instance ToJSON AddonCategorySummary
