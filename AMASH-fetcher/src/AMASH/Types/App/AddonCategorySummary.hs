{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AMASH.Types.App.AddonCategorySummary where

import Data.Text
import Data.Aeson
import GHC.Generics

data AddonCategorySummary = AddonCategorySummary { name :: Text } deriving (Show, Eq, Generic)

instance FromJSON AddonCategorySummary
instance ToJSON AddonCategorySummary
