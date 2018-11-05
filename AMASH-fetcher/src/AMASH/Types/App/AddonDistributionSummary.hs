{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AMASH.Types.App.AddonDistributionSummary where

import Data.Text
import Data.Aeson
import GHC.Generics

data AddonDistributionSummary = AddonDistributionSummary { downloads :: Integer
                                                         , totalInstalls :: Maybe Integer
                                                         , totalUsers :: Maybe Integer
                                                         } deriving (Show, Eq, Generic)

instance FromJSON AddonDistributionSummary
instance ToJSON AddonDistributionSummary
