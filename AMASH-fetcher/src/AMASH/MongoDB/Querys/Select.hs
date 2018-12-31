{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Querys.Select where

import AMASH.Constants

import Database.MongoDB
import Database.MongoDB.Query (Select)
import Data.Text

-- | Builds a selection for an application from the rankings database.
selectApplication :: Select aQueryOrSelection
    => Application       -- ^ The application.
    -> aQueryOrSelection -- ^ The resulting selection.
selectApplication application = select [
        "application" =: appName
    ] "rankings" where appName = showApplication application

-- | Builds a selection for a vendor document from the vendors database.
selectVendor :: Select aQueryOrSelection
    => Text              -- ^ The vendor key.
    -> aQueryOrSelection -- ^ The resulting selection.
selectVendor vendorId = select ["key" =: vendorId] "vendors"

