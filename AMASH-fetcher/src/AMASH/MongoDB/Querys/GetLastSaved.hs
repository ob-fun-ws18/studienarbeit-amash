{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Querys.GetLastSaved where

import AMASH.Constants

import qualified Data.Text as Text
import Data.Time.Clock
import Database.MongoDB
import Control.Monad.IO.Class (MonadIO)


-- | Builds an action that fetches the last saved rankings entry of an application and category.
getLastSavedRankings :: (MonadIO m)
    => Text.Text          -- ^ The application.
    -> Action m [Document]  -- ^ The resulting action.
getLastSavedRankings collectionName = aggregate collectionName [
        ["$sort"   =: ["lastChecked" =: -1]],
        ["$limit"   =: 1]
    ]


getLastSavedVendorEntry :: (MonadIO m)
    => Text.Text            -- ^ The vendor id.
    -> Text.Text            -- ^ The collection name.
    -> Action m [Document]  -- ^ The resulting action.
getLastSavedVendorEntry collectionName vendorId = aggregate collectionName [
        ["$match"   =: [
            "vendor" =: vendorId
        ]],
        ["$sort"   =: ["lastChecked" =: -1]],
        ["$limit"   =: 1]
    ]

-- | Builds an action that fetches the last saved vendor contacts entry of a vendor id.
getLastSavedVendorContacts :: (MonadIO m) => Text.Text -> Action m [Document]
getLastSavedVendorContacts vendorId = getLastSavedVendorEntry "vendor-contacts" vendorId

-- | Builds an action that fetches the last saved vendor metadata entry for a vendor id.
getLastSavedVendorMetaData :: (MonadIO m) => Text.Text -> Action m [Document]
getLastSavedVendorMetaData vendorId = getLastSavedVendorEntry "vendor-metadata" vendorId