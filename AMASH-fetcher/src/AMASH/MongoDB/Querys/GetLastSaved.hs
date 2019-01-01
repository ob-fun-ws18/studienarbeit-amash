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

-- | Builds an action that fetches the last saved contacts entry for a vendor id.
getLastSavedVendorContacts :: (MonadIO m)
    => Text.Text            -- ^ The vendor id.
    -> Action m [Document]  -- ^ The resulting action.
getLastSavedVendorContacts vendorId = aggregate "vendors" [
        ["$match"   =: [
            "key" =: vendorId
        ]],
        ["$unwind"  =: "$contacts"],
        ["$match" =: [
            "contacts.unchangedSince" =: ["$exists" =: False]
        ]],
        ["$limit"   =: 1],
        ["$project" =: [
            "_id"      =: 0,
            "date"     =: "$contacts.date",
            "contacts" =: "$contacts.contacts"
        ]]
    ]

-- | Builds an action that fetches the last saved contacts entry for a vendor id.
getLastSavedVendorMetaData :: (MonadIO m)
    => Text.Text            -- ^ The vendor id.
    -> Action m [Document]  -- ^ The resulting action.
getLastSavedVendorMetaData vendorId = aggregate "vendors" [
        ["$match"   =: [
            "key" =: vendorId
        ]],
        ["$unwind"  =: "$metadata"],
        ["$match" =: [
            "metadata.unchangedSince" =: ["$exists" =: False]
        ]],
        ["$limit"   =: 1],
        ["$project" =: [
            "_id"      =: 0,
            "date"     =: "$metadata.date",
            "metadata" =: "$metadata.metadata"
        ]]
    ]