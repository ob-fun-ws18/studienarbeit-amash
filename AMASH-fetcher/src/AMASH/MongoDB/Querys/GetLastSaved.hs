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
    => Application          -- ^ The application.
    -> AppsListFilter       -- ^ The filter / category.
    -> Action m [Document]  -- ^ The resulting action.
getLastSavedRankings application rankingCategory = aggregate "rankings" [
        ["$match"   =: [
            "application" =: appName
        ]],
        ["$unwind"  =: arrayName'],
        ["$match" =: [
            Text.pack (arrayName ++ ".unchangedSince") =: ["$exists" =: False]
        ]],
        ["$limit"   =: 1],
        ["$project" =: [
            "_id"      =: 0,
            "date"     =: arrayName' ++ ".date",
            "rankings" =: arrayName' ++ ".rankings"
        ]]
    ] where arrayName  = showInKebab rankingCategory
            arrayName' = "$" ++ arrayName
            appName    = showApplication application

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