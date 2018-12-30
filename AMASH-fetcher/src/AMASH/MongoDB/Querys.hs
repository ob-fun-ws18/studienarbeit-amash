{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Querys where

import AMASH.Constants

import qualified Data.Text as Text
import Data.Time.Clock
import Database.MongoDB
import Database.MongoDB.Query (Select)
import Control.Monad.IO.Class (MonadIO)

-- | Builds an action that fetches the last saved rankings entry in for an application and category.
--   The fetched ranking can either directly contain "rankings" or have a reference to an "unchangedSince" object.
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

-- | Builds a selection for an application from the rankings database.
selectApplication :: Select aQueryOrSelection
    => Application       -- ^ The application.
    -> aQueryOrSelection -- ^ The resulting selection.
selectApplication application = select [
        "application" =: appName
    ] "rankings" where appName = showApplication application


-- | Builds an document for pushing new rankings into a given rankingCategory with a timestamp.
pushRankings rankingCategory rankings dateTime = [
    "$push" =: [
        arrayName =: [
            "$position" =: 0,
            "$each" =: [[
                "date" =: dateTime,
                "rankings" =: rankings
            ]]
        ]
    ]] where arrayName = Text.pack $ showInKebab rankingCategory


pushUnchangedSinceRankings rankingCategory dateTime unchangedSince = [
    "$push" =: [
       arrayName =: [
           "$position" =: 0,
           "$each" =: [[
               "date" =: dateTime,
               "unchangedSince" =: unchangedSince
           ]]
       ]
    ]] where arrayName = Text.pack $ showInKebab rankingCategory