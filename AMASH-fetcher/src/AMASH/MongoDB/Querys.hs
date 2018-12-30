{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Querys where

import AMASH.Constants

import qualified Data.Text as Text
import Data.Time.Clock
import Database.MongoDB
import Database.MongoDB.Query (Select)
import Control.Monad.IO.Class (MonadIO)

-- | Builds an action that fetches the latest rankings entry in for an application/category.
--   The fetched ranking can either directly contain "rankings" or have a reference to an "unchangedSince" object.
getLatestRanking :: (MonadIO m)
    => Application          -- ^ The application.
    -> AppsListFilter       -- ^ The filter / category.
    -> Action m [Document]  -- ^ The resulting action.
getLatestRanking application rankingCategory = aggregate "rankings" [
        ["$match"   =: [
            "application" =: appName
        ]],
        ["$unwind"  =: arrayName],
        ["$limit"   =: 1],
        ["$project" =: [
            "date"     =: arrayName ++ ".date",
            "rankings" =: arrayName ++ ".rankings"
        ]]
    ] where arrayName = "$" ++ showInKebab rankingCategory
            appName   = showApplication application

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