{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Rankings (persistRankings) where

import qualified Data.Text as Text
import Database.MongoDB
import Data.Time.Clock
import Data.Maybe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import GenericBson

import AMASH.Util
import AMASH.Constants
import AMASH.MongoDB.Connection
import AMASH.MongoDB.Querys
import AMASH.MongoDB.Setup
import AMASH.MongoDB.Helpers

persistRankings pipe application rankingCategory fetchedRankings = do
    let collectionName       = Text.pack $ rankingsCollectionName application rankingCategory
        getLastSavedAction   = access pipe master "amash" $ getLastSavedRankings collectionName
        compareWithOldDataFn = compareFetchedAndOldData fetchedRankings "rankings"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction compareWithOldDataFn

    let areEqual      = fst checkResult
        maybeObjectId = snd checkResult :: Maybe ObjectId

    if   areEqual && isJust maybeObjectId
    then updateLastChangedTimestamp pipe collectionName (fromJust maybeObjectId :: ObjectId)
    else putNewDataIntoDatabase pipe collectionName fetchedRankings

putNewDataIntoDatabase pipe collectionName rankings = do
    currentDateTime <- getCurrentTime
    let docToPersist = [ "fetched"     =: currentDateTime
                       , "lastChecked" =: currentDateTime
                       , "rankings"    =: rankings]
    access pipe master "amash" $ insert_ collectionName docToPersist
    putStrLn "Saved new data in the database."

updateLastChangedTimestamp pipe collectionName objectId = do
    currentDateTime <- getCurrentTime

    let selectDocument    = selectByObjectId collectionName objectId
        updateLastChecked = ["$set" =: ["lastChecked" =: currentDateTime]]

    access pipe master "amash" $ modify selectDocument updateLastChecked
    putStrLn $ "Updated lastChecked of old entry to '" ++ show currentDateTime ++ "'."
