{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Rankings (saveNewRankings) where

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

saveNewRankings pipe application rankingCategory fetchedRankings = do
    let getLastSavedAction = access pipe master "amash" $ getLastSavedRankings application rankingCategory
        compareWithOldData = compareFetchedAndOldData fetchedRankings "rankings"

    checkResult <- getLastSavedDataAndCompare pipe getLastSavedAction fetchedRankings compareWithOldData

    let rankingsAreEqual    = fst checkResult
        maybeUnchangedSince = snd checkResult :: Maybe UTCTime

    if   rankingsAreEqual && isJust maybeUnchangedSince
    then puteUnchangedSinceIntoDatabase pipe application rankingCategory (fromJust maybeUnchangedSince :: UTCTime)
    else puteNewRankingsIntoDatabase pipe application rankingCategory fetchedRankings

puteNewRankingsIntoDatabase pipe application rankingCategory rankings = do
    currentDateTime <- getCurrentTime
    let selectDocument  = selectApplication application
        pushNewRankings = pushRankings rankingCategory rankings currentDateTime
    access pipe master "amash" $ modify selectDocument pushNewRankings
    putStrLn "Saved new rankings in the database."

puteUnchangedSinceIntoDatabase pipe application rankingCategory unchangedSince = do
    currentDateTime <- getCurrentTime
    let selectDocument  = selectApplication application
        pushNewData     = pushUnchangedSinceRankings rankingCategory currentDateTime unchangedSince
    access pipe master "amash" $ modify selectDocument pushNewData
    putStrLn "Persisted 'unchanged since' pointer in the database."