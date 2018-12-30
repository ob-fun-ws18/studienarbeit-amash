{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB where

import qualified Data.Text as Text
import Database.MongoDB
import Data.Time.Clock
import Data.Maybe
import Control.Monad (when)

import AMASH.Util
import AMASH.Constants
import AMASH.MongoDB.Connection
import AMASH.MongoDB.Querys
import AMASH.MongoDB.Setup

-- Exporting functions from nested modules for single import functionality
openConnection = AMASH.MongoDB.Connection.openConnection'
authenticate   = AMASH.MongoDB.Connection.authenticate'
runSetup       = AMASH.MongoDB.Setup.runSetup'

-- | Unpack a BSON Value that holds a String into a String.
unValue :: Value -> String
unValue (String text) = Text.unpack text

getAllKeys pipe collection = do
    bsonValues <- access pipe master "amash" $ find (select [] collection) >>= rest
    return $ map (unValue . valueAt "key") bsonValues

getAllPlugins pipe = getAllKeys pipe "plugins"
getAllVendors pipe = getAllKeys pipe "vendors"

getLatestRanking pipe application rankingCategory = access pipe master "amash" $ getLastSavedRankings application rankingCategory

-- | Checks if rankings are equal to the last saved rankings and returns a tupel of (areEqual, dateOfEqual)
checkIfRankingsAreNew pipe application rankingCategory rankings = do
    bsonDoc <- getLatestRanking pipe application rankingCategory

    if   Prelude.length bsonDoc == 0
    then firstEntryInThisRankings
    else checkIfRankingsAreNew' (bsonDoc !! 0) rankings

-- | Prints out that there are no earlier rankings to compare the new rankings with and returns (True, Nothing).
firstEntryInThisRankings = do
    putStrLn "NEW DATA! - There are no earlier saved rankings to compare the newly fetched rankings with."
    return (True, Nothing)

-- | Compares the rankings within the bsonDoc and the given rankings and returns the result as well as the embedded date.
checkIfRankingsAreNew' bsonDoc rankings = do
    let maybeRankings    = bsonDoc !? "rankings" :: Maybe [Text.Text]
        maybeDate        = bsonDoc !? "date"     :: Maybe UTCTime
        rankingsAreEqual = compareRankings maybeRankings rankings

    when (isNothing maybeDate) (error "Last saved rankings do not have a date. This means the data is corrupted!")
    putStrLn $ "The last saved data for this ranking is from '" ++ (show $ fromJust maybeDate) ++ "'."

    if   rankingsAreEqual
    then putStrLn "UNCHANGED - The last saved rankings are equal with the newly fetched rankings."
    else putStrLn "NEW DATA! - The newly fetched rankings are different from last saved rankings."

    return (rankingsAreEqual, maybeDate)

compareRankings (Just a) b = a == b
compareRankings Nothing _  = False


-- | Save a new ranking for an application/category if they are new. If they are not new save a "unchangedSince" instead.
saveNewRankings pipe application rankingCategory rankings = do
    checkResult <- checkIfRankingsAreNew pipe application rankingCategory rankings
    let rankingsAreEqual    = fst checkResult
        maybeUnchangedSince = snd checkResult :: Maybe UTCTime

    if   rankingsAreEqual && isJust maybeUnchangedSince
    then saveUnchangedSince pipe application rankingCategory (fromJust maybeUnchangedSince :: UTCTime)
    else saveNewRankings'   pipe application rankingCategory rankings

-- | Save a new ranking for an application/category (without checking if they are new).
saveNewRankings' pipe application rankingCategory rankings = do
    currentDateTime <- getCurrentTime

    let selectDocument  = selectApplication application
        pushNewRankings = pushRankings rankingCategory rankings currentDateTime
    access pipe master "amash" $ modify selectDocument pushNewRankings

    let amountOfResults = show $ Prelude.length rankings
        rankingName     = showRanking application rankingCategory
    putStrLn $ "Persisted " ++ amountOfResults ++ " new results for " ++ rankingName ++ " in the DB."

saveUnchangedSince pipe application rankingCategory unchangedSince = do
    currentDateTime <- getCurrentTime

    let selectDocument  = selectApplication application
        pushNewData = pushUnchangedSinceRankings rankingCategory currentDateTime unchangedSince
    access pipe master "amash" $ modify selectDocument pushNewData

    let rankingName     = showRanking application rankingCategory
    putStrLn $ "Persisted 'unchanged since' pointer to '" ++ (show unchangedSince) ++ "'' for " ++ rankingName ++ " in the DB."