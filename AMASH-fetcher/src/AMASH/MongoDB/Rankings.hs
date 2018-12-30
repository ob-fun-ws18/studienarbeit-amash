{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Rankings (saveNewRankings') where

import qualified Data.Text as Text
import Database.MongoDB
import Data.Time.Clock
import Data.Maybe
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)

import AMASH.Util
import AMASH.Constants
import AMASH.MongoDB.Connection
import AMASH.MongoDB.Querys
import AMASH.MongoDB.Setup


-- | Gets the last saved ranking in the database for a given application and ranking category. (Finds first entry with actual rankings).
-- TODO: Implement reading date from "unchangedSince" and searching for it instead of solving it via aggregation -> performance.
getLatestRanking :: MonadIO m
                 => Pipe           -- ^ The pipe to communicate with the database.
                 -> Application    -- ^ The application.
                 -> AppsListFilter -- ^ The AppsListFilter / Ranking category.
                 -> m [Document]   -- ^ List of results.
getLatestRanking pipe application rankingCategory = access pipe master "amash" $ getLastSavedRankings application rankingCategory


-- | Checks if rankings are equal to the last saved rankings (by fetching them) and returns a tuple of (areEqual, dateOfEqual).
checkIfRankingsAreNew :: Pipe                     -- ^ The pipe to communicate with the database.
                      -> Application              -- ^ The application.
                      -> AppsListFilter           -- ^ The AppsListFilter / Ranking category.
                      -> [Text.Text]              -- ^ The newly fetched rankings.
                      -> IO (Bool, Maybe UTCTime) -- ^ Whether the rankings are equal + the date of the entry being equal if exists.
checkIfRankingsAreNew pipe application rankingCategory rankings = do
    bsonDoc <- getLatestRanking pipe application rankingCategory

    if   Prelude.length bsonDoc == 0
    then firstEntryInThisRankings
    else checkIfRankingsAreNew' (bsonDoc !! 0) rankings


-- | Prints out that there are no earlier rankings to compare the new rankings with. Utility function for "checkIfRankingsAreNew".
firstEntryInThisRankings :: IO (Bool, Maybe a) -- ^ (True, Nothing).
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


-- | Compare two rankings against each other.
compareRankings :: Maybe [Text.Text] -- ^ The old rankings (coming from DB -> wrapped in Maybe)
                -> [Text.Text]       -- ^ The new rankings (freshly fetched from the Marketplace API)
                -> Bool              -- ^ Whether or not the rankings are equal
compareRankings (Just a) b = a == b
compareRankings Nothing _  = False


-- | Save a new ranking for an application/category if they are new. If they are not new save a "unchangedSince" instead.
saveNewRankings' pipe application rankingCategory rankings = do
    checkResult <- checkIfRankingsAreNew pipe application rankingCategory rankings
    let rankingsAreEqual    = fst checkResult
        maybeUnchangedSince = snd checkResult :: Maybe UTCTime

    if   rankingsAreEqual && isJust maybeUnchangedSince
    then saveUnchangedSinceRanking pipe application rankingCategory (fromJust maybeUnchangedSince :: UTCTime)
    else saveNewRankingsHard       pipe application rankingCategory rankings


-- | Save a new ranking for an application / ranking category (without checking if they are new).
saveNewRankingsHard pipe application rankingCategory rankings = do
    currentDateTime <- getCurrentTime

    let selectDocument  = selectApplication application
        pushNewRankings = pushRankings rankingCategory rankings currentDateTime
    access pipe master "amash" $ modify selectDocument pushNewRankings

    let amountOfResults = show $ Prelude.length rankings
        rankingName     = showRanking application rankingCategory
    putStrLn $ "Persisted " ++ amountOfResults ++ " new results for " ++ rankingName ++ " in the DB."


-- | Save a new 'unchanged since' reference to the database for a given application / ranking category.
saveUnchangedSinceRanking pipe application rankingCategory unchangedSince = do
    currentDateTime <- getCurrentTime

    let selectDocument  = selectApplication application
        pushNewData = pushUnchangedSinceRankings rankingCategory currentDateTime unchangedSince
    access pipe master "amash" $ modify selectDocument pushNewData

    let rankingName     = showRanking application rankingCategory
    putStrLn $ "Persisted 'unchanged since' pointer to '" ++ (show unchangedSince) ++ "'' for " ++ rankingName ++ " in the DB."