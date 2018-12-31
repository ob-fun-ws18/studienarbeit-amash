{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Helpers (
    thereAreNoOlderEntries,
    getLastSavedDataAndCompare,
    compareFetchedAndOldData,
    eqMaybe
) where

import Database.MongoDB
import Data.Time.Clock
import Data.Maybe
import Control.Monad (when)

-- | Prints out that there are no earlier rankings to compare the new rankings with. Utility function for "checkIfRankingsAreNew".
thereAreNoOlderEntries :: IO (Bool, Maybe a) -- ^ (True, Nothing).
thereAreNoOlderEntries = do
    putStrLn "NEW DATA! - There aren't any old entries in the database to compare the fetched data with."
    return (True, Nothing)

-- | Fetches the last saved data and (if it exists) calls the function that compares it with the new data.
getLastSavedDataAndCompare pipe getLastSavedAction maybeNewData compareWithOldDataFn = do
    bsonDoc <- access pipe master "amash" getLastSavedAction

    if   Prelude.length bsonDoc == 0
    then thereAreNoOlderEntries
    else compareWithOldDataFn $ bsonDoc !! 0

-- | Compares fetchedData with last saved data. Needs attributeName to extract data from document.
compareFetchedAndOldData fetchedData attributeName lastSavedData = do
    let maybeOldData = lastSavedData !? attributeName
        maybeDate    = lastSavedData !? "date"     :: Maybe UTCTime
        dataIsEqual  = maybeOldData `eqMaybe` fetchedData

    when (isNothing maybeDate) (error "Last saved data does not have a date. This means the data is corrupted!")
    putStrLn $ "The last saved data is from '" ++ (show $ fromJust maybeDate) ++ "'."

    if   dataIsEqual
    then putStrLn "UNCHANGED - The last saved data is equal to the newly fetched data."
    else putStrLn "NEW DATA! - The newly fetched data differs from the last saved data."

    return (dataIsEqual, maybeDate)

-- | Compares something wrapped in a Maybe with another value and returns false if it doesn't exist.
eqMaybe :: Eq a => Maybe a -> a -> Bool
eqMaybe (Just a) b = a == b
eqMaybe Nothing _  = False