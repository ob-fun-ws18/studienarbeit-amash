module AMASH (
    fetchRankings,
    fetchVendors,
    fetchApps,

    module AMASH.MongoDB,
    module AMASH.Constants,
    module AMASH.Util,
    module AMASH.REST
) where

import AMASH.MongoDB
import AMASH.Constants
import AMASH.Util
import AMASH.REST

import qualified Data.Text as Text -- TODO remove after implementing fetchApps
import Database.MongoDB
import Data.Maybe
import Control.Monad (when)

fetchRankings pipe = do
    runQuickSetup pipe
    putStrLn ">>> Fetching rankings."
    mapM_ (fetchAndPersistRanking pipe) (take 1 rankingAppsAndFilters) -- TODO: remove "take 1" !!!
    putStrLn "----------------------------------------"
    putStrLn ">>> All rankings fetched."

fetchAndPersistRanking pipe (application, appsListFilter) = do
    putStrLn "----------------------------------------"
    result <- getTop100 application appsListFilter
    putStrLn $ "Successfully fetched " ++ (show $ Prelude.length result) ++ " results."
    saveNewRankings pipe application appsListFilter result


fetchVendors :: Pipe -> IO ()
fetchVendors pipe = do
    vendorKeys <- getAllVendorKeys pipe
    let amountOfVendors = show $ length vendorKeys
    putStrLn $ ">>> Fetching vendors. Found " ++ amountOfVendors ++ " vendor keys in the database."
    putStrLn "----------------------------------------"
    mapM_ (fetchAndPersistVendor pipe amountOfVendors) (zip (take 1 vendorKeys) [1..]) -- TODO: remove "take 1" !!!
    putStrLn ">>> All vendors fetched."

fetchAndPersistVendor :: Pipe -> String -> (Text.Text, Integer) -> IO ()
fetchAndPersistVendor pipe totalVendors (vendorKey, currentVendor) = do
    putStrLn $ "Fetching vendor '" ++ (Text.unpack vendorKey) ++ "'. (" ++ (show currentVendor) ++ "/" ++ totalVendors ++ ")"

    -- TODO: uncomment
    --maybeVendorContacts <- fetchVendorContacts vendorKey
    --when (isJust maybeVendorContacts) (persistVendorContacts pipe vendorKey $ fromJust maybeVendorContacts)

    maybeVendorMetaData <- fetchVendorMetaData vendorKey
    --when (isJust maybeVendorMetaData) (persistVendorContacts pipe vendorKey $ fromJust maybeVendorMetaData)

    putStrLn "----------------------------------------"

fetchApps pipe = putStrLn "Not yet implemented."