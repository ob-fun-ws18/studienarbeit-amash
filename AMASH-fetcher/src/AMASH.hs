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
import AMASH.Data.Vendor.StorableVendorContact as StorableVendorContact

import qualified Data.Text as Text -- TODO remove after implementing fetchApps
import Database.MongoDB
import Data.Maybe
import Control.Monad (when)

fetchRankings pipe = do
    runQuickSetup pipe
    putStrLn ">>> Fetching rankings."
    mapM_ (fetchAndPersistRanking pipe) rankingAppsAndFilters
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
    mapM_ (fetchAndPersistVendor pipe amountOfVendors) (zip (take 1 vendorKeys) [1..]) -- TODO: remove "take 1"!!!
    putStrLn "----------------------------------------"
    putStrLn ">>> All vendors fetched."

fetchAndPersistVendor :: Pipe -> String -> (Text.Text, Integer) -> IO ()
fetchAndPersistVendor pipe totalVendors (vendorKey, currentVendor) = do
    putStrLn "----------------------------------------"
    putStrLn $ "Fetching vendor '" ++ (Text.unpack vendorKey) ++ "'. (" ++ (show currentVendor) ++ "/" ++ totalVendors ++ ")"

    maybeVendorContacts <- fetchVendorContacts vendorKey
    when (isJust maybeVendorContacts) (persistVendorContacts pipe vendorKey $ fromJust maybeVendorContacts)

    --saveNewRankings pipe application appsListFilter result
    putStrLn ""


fetchApps pipe = putStrLn "Not yet implemented."