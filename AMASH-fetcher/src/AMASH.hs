{-# LANGUAGE OverloadedStrings #-} -- TODO: remove

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
    putStrLn ">>> Fetching rankings."
    mapM_ (fetchAndPersistRanking pipe) rankingAppsAndFilters --(take 1 rankingAppsAndFilters) -- TODO: remove "take 1" !!!
    putStrLn "----------------------------------------"
    putStrLn ">>> All rankings fetched."

fetchAndPersistRanking pipe (application, appsListFilter) = do
    putStrLn "----------------------------------------"
    result <- getTop100 application appsListFilter
    putStrLn $ "Successfully fetched " ++ (show $ Prelude.length result) ++ " results."
    saveNewRankings pipe application appsListFilter result


fetchVendors :: Pipe -> IO ()
fetchVendors pipe = do
    trackedVendorKeys <- getAllTrackedVendorKeys pipe
    let amountOfVendors = show $ length trackedVendorKeys
    putStrLn $ ">>> Fetching vendors. Found " ++ amountOfVendors ++ " vendor keys in the database."
    putStrLn "----------------------------------------"
    mapM_ (fetchAndPersistVendor pipe amountOfVendors) (zip trackedVendorKeys [1..])
    --mapM_ (fetchAndPersistVendor pipe amountOfVendors) (zip ["111"] [1..]) -- TODO remove (only for testing)
    putStrLn ">>> All vendors fetched."

fetchAndPersistVendor :: Pipe -> String -> (Text.Text, Integer) -> IO ()
fetchAndPersistVendor pipe totalVendors (vendorKey, currentVendor) = do
    putStrLn $ ">> Fetching vendor '" ++ (Text.unpack vendorKey) ++ "'. (" ++ (show currentVendor) ++ "/" ++ totalVendors ++ ")"

    maybeVendorMetaData <- fetchVendorMetaData vendorKey
    when (isJust maybeVendorMetaData) (persistVendorMetaData pipe vendorKey $ fromJust maybeVendorMetaData)

    maybeVendorContacts <- fetchVendorContacts vendorKey
    when (isJust maybeVendorContacts) (persistVendorContacts pipe vendorKey $ fromJust maybeVendorContacts)

    maybeVendorApps <- fetchVendorApps vendorKey
    --when (isJust maybeVendorApps) (persistVendorApps pipe vendorKey $ fromJust maybeVendorApps)

    maybeVendorArchivedApps <- fetchVendorApps vendorKey
    --when (isJust maybeVendorApps) (persistVendorApps pipe vendorKey $ fromJust maybeVendorApps)

    putStrLn "----------------------------------------"


fetchApps :: Pipe -> IO ()
fetchApps pipe = do
    trackedVendorApps <- getAllTrackedAppKeys pipe
    let amountOfApps = show $ length trackedVendorApps
    putStrLn $ ">>> Fetching apps. Found " ++ amountOfApps ++ " app keys in the database."
    putStrLn "----------------------------------------"
    --mapM_ (fetchAndPersistApp pipe amountOfApps) (zip trackedVendorKeys [1..])
    mapM_ (fetchAndPersistApp pipe amountOfApps) (zip ["de.scandio.confluence.plugins.pocketquery"] [1..]) -- TODO remove (only for testing)
    putStrLn ">>> All apps fetched."

fetchAndPersistApp :: Pipe -> String -> (Text.Text, Integer) -> IO ()
fetchAndPersistApp pipe totalVendors (vendorKey, currentVendor) = do
    putStrLn $ ">> Fetching app '" ++ (Text.unpack vendorKey) ++ "'. (" ++ (show currentVendor) ++ "/" ++ totalVendors ++ ")"

    -- Collections to do:
    -- - app-metadata
    -- - app-recommendations
    -- - app-pricing
    -- - app-metrics

    --maybeVendorMetaData <- fetchVendorMetaData vendorKey
    --when (isJust maybeVendorMetaData) (persistVendorMetaData pipe vendorKey $ fromJust maybeVendorMetaData)

    putStrLn "----------------------------------------"