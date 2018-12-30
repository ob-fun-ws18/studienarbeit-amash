{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AMASH.MongoDB
import AMASH.REST.Rankings
import AMASH.Constants

main :: IO ()
main = do
   pipe <- openConnection -- TODO: Catch Exception (connect can fail)
   authenticated <- authenticate pipe

   if authenticated
   then do
        runSetup pipe
        fetchStuff pipe
   else putStrLn "Authentication failed! Are the credentials set in your ENV correct?"

-- Read existing keys from DB and get info for them
fetchAllPlugins pipe = do
    plugins <- getAllPlugins pipe
    mapM_ fetchPluginMetaData plugins

    vendors <- getAllVendors pipe
    mapM_ fetchVendorMetaData vendors

fetchStuff pipe = do
    putStrLn ">>> Fetching rankings."
    mapM_ (fetchAndPersist pipe) rankingAppsAndFilters
    putStrLn "----------------------------------------"
    putStrLn ">>> All rankings fetched."

fetchAndPersist pipe (application, appsListFilter) = do
    putStrLn "----------------------------------------"
    result <- getTop100 application appsListFilter
    putStrLn $ "Successfully fetched " ++ (show $ Prelude.length result) ++ " results."
    saveNewRankings pipe application appsListFilter result
