{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AMASH.MongoDB
import AMASH.REST.Rankings
import AMASH.Constants
import System.Environment

main :: IO ()
main = do
   args <- getArgs
   pipe <- openConnection -- TODO: Catch Exception (connect can fail)
   authenticated <- authenticate pipe

   if authenticated
   then if "--dbsetup" `elem` args
        then runSetup pipe
        else fetchStuff pipe
   else putStrLn "Authentication failed! Are the credentials set in your ENV correct?"

-- Read existing keys from DB and get info for them
fetchAllPlugins pipe = do
    plugins <- getAllPlugins pipe
    mapM_ fetchPluginMetaData plugins

    vendors <- getAllVendors pipe
    mapM_ fetchVendorMetaData vendors

fetchStuff pipe = do
    mapM_ (fetchAndPersist pipe) rankings
    putStrLn "Fetched everything! :)"

fetchAndPersist pipe (application, appsListFilter) = do
    result <- getTop100 application appsListFilter
    putStrLn $ "Successfully fetched " ++ (show $ Prelude.length result) ++ " results."
    saveNewRankings pipe application appsListFilter result
