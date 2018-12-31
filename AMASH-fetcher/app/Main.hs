{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AMASH.MongoDB
import AMASH.REST
import AMASH.Constants
import AMASH.Util

import Control.Monad (when)
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    let missingArgsMessage = "Missing arg! Supply at least one of the following args to the program: " ++ show validProgramArgs
    when (not $ validProgramArgs `elemAtLeastOne` args) (error missingArgsMessage)

    pipe <- openConnection -- TODO: Catch Exception (connect can fail)
    authenticated <- authenticate pipe

    if authenticated
    then do
        when (["--setup",    "-s"] `elemAtLeastOne` args) (runFullSetup  pipe)
        when (["--rankings", "-r"] `elemAtLeastOne` args) (fetchRankings pipe)
        when (["--vendors",  "-v"] `elemAtLeastOne` args) (fetchVendors  pipe)
        when (["--apps",     "-a"] `elemAtLeastOne` args) (fetchApps     pipe)
    else putStrLn "Authentication failed! Are the credentials set in your ENV correct?"

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

notYetImplemented  = putStrLn "Not yet implemented."
fetchVendors  pipe = notYetImplemented
fetchApps     pipe = notYetImplemented