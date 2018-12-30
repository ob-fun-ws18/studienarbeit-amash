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
    then if "--setup" `elem` args
         then runFullSetup pipe
         else do
            runQuickSetup pipe
            fetchStuff pipe
    else putStrLn "Authentication failed! Are the credentials set in your ENV correct?"

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
