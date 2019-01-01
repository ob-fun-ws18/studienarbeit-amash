{-# LANGUAGE OverloadedStrings #-}

module Main where

import AMASH
import Control.Monad (when)
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    let missingArgsMessage = "Missing arg! Supply at least one of the following args to the program: " ++ show validProgramArgs
    when (not $ validProgramArgs `elemAtLeastOne` args) (error missingArgsMessage)

    pipe <- openConnection
    authenticated <- authenticate pipe

    if authenticated
    then do
        when (["--setup",    "-s"] `elemAtLeastOne` args) (runSetup pipe)
        when (["--rankings", "-r"] `elemAtLeastOne` args) (fetchRankings pipe)
        when (["--vendors",  "-v"] `elemAtLeastOne` args) (fetchVendors pipe)
        when (["--apps",     "-a"] `elemAtLeastOne` args) (fetchApps pipe)
        putStrLn "Successfully finished. Exiting."
    else putStrLn "Authentication failed! Are the credentials set in your ENV correct?"