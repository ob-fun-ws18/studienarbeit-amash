{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AMASH.Config
import MongoDB
import AMASH.MongoDB.Setup
import Control.Monad (when)
import System.Environment

main :: IO ()
main = do
   args <- getArgs
   pipe <- connect -- TODO: Catch Exception (connect can fail)
   authenticated <- authenticate pipe

   if authenticated
   then if "--dbsetup" `elem` args
        then runSetup pipe
        else fetchAllPlugins pipe
   else putStrLn "Authentication failed! Are the credentials set in your ENV correct?"

fetchAllPlugins pipe = do
    plugins <- getAllPlugins pipe
    mapM_ fetchPluginMetaData plugins

    vendors <- getAllVendors pipe
    mapM_ fetchVendorMetaData vendors

fetchScandio = fetchVendorMetaData "1210714"
fetchPocketQuery = fetchPluginMetaData "de.scandio.confluence.plugins.pocketquery"