{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AMASH.Config
import MongoTest

-- main = readConfig >>= mapM_ fetchPluginMetaData

main :: IO ()
main = do
   pipe <- connect -- TODO: Catch Exception (connect can fail)
   authenticated <- authenticate pipe

   if authenticated
   then fetchAllPlugins pipe
   else putStrLn "Authentication failed!"

fetchAllPlugins pipe = do
    plugins <- getAllPlugins pipe
    mapM_ fetchPluginMetaData plugins

fetchScandio = fetchVendorMetaData "1210714"
fetchPocketQuery = fetchPluginMetaData "de.scandio.confluence.plugins.pocketquery"