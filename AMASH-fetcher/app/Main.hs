{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = readConfig >>= mapM_ fetchPluginMetaData

fetchScandio = fetchVendorMetaData "1210714"
fetchPocketQuery = fetchPluginMetaData "de.scandio.confluence.plugins.pocketquery"