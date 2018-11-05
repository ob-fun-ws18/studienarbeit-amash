{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (readConfig)
import Network.Wreq
import Control.Lens
import WreqUtil
import MarketplaceURIs
import Data.Aeson.Lens
import Control.Monad (mapM_)

import qualified Data.HashMap.Strict as HM

main :: IO ()
main = readConfig >>= mapM_ getPluginData

-- | Gets and prints the data for a plugin key (e.g. "de.scandio.confluence.plugins.pocketquery")
getPluginData :: [Char] -> IO ()
getPluginData plugin = do
    let uri = MarketplaceURIs.app $ plugin
    r <- get uri
    -- print $ responseGetBody r
    print $ responseIsOkay r
    print $ responseGetStatus r
    -- print $ r ^. responseBody . key "vendorLinks" . key "privacy" . _String
    putStrLn $ "URI: " ++ uri

    let vendorLinks = HM.toList $ r ^. responseBody . key "vendorLinks" . _Object
    print vendorLinks

    let link = (snd $ vendorLinks !! 1) ^. _String
    print link
