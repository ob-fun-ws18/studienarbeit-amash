{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import WreqUtil
import MarketplaceURIs
import Data.Aeson.Lens

import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    let uri = MarketplaceURIs.app "de.scandio.confluence.plugins.pocketquery"
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
