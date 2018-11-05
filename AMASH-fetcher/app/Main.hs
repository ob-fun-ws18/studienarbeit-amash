{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import WreqUtil
import MarketplaceURIs
import Data.Aeson.Lens
import Control.Monad (mapM_)
import System.Environment (getArgs)

import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    plugins <- readConfig
    mapM_ getPluginData plugins

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

-- | Read config (plugin keys) either from args or from the file "plugins.config" if there are no args
-- | TODO: Read config from DB instead.
readConfig :: IO [String]
readConfig = do
    args <- getArgs
    if length args > 0
    then return args
    else readFile "plugins.config" >>= return . lines