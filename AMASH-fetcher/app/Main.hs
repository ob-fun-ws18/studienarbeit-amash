{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)

import Lib (readConfig)

import AMASH.Types
import qualified AMASH.URIs as URIs

main :: IO ()
main = readConfig >>= mapM_ getPluginData

-- | Gets and prints the data for a given plugin key (e.g. "de.scandio.confluence.plugins.pocketquery")
getPluginData :: [Char] -> IO ()
getPluginData plugin = do
    let uri = URIs.app plugin
        getJSON = simpleHttp uri -- TODO: error handling on HTTP code 4xx

    e <- (eitherDecode <$> getJSON) :: IO (Either String AppInfo)

    case e of
        Left err -> putStrLn err
        Right appInfo -> do
            putStrLn "\nGot AppInfo for: " 
            print $ name appInfo
            -- print $ encode appInfo
