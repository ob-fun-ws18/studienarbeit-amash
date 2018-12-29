{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import AMASH.Config
import MongoTest

import qualified AMASH.URIs as URIs -- TODO: Remove ab hier für fetchPage
import Network.HTTP.Conduit (simpleHttp)
import Data.Text
import Data.Aeson
import GHC.Generics
import AMASH.Data.AppsList

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

    vendors <- getAllVendors pipe
    mapM_ fetchVendorMetaData vendors

fetchScandio = fetchVendorMetaData "1210714"
fetchPocketQuery = fetchPluginMetaData "de.scandio.confluence.plugins.pocketquery"

-- | Fetches and prints the metadata for a given vendor id (e.g. "1210714")
fetchPage :: Int -> [Text] -> IO [Text]
fetchPage page apps = do
    let uri = URIs.newApps page
        getJSON = simpleHttp uri -- TODO: error handling on HTTP code 4xx via try / catch

    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsListResponse)

    case e of
        Left err -> return []
        Right appsResponse -> do
            let fetchedApps = appsResponseToTextList appsResponse
                allApps = apps ++ fetchedApps

            putStrLn "fetched Apps!: "
            print fetchedApps
            putStrLn $ "Total fetched Apps: " ++ (show $ Prelude.length allApps)

            if Prelude.length fetchedApps < 10 -- TODO: oder page == maxResults && /= 0
            then return $ allApps
            else fetchPage (page+1) allApps