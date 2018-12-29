{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-} -- TODO: remove

module Main where

import Lib
import AMASH.Config
import MongoTest
import qualified AMASH.URIs as URIs -- TODO: Remove ab hier
import Network.HTTP.Conduit (simpleHttp) -- TODO: REMOVE

import Data.Text
import Data.Aeson
import GHC.Generics
import Data.Time.Clock

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

    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsResponse)

    case e of
        Left err -> return []
        Right appList -> do
                             let fetchedApps = Prelude.map key (addons $ _embedded appList)
                                 allApps = apps ++ fetchedApps

                             putStrLn "fetched Apps!: "
                             print fetchedApps
                             putStrLn $ "Total fetched Apps: " ++ (show $ Prelude.length allApps)


                             if Prelude.length fetchedApps < 10 -- TODO: oder page == maxResults && /= 0
                             then return $ allApps
                             else fetchPage (page+1) allApps



data AppsResponse = AppsResponse { count :: Int
                                 , _embedded :: AppsList
                                 } deriving (Show, Eq, Generic)

newtype AppsList = AppsList { addons :: [App] } deriving (Show, Eq, Generic)

newtype App = App { key :: Text } deriving (Show, Eq, Generic)

instance FromJSON AppsResponse
instance ToJSON AppsResponse

instance FromJSON AppsList
instance ToJSON AppsList

instance FromJSON App
instance ToJSON App