module AMASH.REST.Apps (fetchAllExistingKeys) where

import qualified AMASH.REST.URIs as URIs
import qualified AMASH.Data.AppsList as AppsList
import AMASH.Constants

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)

-- | Fetches all existing app and vendor keys and returns them in a tuple.
fetchAllExistingKeys :: IO ([Text], [Text]) -- ^ All app keys and corresponding vendor keys. (vendor keys can be duplicate)
fetchAllExistingKeys = do
    let getJSON = simpleHttp URIs.apps
    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsList.AppsListResponse)

    case e of
        Left err -> do
            putStrLn err
            return ([], [])
        Right response -> do
            let existingApps = show $ AppsList.count response
            putStrLn $ "There are currently " ++ existingApps ++ " apps in the marketplace. Starting to fetch..."
            fetchAllExistingKeys' 0

-- | Fetches all existing app and vendor keys and returns them in a tuple starting from a page number.
fetchAllExistingKeys' :: Integer             -- ^ The page number.
                      -> IO ([Text], [Text]) -- ^ All app keys and corresponding vendor keys. (vendor keys can be duplicate)
fetchAllExistingKeys' page = do
    let getJSON = simpleHttp $ URIs.appsPaged page
    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsList.AppsListResponse)

    case e of
        Left err -> do
            putStrLn err
            return ([], [])
        Right response -> do
            let existingApps     = show $ AppsList.count response
                currentlyFetched = show $ page * resultsPerPage + (fromIntegral $ Prelude.length appKeys)
                appKeys          = AppsList.appsResponseToAppKeys response
                vendorKeys       = AppsList.appsResponseToVendorKeys response

            when (page `mod` 10 == 0) (putStrLn $ "Fetched: " ++ currentlyFetched ++ "/" ++ existingApps)

            if Prelude.length appKeys < fromIntegral resultsPerPage
            then do
                putStrLn $ "Fetched: " ++ existingApps ++ "/" ++ existingApps
                return (appKeys, vendorKeys)
            else do
                nextResult <- fetchAllExistingKeys' (page + 1)
                return $ (appKeys ++ fst nextResult, vendorKeys ++ snd nextResult)
