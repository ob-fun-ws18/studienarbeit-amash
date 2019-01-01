module AMASH.REST.VendorApps (fetchVendorApps, fetchVendorArchivedApps) where

import AMASH.Util
import AMASH.Constants
import AMASH.REST.URIs (vendorAppsPaged, vendorArchivedAppsPaged)
import AMASH.Data.AppsList

import Network.HTTP.Conduit (simpleHttp)
import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Monad (liftM2, when)

fetchVendorApps :: Text -> IO [Text]
fetchVendorApps vendorId = do
    vendorApps <- fetchVendorApps' (vendorAppsPaged $ unpack vendorId) 0
    putStrLn $ "> Fetched " ++ show (Prelude.length vendorApps) ++ " apps for vendor."
    return vendorApps

fetchVendorArchivedApps :: Text -> IO [Text]
fetchVendorArchivedApps vendorId = do
    vendorApps <- fetchVendorApps' (vendorArchivedAppsPaged $ unpack vendorId) 0
    putStrLn $ "> Fetched " ++ show (Prelude.length vendorApps) ++ " archived apps for vendor."
    return vendorApps

fetchVendorApps' pagedUri currentPage = do
    let uri = pagedUri currentPage
        getJSON = simpleHttp uri -- TODO: error handling on HTTP code 4xx via try / catch

    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsListResponse)

    case e of
        Left err -> do
            putStrLn $ "> Failed to fetch contacts because of error: " ++ err
            return []
        Right appsResponse -> do
            let apps = appsResponseToAppKeys appsResponse
                fetchedAmount = fromIntegral $ Prelude.length apps
                fetchNextPage = fetchVendorApps' pagedUri (currentPage+1)

            if fetchedAmount < resultsPerPage
            then return apps
            else liftM2 (++) (return apps) fetchNextPage