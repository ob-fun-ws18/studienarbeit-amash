module AMASH.REST.FetchRecord (
    fetchAppMetrics,
    fetchAppRecommendations,
    fetchVendorMetaData,
    fetchVendorContacts
) where

import qualified AMASH.REST.URIs as URIs
import qualified AMASH.Data.AppMetrics as AppMetrics
import qualified AMASH.Data.Vendor as Vendor
import qualified AMASH.Data.Vendor.VendorContacts as VendorContacts
import qualified AMASH.Data.Vendor.StorableVendorContact as StorableVendorContact
import qualified AMASH.Data.AppsList as AppsList
import AMASH.REST.Rankings
import AMASH.MongoDB

import Data.Aeson
import Data.Text
import Data.Maybe
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)

fetchAppMetrics :: Text -> IO (Maybe AppMetrics.AppMetrics)
fetchAppMetrics appKey = fetchRecord (URIs.appMetrics $ unpack appKey) "app metrics"

fetchAppRecommendations :: Text -> IO (Maybe [Text])
fetchAppRecommendations appKey = do
    maybeRecommendations <- fetchRecord (URIs.appRecommendations $ unpack appKey) "app recommendations" :: IO (Maybe AppsList.AppsListResponse)
    return . fmap AppsList.appsResponseToAppKeys $ maybeRecommendations

fetchVendorMetaData :: Text -> IO (Maybe Vendor.Vendor)
fetchVendorMetaData vendorId = fetchRecord (URIs.vendor $ unpack vendorId) "vendor metadata"

fetchVendorContacts :: Text -> IO (Maybe [StorableVendorContact.StorableVendorContact])
fetchVendorContacts vendorId = do
    maybeContacts <- fetchRecord (URIs.vendorContacts $ unpack vendorId) "vendor contacts" :: IO (Maybe VendorContacts.VendorContacts)
    return . fmap StorableVendorContact.vendorContactsToStorable $ maybeContacts


fetchRecord uri name = do
    let getJSON = simpleHttp uri
    e <- eitherDecode <$> getJSON

    case e of
        Left err -> do
            putStrLn $ "> Failed to fetch " ++ name ++ " because of error: " ++ err
            return Nothing
        Right fetchedData -> do
            putStrLn $ "> Fetched " ++ name ++ "."
            return $ Just fetchedData