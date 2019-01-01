module AMASH.REST.VendorMetaData (fetchVendorMetaData) where

import qualified AMASH.REST.URIs as URIs
import qualified AMASH.Data.Vendor as Vendor
import AMASH.REST.Rankings
import AMASH.MongoDB

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)

-- | Fetches and prints the metadata for a given vendor id (e.g. "1210714")
fetchVendorMetaData :: Text -> IO (Maybe Vendor.Vendor)
fetchVendorMetaData vendorId = do
    let getJSON = simpleHttp . URIs.vendor $ unpack vendorId
    e <- (eitherDecode <$> getJSON) :: IO (Either String Vendor.Vendor)

    case e of
        Left err -> do
            putStrLn $ "> Failed to fetch contacts because of error: " ++ err
            return Nothing
        Right vendorMetaData -> do
            putStrLn "> Fetched vendor metadata."
            return $ Just vendorMetaData