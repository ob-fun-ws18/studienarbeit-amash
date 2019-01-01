module AMASH.REST.VendorContacts (fetchVendorContacts) where

import qualified AMASH.REST.URIs as URIs
import qualified AMASH.Data.Vendor.VendorContacts as VendorContacts
import qualified AMASH.Data.Vendor.StorableVendorContact as StorableVendorContact
import AMASH.REST.Rankings
import AMASH.MongoDB

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)

-- | Fetches and prints the metadata for a given vendor id (e.g. "1210714")
fetchVendorContacts :: Text -> IO (Maybe [StorableVendorContact.StorableVendorContact])
fetchVendorContacts vendorId = do
    let getJSON = simpleHttp . URIs.vendorContacts $ unpack vendorId
    e <- (eitherDecode <$> getJSON) :: IO (Either String VendorContacts.VendorContacts)

    case e of
        Left err -> do
            putStrLn $ "> Failed to fetch contacts because of error: " ++ err
            return Nothing
        Right vendorContacts -> do
            putStrLn $ "> Fetched " ++ (show $ VendorContacts.count vendorContacts) ++ " contact(s)."
            return . Just $ StorableVendorContact.vendorContactsToStorable vendorContacts