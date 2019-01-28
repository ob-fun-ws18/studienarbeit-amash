-- | A Lib module.
module Lib (square, fetchVendorMetaData, fetchPluginMetaData) where

import System.Environment (getArgs)
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)

import qualified AMASH.Data.App as App
import qualified AMASH.Data.Vendor as Vendor
import qualified AMASH.Data.StorableApp as StorableApp
import qualified AMASH.REST.URIs as URIs

-- TODO: Remove this (just here for benchmark and test example)
-- | Calculate the square of a number.
square :: Num a
    => a -- ^ The number
    -> a -- ^ The square
square n = n^2

-- | Fetches and prints the metadata for a given vendor id (e.g. "1210714")
fetchVendorMetaData :: String -> IO ()
fetchVendorMetaData vendorId = do
    let uri = URIs.vendor vendorId
        getJSON = simpleHttp uri -- TODO: error handling on HTTP code 4xx via try / catch

    e <- (eitherDecode <$> getJSON) :: IO (Either String Vendor.Vendor)

    case e of
        Left err -> putStrLn err
        Right vendorInfo -> do
            putStrLn "\n------------------------------"
            putStrLn $ "Got VendorInfo for: " ++ vendorId
            print vendorInfo
            putStrLn "------------------------------"

-- | Fetches and prints the metadata for a given plugin key (e.g. "de.scandio.confluence.plugins.pocketquery")
fetchPluginMetaData :: String -> IO ()
fetchPluginMetaData pluginKey = do
    let uri = URIs.app pluginKey
        getJSON = simpleHttp uri -- TODO: error handling on HTTP code 4xx via try / catch

    e <- (eitherDecode <$> getJSON) :: IO (Either String App.App)

    case e of
        Left err -> putStrLn err
        Right appInfo -> do
            putStrLn "\n------------------------------"
            putStrLn "Got AppInfo for: "
            print $ App.name appInfo
            putStrLn ""
            print {-$ toEncoding-} $ StorableApp.createStorableApp appInfo
            putStrLn "------------------------------"