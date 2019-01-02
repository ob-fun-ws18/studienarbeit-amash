module AMASH.REST.AppMetrics (fetchAppMetrics) where

import qualified AMASH.REST.URIs as URIs
import qualified AMASH.Data.AppMetrics as AppMetrics
import AMASH.REST.Rankings
import AMASH.MongoDB

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)


fetchAppMetrics :: Text -> IO (Maybe AppMetrics.AppMetrics)
fetchAppMetrics appKey = do
    let getJSON = simpleHttp . URIs.appMetrics $ unpack appKey
    e <- (eitherDecode <$> getJSON) :: IO (Either String AppMetrics.AppMetrics)

    case e of
        Left err -> do
            putStrLn $ "> Failed to fetch app metrics because of error: " ++ err
            return Nothing
        Right vendorMetaData -> do
            putStrLn "> Fetched app metrics."
            return $ Just vendorMetaData