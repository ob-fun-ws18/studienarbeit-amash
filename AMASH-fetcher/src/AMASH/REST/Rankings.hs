module AMASH.REST.Rankings (getTop100) where

import AMASH.Util
import AMASH.Constants
import AMASH.REST.URIs (buildRankingURI)
import AMASH.Data.AppsList

import Network.HTTP.Conduit (simpleHttp)
import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Monad (liftM2, when)

getTop100 :: Application -> AppsListFilter -> IO [Text]
getTop100 application appsListFilter = do
    putStrLn $ "Fetching rankings for " ++ showRanking application appsListFilter ++ "."
    let uriBuilder = buildRankingURI application appsListFilter
    getRanking uriBuilder 100

getRanking :: (Integer -> String) -> Integer -> IO [Text]
getRanking uriBuilder maxResults = getRanking' uriBuilder maxResults 0
getRanking' uriBuilder maxResults currentPage = do
    let uri = uriBuilder currentPage
        getJSON = simpleHttp uri

    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsListResponse)

    case e of
        Left err -> return []
        Right appsResponse -> do
            let apps = appsResponseToAppKeys appsResponse
                lastAmount = fromIntegral $ currentPage * resultsPerPage
                fetchedAmount = fromIntegral $ Prelude.length apps
                totalAmount = lastAmount + fetchedAmount
                fetchNextPage = getRanking' uriBuilder maxResults (currentPage+1)

            putStrLn $ "Fetched: " ++ show totalAmount ++ "/" ++ show maxResults

            when (fetchedAmount < resultsPerPage) (putStrLn "Fetched amount was less than page size -> Reached max results.")

            if totalAmount >= maxResults || fetchedAmount < resultsPerPage
            then return apps
            else Prelude.take (fromIntegral maxResults) <$> liftM2 (++) (return apps) fetchNextPage