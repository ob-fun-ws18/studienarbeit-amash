module AMASH.REST.Rankings where

import AMASH.REST.QueryParameters
import AMASH.REST.URIs (buildRankingURI)
import AMASH.Data.AppsList
import Network.HTTP.Conduit (simpleHttp)
import Data.Text
import Data.Aeson
import GHC.Generics
import Control.Monad (liftM2)

-- TODO: Später sollte product, AppsListFilter und Hosting alles Maybe werden.
getTop100 :: Application -> AppsListFilter -> IO [Text]
getTop100 product appsListFilter =
    let uri = buildRankingURI product appsListFilter
    in getRanking uri 100

getRanking uri maxResults = getRanking' uri maxResults 0

getRanking' uri maxResults currentPage = do
    let getRanking'' = getRanking' uri maxResults
        pagedUri = uri ++ "&offset=" ++ (show $ currentPage * 10)
        getJSON = simpleHttp pagedUri -- TODO: error handling on HTTP code 4xx via try / catch

    e <- (eitherDecode <$> getJSON) :: IO (Either String AppsListResponse)

    case e of
        Left err -> return []
        Right appsResponse -> do
            let apps = appsResponseToTextList appsResponse

            putStrLn "fetched Apps!: "
            print apps

            if currentPage == 10 || Prelude.length apps < 10
            then return $ apps
            else liftM2 (++) (return apps) (getRanking'' (currentPage+1))