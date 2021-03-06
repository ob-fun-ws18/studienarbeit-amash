module AMASH.Constants where

import Data.Char
import Data.List (intersperse)

resultsPerPage :: Integer
resultsPerPage = 25

validProgramArgs = ["-s", "--setup", "-a", "--apps", "-v", "--vendors", "-r", "--rankings"]

showInKebab :: Show a => a -> String
showInKebab = toKebabCase . show

-- Need a CamelCase to KebabCase converter for the AppsListFilters
toKebabCase []     = []
toKebabCase [x]    = [toLower x]
toKebabCase (x:xs) = toLower x : toKebabCase' xs

toKebabCase' []     = []
toKebabCase' [x]    = [toLower x]
toKebabCase' (x:xs) = if isUpper x
                      then '-' : toLower x : toKebabCase' xs
                      else x : toKebabCase' xs

data Application = Confluence
                 | Jira
                 | Bitbucket
                 deriving (Eq, Show)

showApplication :: Application -> String
showApplication application = map toLower (show application)

data AppsListFilter = Atlassian
                    | Codegeist
                    | Featured
                    | HighestRated
                    | Name
                    | New
                    | Popular
                    | Recent
                    | TopGrossing
                    | TopVendor
                    | Trending
                    | Verified
                    deriving (Eq, Show)

filtersRelevantForRankings :: [AppsListFilter]
filtersRelevantForRankings = [Featured, HighestRated, Popular, TopGrossing, TopVendor, Trending]

rankingAppsAndFilters = [(app, filter) | app <- [Confluence, Jira, Bitbucket], filter <- filtersRelevantForRankings]

data Hosting = Server
             | Cloud
             | DataCenter
             deriving (Eq)

instance Show Hosting where
    show Server = "server"
    show Cloud = "cloud"
    show DataCenter = "dataCenter"
