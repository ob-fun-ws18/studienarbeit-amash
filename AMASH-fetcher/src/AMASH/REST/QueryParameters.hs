module AMASH.REST.QueryParameters where

import Data.Char
import Data.List (intersperse)

-- TODO: Das hier kann man prima testen !!!

-- Need a CamelCase to KebabCase converter since we want the Strings of the data
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
                 | JiraServiceDesk
                 | Bitbucket
                 deriving (Eq, Show)

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

data Hosting = Server
             | Cloud
             | DataCenter
             deriving (Eq)

instance Show Hosting where
    show Server = "server"
    show Cloud = "cloud"
    show DataCenter = "dataCenter"

-- TODO: Kategorien sind HTML encodiert... Wie lösen? Gehören die überhaupt hier rein?