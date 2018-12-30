module AMASH.Util where

import AMASH.Constants

showRanking :: Application
            -> AppsListFilter
            -> String
showRanking application category =
    let appName = showApplication application
        catName = showInKebab category
    in "'" ++ appName ++ "/" ++ catName ++ "'"