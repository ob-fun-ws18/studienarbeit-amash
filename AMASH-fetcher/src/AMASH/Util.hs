module AMASH.Util where

import AMASH.Constants

-- TODO: write tests for this!
showRanking :: Application
            -> AppsListFilter
            -> String
showRanking application category =
    let appName = showApplication application
        catName = showInKebab category
    in "'" ++ appName ++ "/" ++ catName ++ "'"

rankingsCollectionName application rankingCategory = "rankings-" ++ (showApplication application) ++ "-" ++ (showInKebab rankingCategory)

-- TODO: write tests for this!
-- | Given two lists tests if at least one element of the first list is also an element of the second list.
elemAtLeastOne :: (Eq a)
               => [a]  -- ^ The first list.
               -> [a]  -- ^ The second list against which the elements of the first list are tested.
               -> Bool -- ^ Whether at least one element of the first list is also an element of the second list.
elemAtLeastOne maybeElems list = or $ map (\x -> x `elem` list) maybeElems