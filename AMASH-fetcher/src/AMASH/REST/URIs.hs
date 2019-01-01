-- | Contains / Generates URIs for the Atlassian Marketplace REST API.
-- | All of these URIs are for GET calls.
module AMASH.REST.URIs (
        baseURI,

        app,
        appVersions,
        appVersionLatest,
        appVersionByBuildNumber,
        appVersionByVersion,
        appDistribution,
        appPricing,
        appRecommendations,
        appReviews,

        apps,
        appsPaged,
        buildRankingURI,

        vendor,
        vendorContacts,
        vendorAppsPaged,
        vendorArchivedAppsPaged
    ) where

import AMASH.Constants

-----------------------------------------------------------------------

-- | The Atlassian Marketplace REST base URI.
baseURI :: String 
baseURI = "https://marketplace.atlassian.com/rest/2"

-----------------------------------------------------------------------

-- | Provides the Atlassian Marketplace REST base URI.
-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-get
app :: String  -> String 
app ""       = error "Empty addon key not allowed for URI buildung!"
app addonKey = baseURI ++ "/addons/" ++ addonKey

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-get
appVersions :: String  -> String 
appVersions addonKey = app addonKey ++ "/versions"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-latest-get
appVersionLatest :: String  -> String 
appVersionLatest addonKey = appVersions addonKey ++ "/latest"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-build-pluginBuildNumber-get
-- | Build Number die man aus anderen calls kriegt, also z.B. "3005000020"
appVersionByBuildNumber :: String  -> String  -> String 
appVersionByBuildNumber addonKey buildNumber = appVersions addonKey ++ "/build/" ++ buildNumber

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-name-name-get
-- | Tatsächliche Version also z.B. "3.6.1"
appVersionByVersion :: String  -> String  -> String 
appVersionByVersion addonKey version = appVersions addonKey ++ "/name/" ++ version

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-distribution-get
appDistribution :: String  -> String 
appDistribution addonKey = app addonKey ++ "/distribution"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-pricing-cloudOrServer-liveOrPending-get
appPricing, appPricing' :: String  -> String  -> String 
appPricing addonKey "cloud"      = appPricing' addonKey "cloud"
appPricing addonKey "datacenter" = appPricing' addonKey "datacenter"
appPricing addonKey "server"     = appPricing' addonKey "server"
appPricing addonKey pricingType  = error $ "Illegal pricing type \"" ++ pricingType ++ "\"! Allowed are only: [\"cloud\", \"datacenter\", \"server\"]."

appPricing' addonKey pricingType = app addonKey ++ "/pricing/" ++ pricingType ++ "/live"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-recommendations-get
-- | The other apps that appear in the "recommendations" panel below your app.
appRecommendations :: String  -> String 
appRecommendations addonKey = app addonKey ++ "/recommendations"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-reviews-get
appReviews :: String  -> String 
appReviews addonKey = app addonKey ++ "/reviews"

-----------------------------------------------------------------------

apps :: String
apps = baseURI ++ "/addons"

appsPaged :: Integer -> String
appsPaged page = baseURI ++ "/addons?" ++ pageParams page

buildRankingURI :: Application -> AppsListFilter -> Integer -> String
buildRankingURI application appsListFilter page = apps
                                        ++ "?application=" ++ (showApplication application)
                                        ++ "&filter=" ++ (showInKebab appsListFilter)
                                        ++ "&" ++ pageParams page

-----------------------------------------------------------------------

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-vendors-vendorId-get
vendor :: String -> String
vendor vendorId = baseURI ++ "/vendors/" ++ vendorId

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-vendors-vendorId-contacts-get
vendorContacts :: String -> String
vendorContacts vendorId = vendor vendorId ++ "/contacts"

vendorApps :: String -> String
vendorApps vendorId = baseURI ++ "/addons/vendor/" ++ vendorId

vendorAppsPaged :: String -> Integer -> String
vendorAppsPaged vendorId page = (vendorApps vendorId) ++ "?" ++ (pageParams page)

vendorArchivedApps :: String -> String
vendorArchivedApps vendorId = baseURI ++ "/addons/archived/vendor/" ++ vendorId

vendorArchivedAppsPaged :: String -> Integer -> String
vendorArchivedAppsPaged vendorId page = (vendorArchivedApps vendorId) ++ "?" ++ (pageParams page)

-----------------------------------------------------------------------

pageParams :: Integer -> String
pageParams page = "limit=" ++ (show resultsPerPage) ++ "&offset=" ++ (show $ page * resultsPerPage)