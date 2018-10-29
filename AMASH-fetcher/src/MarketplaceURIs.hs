-- | Contains URIs for the Atlassian Marketplace REST API
module MarketplaceURIs (
        appVersions,
        appVersionByBuildNumber,
        appVersionLatest
    ) where

-----------------------------------------------------------------------

-- | Provides the Atlassian Marketplace REST base URI.
baseURI :: [Char] -> [Char]
baseURI path = "https://marketplace.atlassian.com/rest/2" ++ path

-- | Provides the Atlassian Marketplace REST base URI.
appURI :: [Char] -> [Char]
appURI addonKey = baseURI "/addons/" ++ addonKey

-----------------------------------------------------------------------

-- https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-get
appVersions :: [Char] -> [Char]
appVersions addonKey = appURI addonKey ++ "/versions"

-- https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-latest-get
appVersionLatest :: [Char] -> [Char]
appVersionLatest addonKey = appVersions addonKey ++ "/latest"

-- https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-build-pluginBuildNumber-get
appVersionByBuildNumber :: [Char] -> [Char] -> [Char]
appVersionByBuildNumber addonKey buildNumber = appVersions addonKey ++ "/build/" ++ buildNumber

-- https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-name-name-get
appVersionByBuildNumber :: [Char] -> [Char] -> [Char]
appVersionByVersion addonKey version = appVersions addonKey ++ "/name/" ++ version
