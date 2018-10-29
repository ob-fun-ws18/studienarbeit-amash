-- | Contains URIs for the Atlassian Marketplace REST API
module MarketplaceURIs (
        app,
        appVersions,
        appVersionLatest,
        appVersionByBuildNumber,
        appVersionByVersion
    ) where

-----------------------------------------------------------------------

-- | Provides the Atlassian Marketplace REST base URI.
baseURI :: [Char] -> [Char]
baseURI path = "https://marketplace.atlassian.com/rest/2" ++ path

-----------------------------------------------------------------------

-- | Provides the Atlassian Marketplace REST base URI.
-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-get
app :: [Char] -> [Char]
app addonKey = baseURI "/addons/" ++ addonKey

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-get
appVersions :: [Char] -> [Char]
appVersions addonKey = app addonKey ++ "/versions"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-latest-get
appVersionLatest :: [Char] -> [Char]
appVersionLatest addonKey = appVersions addonKey ++ "/latest"

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-build-pluginBuildNumber-get
-- | Build Number die man aus anderen calls kriegt, also z.B. "3005000020"
appVersionByBuildNumber :: [Char] -> [Char] -> [Char]
appVersionByBuildNumber addonKey buildNumber = appVersions addonKey ++ "/build/" ++ buildNumber

-- | https://developer.atlassian.com/platform/marketplace/rest/#api-addons-addonKey-versions-name-name-get
-- | Tatsächliche Version also z.B. "3.6.1"
appVersionByVersion :: [Char] -> [Char] -> [Char]
appVersionByVersion addonKey version = appVersions addonKey ++ "/name/" ++ version
