# AMASH Fetcher

The Fetcher is responsible for obtaining the data from the Atlassian Marketplace REST API.

Currently planned features:
- Get global data (rankings, etc.) from Marketplace
- Get app specific data (pricing, reviews, etc.) from Marketplace

Was holt der Fetcher?
- Rankings / Top 100 Listen von (confluence, jira, bitbucket):
    - featured
    - highest-rated
    - popular
    - top-grossing
    - top-vendor
    - trending
- Info für vendors holen
    - Metadaten
    - Kontakte
- Info für einzelne Apps (keys aus DB gesaugt):
    - Metadaten (Beschreibung, etc)
    - Pricing
    - Distribution
    - Gab es neue Reviews?
    - Versionen, Kompatibilitäten (Gab es ein Update?)
    - Ranking in spezifizierten Kategorien+Product (dafür coole lazy Liste?)
    - Ranking in extra angegebenen queries (z.B. query: "&text=SQL" für PQ)

applicationBuild bei /addons lässt nach apps suchen die mit application build number kompatibel sind
https://marketplace.atlassian.com/rest/2/applications/jira/versions/latest
frage: wie schnell updaten die Anderen ??? könnte man dadurch rausfinden eventuell

Eine Seite hat immer 50 results (limit immer hart auf 50 setzen!!!)

TODO: integrity check on startup: Wurde --dbsetup ausgeführt ?

TODO vielleicht: --dbsetup
    - Fülle top vendors
    - Fülle plugin keys to track
    