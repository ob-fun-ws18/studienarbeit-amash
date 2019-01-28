# AMASH - Atlassian Marketplace App Statistics (written in) Haskell

The AMASH Fetcher is responsible for fetching data from the Atlassian Marketplace REST API. Such as:

- Rankings / Top 100 apps (for confluence, jira, bitbucket):
    - featured
    - highest-rated
    - popular
    - top-grossing
    - top-vendor
    - trending
- Vendor Data
    - Metadata
    - Contacts
- App Data
    - Metadata
    - Pricing
    - Distribution
    - Reviews
    - Versions

# Testing / Dev environment (vagrant)

To use the testing environment you need to [install vagrant](https://www.vagrantup.com/downloads.html).
Use `vagrant up` at the root level of this repository to spin up a virtual machine exposing a **MongoDB**.
If you want to stop the vm use `vagrant halt`. If you want a new vm use `vagrant destroy`.

### MongoDB Server
The MongoDB server is configured via the file `mongod.conf`.
The script `init-mongo.js` will be executed during the vagrant provision process (first time a vm starts)
and fills the MongoDB with some plugin keys and vendor ids.
It also adds the user `admin` with the password `123` which is needed for authentication.
The server is forwarded to `localhost:27017`.
You can change the port forwarding in the `Vagrantfile`.

### Running the Fetcher

The fetcher needs to be supplied with at least one of the following arguments:
- **"-s"** / **"--setup"** Fetches all existing vendor and app keys and persists them.
- **"-r"** / **"--rankings"** Fetches the current global rankings.
- **"-v"** / **"--vendors"** Fetches the current data of all vendors keys in the database.
- **"-a"** / **"--apps"** Fetches the current data of all app keys in the database.