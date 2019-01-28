# AMASH

Atlassian Marketplace App Statistics (written in) Haskell

AMASH consists of 2 individual applications:
- The Fetcher
- The Webserver

Further descriptions will be added.

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