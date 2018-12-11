{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.Config (getIP, getPort, getUser, getPassword) where

import System.Environment
import Data.Maybe
import Database.MongoDB
import Text.Read
import Data.Text

getEnvOrDefault envName defaultValue = do
   maybeEnv <- lookupEnv envName
   return $ maybe defaultValue id maybeEnv

getIP = getEnvOrDefault "AMASH_MONGO_IP" "127.0.0.1"

getPort :: IO PortID
getPort = return $ PortNumber 27017

{-
Nach 1,5h rumfailen geb ich's jetzt mal auf...
https://www.reddit.com/r/haskell/comments/490z3x/newbie_portnumber_from_a_nonliteral/

getPort = do
    maybeString <- lookupEnv "AMASH_MONGO_PORT"
    return $ maybe (PortNumber 27017) (id) fromString maybeString
-}

getUser :: IO Text
getUser = getEnvOrDefault "AMASH_MONGO_USER" "admin" >>= return . pack

getPassword :: IO Text
getPassword = getEnvOrDefault "AMASH_MONGO_PW" "123" >>= return . pack

