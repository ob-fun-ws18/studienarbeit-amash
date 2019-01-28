{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.Config (getIP, getPort, getUser, getPassword) where

import System.Environment
import Data.Maybe
import Database.MongoDB
import Text.Read
import Data.Text

-- | Get the value of a environment variable or a default value instead if it doesn't exist.
getEnvOrDefault :: String -- ^ The name of the environment variable
                -> String -- ^ The default value
                -> IO String -- ^ Either the value of the environment variable or the default value
getEnvOrDefault envName defaultValue = do
   maybeEnv <- lookupEnv envName
   return $ fromMaybe defaultValue maybeEnv

-- | Returns the IP used to connect to the AMASH MongoDB database.
getIP :: IO String -- ^ The IP read from the env var "AMASH_MONGO_IP" or "127.0.0.1" if the var is empty.
getIP = getEnvOrDefault "AMASH_MONGO_IP" "127.0.0.1"

-- | Returns the Port used to connect to the AMASH MongoDB database.
getPort :: IO PortID -- ^ The Port read from the env var "AMASH_MONGO_PORT" or "27017" if the var is empty.
getPort = do
    maybeString <- lookupEnv "AMASH_MONGO_PORT"
    let maybeInt = maybe (Just 27017) readMaybe maybeString
    return $ PortNumber (fromIntegral $ fromMaybe 27017 maybeInt)

-- | Returns the User used to authenticate at the AMASH MongoDB database.
getUser :: IO Text -- ^ The User read from the env var "AMASH_MONGO_USER" or "admin" if the var is empty.
getUser = pack <$> getEnvOrDefault "AMASH_MONGO_USER" "admin"

-- | Returns the Password used to authenticate at the AMASH MongoDB database.
getPassword :: IO Text -- ^ The IP read from the env var "AMASH_MONGO_PW" or "123" if the var is empty.
getPassword = pack <$> getEnvOrDefault "AMASH_MONGO_PW" "123"

