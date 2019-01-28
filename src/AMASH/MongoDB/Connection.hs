{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Connection (openConnection, authenticate) where

import qualified Data.Text as Text
import Database.MongoDB

import qualified AMASH.Config as Config

-- | Connect to the database specified in the environment variables. Can fail due to connection errors.
openConnection :: IO Pipe -- ^ The pipe used to communicate with the database.
openConnection = do
    ip <- Config.getIP
    port <- Config.getPort
    connect $ Host ip port

-- | Authenticate at the database using the credentials specified in the environment variables.
authenticate :: Pipe    -- ^ The pipe used to communicate with the database.
              -> IO Bool -- ^ Whether the authentication was successful.
authenticate pipe = do
    user <- Config.getUser
    pw <- Config.getPassword
    access pipe master "admin" $ auth user pw

