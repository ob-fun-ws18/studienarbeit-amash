{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Setup (runQuickSetup, runFullSetup) where

import AMASH.Constants
import AMASH.REST.Apps
import AMASH.MongoDB.Querys

import Database.MongoDB
import Control.Monad.IO.Class
import Data.Char
import Data.List

-- | Returns the count of documents in the rankings database for a given application.
countApplication :: (MonadIO m, Val v)
    => Pipe  -- ^ The pipe used to connect to the database.
    -> v     -- ^ The application.
    -> m Int -- ^ The count.
countApplication pipe application = access pipe master "amash" $ count $ select ["application" =: application] "rankings"

-- | Creates a new document for a given application name.
createApplication :: Pipe   -- ^ The pipe used to connect to the database.
                  -> [Char] -- ^ The application.
                  -> IO ()
createApplication pipe application = do
    access pipe master "amash" $ insert_ "rankings" ["application" =: application]
    putStrLn $ "[DB:rankings] Created new application '" ++ application ++ "'."

-- | Creates a new document for a given application if it doesn't already exist.
createApplicationIfNotExist :: Pipe        -- ^ The pipe used to connect to the database.
                            -> Application -- ^ The application.
                            -> IO ()
createApplicationIfNotExist pipe application = do
    appCount <- countApplication pipe appString

    if appCount == 0
    then createApplication pipe appString
    else putStrLn $ "[DB:rankings] Application '" ++ appString ++ "' already exists."
    where appString = map (toLower) (show application)

-- | Runs the database setup. The database setup currently just creates necessary empty documents in the rankings database.
runQuickSetup :: Pipe  -- ^ The pipe used to connect to the database.
               -> IO ()
runQuickSetup pipe = do
    putStrLn "Starting AMASH Database quick setup..."
    schemaSetup pipe
    putStrLn "AMASH Database quick setup finished."

-- | Creates necessary empty documents in the rankings database.
schemaSetup :: Pipe  -- ^ The pipe used to connect to the database.
             -> IO ()
schemaSetup pipe = do
    createApplicationIfNotExist pipe Confluence
    createApplicationIfNotExist pipe Jira
    createApplicationIfNotExist pipe Bitbucket

-- | Runs the database setup. The database setup currently just creates necessary empty documents in the rankings database.
runFullSetup :: Pipe  -- ^ The pipe used to connect to the database.
              -> IO ()
runFullSetup pipe = do
    putStrLn "[Step 1/2] Create schema"
    schemaSetup pipe
    putStrLn "[Step 2/2] Fill the DB with untracked app and vendor keys"

    savedAppKeys    <- getAllAppKeys    pipe
    putStrLn $ (show $ Prelude.length savedAppKeys) ++ " app keys stored in the DB.."

    savedVendorKeys <- getAllVendorKeys pipe
    putStrLn $ (show $ Prelude.length savedVendorKeys) ++ " vendor keys stored in the DB.."

    allExistingKeys <- fetchAllExistingKeys

    let fetchedAppKeys      = fst allExistingKeys
        fetchedVendorKeys   = nub $ snd allExistingKeys -- vendor keys can be duplicate
        newAppKeys          = fetchedAppKeys \\ savedAppKeys
        newVendorKeys       = fetchedVendorKeys \\ savedVendorKeys
        countNewApps        = show $ Prelude.length newAppKeys
        countNewVendors     = show $ Prelude.length newVendorKeys
        countFetchedApps    = show $ Prelude.length fetchedAppKeys
        countFetchedVendors = show $ Prelude.length fetchedVendorKeys

    putStrLn $ countNewApps ++ "/" ++ countFetchedApps ++ " of the fetched app keys are new."
    insertAppKeys pipe newAppKeys
    putStrLn $ "Saved " ++ countNewApps ++ " new app keys in the database."

    putStrLn $ countNewVendors ++ "/" ++ countFetchedVendors ++ " of the fetched vendor keys are new."
    insertVendorKeys pipe newVendorKeys
    putStrLn $ "Saved " ++ countNewVendors ++ " new vendor keys in the database."