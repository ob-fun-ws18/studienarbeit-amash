{-# LANGUAGE OverloadedStrings #-}

module AMASH.MongoDB.Setup (runSetup) where

import AMASH.Constants
import AMASH.REST.AppKeys
import AMASH.MongoDB.Querys

import Database.MongoDB
import Control.Monad.IO.Class
import Data.Char
import Data.List

-- | Runs the database setup. The database setup currently just creates necessary empty documents in the rankings database.
runSetup :: Pipe  -- ^ The pipe used to connect to the database.
              -> IO ()
runSetup pipe = do
    putStrLn "Find all existing app keys and their vendors and put untracked keys into the database."

    trackedAppKeys <- getAllTrackedAppKeys pipe
    putStrLn $ show (Prelude.length trackedAppKeys) ++ " app keys stored in the DB.."

    trackedVendorKeys <- getAllTrackedVendorKeys pipe
    putStrLn $ show (Prelude.length trackedVendorKeys) ++ " vendor keys stored in the DB.."

    fetchedKeys <- fetchAllExistingKeys

    let fetchedAppKeys      = fst fetchedKeys
        fetchedVendorKeys   = nub $ snd fetchedKeys -- vendor keys can be duplicate
        newAppKeys          = fetchedAppKeys \\ trackedAppKeys
        newVendorKeys       = fetchedVendorKeys \\ trackedVendorKeys
        countNewApps        = show $ Prelude.length newAppKeys
        countNewVendors     = show $ Prelude.length newVendorKeys
        countFetchedApps    = show $ Prelude.length fetchedAppKeys
        countFetchedVendors = show $ Prelude.length fetchedVendorKeys

    putStrLn $ countNewApps ++ "/" ++ countFetchedApps ++ " of the fetched app keys are new."
    insertTrackedAppKeys pipe newAppKeys
    putStrLn $ "Added " ++ countNewApps ++ " new app keys to the database."

    putStrLn $ countNewVendors ++ "/" ++ countFetchedVendors ++ " of the fetched vendor keys are new."
    insertTrackedVendorKeys pipe newVendorKeys
    putStrLn $ "Added " ++ countNewVendors ++ " new vendor keys to the database."