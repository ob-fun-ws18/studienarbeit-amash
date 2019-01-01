{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module AMASH.MongoDB.Querys.Push where

import AMASH.Constants

import Data.Time.Clock
import Database.MongoDB
import qualified Data.Text as Text

pushUnchangedSince arrayName dateTime unchangedSince = [
    "$push" =: [
       arrayName =: [
           "$position" =: 0,
           "$each" =: [[
               "date" =: dateTime,
               "unchangedSince" =: unchangedSince
           ]]
       ]
    ]]

pushDocument arrayName document = [
    "$push" =: [
        arrayName =: [
            "$position" =: 0,
            "$each" =: [document]
        ]
    ]]


-- | Builds an document for pushing new rankings into a given rankingCategory with a timestamp.
pushVendorContacts dateTime contacts = pushDocument "contacts" ["date" =: dateTime, "contacts" =: contacts]

pushUnchangedSinceVendorContacts dateTime unchangedSince = pushUnchangedSince "contacts" dateTime unchangedSince

