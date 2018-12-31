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


-- | Builds an document for pushing new rankings into a given rankingCategory with a timestamp.
pushRankings rankingCategory rankings dateTime =
    let arrayName = Text.pack $ showInKebab rankingCategory
    in pushDocument arrayName ["date" =: dateTime, "rankings" =: rankings]

pushUnchangedSinceRankings rankingCategory dateTime unchangedSince =
    let arrayName = Text.pack $ showInKebab rankingCategory
    in pushUnchangedSince arrayName dateTime unchangedSince

