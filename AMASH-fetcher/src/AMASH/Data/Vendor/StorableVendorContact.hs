{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AMASH.Data.Vendor.StorableVendorContact where

import qualified AMASH.Data.Vendor.VendorContacts as Unstorable

import Data.Text
import Data.Aeson
import GHC.Generics

data StorableVendorContact = StorableVendorContact { displayName :: Text
                                                   , userId :: Text
                                                   } deriving (Show, Eq, Generic)

vendorContactsToStorable :: Unstorable.VendorContacts -> [StorableVendorContact]
vendorContactsToStorable vendorContacts = Prelude.map toStorableContact (Unstorable.contacts vendorContacts)

toStorableContact :: Unstorable.VendorContact -> StorableVendorContact
toStorableContact contact = StorableVendorContact {
        displayName = Unstorable.displayName contact,
        userId = strip . replace "/users/" "" . Unstorable.href . Unstorable.alternate $ Unstorable._links contact
    }

instance FromJSON StorableVendorContact
instance ToJSON StorableVendorContact