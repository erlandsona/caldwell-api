{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
import Elm
import Elm.Export.Persist.Entity ()
import GHC.Generics

-- DB Models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbAccount
  firstName Text
  lastName Text
  email Text
  UniqueEmail email
  deriving Show Generic

DbVenue
    date UTCTime
    location Text
    deriving Show Generic
|]

data Account = Account
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    } deriving (Eq, Show, Generic)

convertDbAccount :: DbAccount -> Account
convertDbAccount DbAccount{..} = Account
    { firstName = dbAccountFirstName
    , lastName = dbAccountLastName
    , email = dbAccountEmail
    }

instance ToJSON Account
instance FromJSON Account
instance ElmType Account

data Venue = Venue
    { date :: UTCTime
    , location :: Text
    } deriving (Eq, Show, Generic)

convertDbVenue :: DbVenue -> Venue
convertDbVenue DbVenue{..} = Venue
    { date = dbVenueDate
    , location = dbVenueLocation
    }

instance ToJSON Venue
instance FromJSON Venue
instance ElmType Venue
