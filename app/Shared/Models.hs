{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Models where


import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
import Elm
import Elm.Export.Persist.Entity ()
import GHC.Generics

import qualified Database as Db

data Account = Account
    { firstName :: Text
    , lastName :: Text
    , email :: Text
    } deriving
    ( Eq
    , Show
    , Generic
    , ToJSON
    , FromJSON
    , ElmType
    )

convertDbAccount :: Db.Account -> Account
convertDbAccount Db.Account{..} = Account
    { firstName = accountFirstName
    , lastName = accountLastName
    , email = accountEmail
    }

data Venue = Venue
    { date :: UTCTime
    , location :: Text
    } deriving
    ( Eq
    , Show
    , Generic
    , ToJSON
    , FromJSON
    , ElmType
    )

convertDbVenue :: Db.Venue -> Venue
convertDbVenue Db.Venue{..} = Venue
    { date = venueDate
    , location = venueLocation
    }

