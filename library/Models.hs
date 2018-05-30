{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where


import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH
    ( persistLowerCase
    , share
    , mkPersist
    , sqlSettings
    , mkMigrate
    )
import GHC.Generics (Generic)

-- DB Models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account json
    firstName Text
    lastName Text
    email Text
    UniqueEmail email
    deriving Generic Show

Gig json
    date UTCTime
    venue Text
    -- venueId VenueId
    deriving Generic Show

-- Venue json
--     name Text
--     locationId LocationId
--     deriving Show

-- Location json
--     -- Consider adding for Maps or something.
--     -- Maybe needs it's own model like Location
--     street Text
--     city Text
--     state Text
--     zip Integer
--     deriving Show
|]
