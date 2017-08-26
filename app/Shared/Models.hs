{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where


import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

-- DB Models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account json
    firstName Text
    lastName Text
    email Text
    UniqueEmail email
    -- friends [Account]
    deriving Show Generic

Gig json
    date UTCTime
    venueId VenueId
    deriving Eq Show

Venue json
    name Text
    -- Consider adding for Maps or something.
    -- street Text
    -- city Text
    -- state Text
    -- zip Integer
    deriving Show


 -- By default this file is used in Model.hs (which is imported by Foundation.hs)
|]
