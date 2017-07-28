{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Apis where


-- Libs
import Database.Persist.Postgresql (Entity(..))
import Servant

-- Source
import Models

type Root
    =    Endpoints
    :<|> Raw

type Endpoints
    = "api" :>
    (    "users" :> Get '[JSON] [Entity User]
    :<|> "shows" :> Get '[JSON] [Entity Venue]
    )

