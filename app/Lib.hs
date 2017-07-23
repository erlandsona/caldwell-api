{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where


-- Libs
import Database.Persist.Postgresql (Entity(..))
import Servant

-- Source
import Models

type Api
    =    Endpoints
    :<|> Raw

type Endpoints
    = "api" :>
    (    "users" :> Get '[JSON] [Entity User]
    :<|> "shows" :> Get '[JSON] [Entity Venue]
    )

