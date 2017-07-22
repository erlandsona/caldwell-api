{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where


-- Libs
-- import Database.Persist.Postgresql (Entity(..))
import Servant

-- Source
import Models

type Api
    =    Endpoints
    :<|> Raw

type Endpoints
    = "api" :>
    (    "users" :> Get '[JSON] [User]
    :<|> "shows" :> Get '[JSON] [Venue]
    )

