{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module Apis where


-- Libs
import Servant
import Servant.Generic

-- Source
import Models

-- type Root
--     =    Endpoints
--     :<|> Raw

-- type Endpoints
--     = "api" :>
--     (    "accounts" :> Get '[JSON] [Entity User]
--     :<|> "shows" :> Get '[JSON] [Entity Venue]
--     )

type Root = ToServant (Endpoints AsApi)

data Endpoints route = Endpoints
    -- { root :: route :- Raw
    { accounts :: route :- "api" :> "accounts" :> Get '[JSON] [Account]
    , venues :: route :- "api" :> "shows" :> Get '[JSON] [Venue]
    } deriving Generic
