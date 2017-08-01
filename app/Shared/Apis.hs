{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module Apis where


-- Libs
import Database.Persist.Postgresql (Entity(..))
import Servant
import Servant.Generic

-- Source
import Models

-- type Root
--     =    Endpoints
--     :<|> Raw

-- type Endpoints
--     = "api" :>
--     (    "users" :> Get '[JSON] [Entity User]
--     :<|> "shows" :> Get '[JSON] [Entity Venue]
--     )

type Root = ToServant (Base AsApi)

data Base route = Base
    { root :: route :- Raw
    , api :: route :- "api" :> ToServant (Api AsApi)
    } deriving Generic

data Api route = Api
    { users  :: route :- "users" :> Get '[JSON] [Entity User]
    , venues :: route :- "shows" :> Get '[JSON] [Entity Venue]
    } deriving Generic
