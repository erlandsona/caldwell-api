{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module Apis where


-- Libs
import Servant
import Servant.Generic

-- Source
import Models

type Root = ToServant (Endpoints AsApi)

data Endpoints route = Endpoints
    { accounts :: route :- "api" :> "accounts" :> Get '[JSON] [Account]
    , venues :: route :- "api" :> "shows" :> Get '[JSON] [Venue]
    , root :: route :- Raw
    } deriving Generic
