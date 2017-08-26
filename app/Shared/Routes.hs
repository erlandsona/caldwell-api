{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module Routes where

-- Libs
import Servant
import Servant.Generic

-- Source
import Models

data Routes path = Routes
    { accounts :: path :- "api" :> "accounts" :> Get '[JSON] [Account]
    , gigs :: path :- "api" :> "shows" :> Get '[JSON] [Gig]
    , root :: path :- Raw
    } deriving Generic

type Router = ToServant (Routes AsApi)

