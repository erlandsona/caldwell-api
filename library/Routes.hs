{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

-- Libs
import Servant
import Servant.Generic

-- Source
import Models


-- Abstraction over Content Types
data Routes path = Routes
    { api :: path :- ApiRouter
    } deriving Generic

type Router = ToServant (Routes AsApi)

type Version = "v1"

data ApiRoutes path = ApiRoutes
    { accounts :: path :- Version :> "accounts" :> Get '[JSON] [Account]
    , gigs :: path :- Version :> "shows" :> Get '[JSON] [Gig]
    } deriving Generic

type ApiRouter = ToServant (ApiRoutes AsApi)
