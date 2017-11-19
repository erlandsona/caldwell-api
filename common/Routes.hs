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
type Api = "api"

data ApiRoutes path = ApiRoutes
    { accounts :: path :- Api :> Version :> "accounts" :> Get '[JSON] [Account]
    , gigs :: path :- Api :> Version :> "shows" :> Get '[JSON] [Gig]
    } deriving Generic

type ApiRouter = ToServant (ApiRoutes AsApi)
