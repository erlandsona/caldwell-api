{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

-- Libs
import Servant
import Servant.Generic

-- Source
import Models


data Routes path = Routes
    { api :: path :- Api :> Version :> ApiRouter
    , root :: path :- Raw
    } deriving Generic

type Router = ToServant (Routes AsApi)



type Version = "v1"
type Api = "api"

data ApiRoutes path = ApiRoutes
    { accounts :: path :- "accounts" :> Get '[JSON] [Account]
    , gigs :: path :- "shows" :> Get '[JSON] [Gig]
    } deriving Generic

type ApiRouter = ToServant (ApiRoutes AsApi)
