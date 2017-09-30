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
    { api :: path :- ApiRouter
    , root :: path :- Raw
    } deriving Generic

type Router = ToServant (Routes AsApi)

type ApiVersion = "v1"
data ApiRoutes path = ApiRoutes
    { accounts :: path :- ApiVersion :> "accounts" :> Get '[JSON] [Account]
    , gigs :: path :- ApiVersion :> "shows" :> Get '[JSON] [Gig]
    } deriving Generic

type ApiRouter = ToServant (ApiRoutes AsApi)
