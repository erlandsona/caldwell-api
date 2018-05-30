{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

-- Libs
import Database.Persist (Entity)
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
    { allAccounts :: path :- Version :> "accounts" :> Get '[JSON] [Entity Account]
    , allShows :: path :- Version :> "shows" :> Get '[JSON] [Entity Gig]
    , createShow :: path :- Version :> "shows" :> ReqBody '[JSON] Gig :> Post '[JSON] (Entity Gig)
    , deleteShow :: path :- Version :> "shows" :> Capture "id" (Key Gig) :> DeleteNoContent '[JSON] NoContent
    } deriving Generic

type ApiRouter = ToServant (ApiRoutes AsApi)
