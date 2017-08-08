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
    , venues :: path :- "api" :> "shows" :> Get '[JSON] [Venue]
    , root :: path :- Raw
    } deriving Generic
