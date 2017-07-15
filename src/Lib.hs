{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib (
    startApp
) where


import Data.Aeson
import Data.Aeson.TH
import Data.Time
import Elm         
    ( Spec (Spec)
    , specsToDir
    , toElmDecoderSource
    , toElmTypeSource
    )
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
-- import Servant.API  ((:>), Capture, Get, JSON)
import Servant.Elm
    ( ElmType
    , Proxy (Proxy)
    , defElmImports
    , generateElmForAPI
    )

data User = User
    { userId        :: Int
    , firstName :: String
    , lastName  :: String
    , email     :: String
    } deriving (Eq, Generic, Show)

data Gig = Gig
    { showId        :: Int
    , location  :: String
    , date      :: UTCTime
    } deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Gig)

instance ElmType User
instance ElmType Gig

type API = "api" :>
    (    "users" :> Get '[JSON] [User]
    :<|> "shows" :> Get '[JSON] [Gig]
    )


startApp :: IO ()
startApp = run 7777 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server
    =    return users
    :<|> return gigs

users :: [User]
users = [ User 1 "Austin" "Erlandson" "austin@erlandson.com"
        , User 2 "Emily" "Kroll" "emilykrollmusic@yahoo.com"
        ]

gigs :: [Gig]
gigs =
    [ Gig 1 "Austin's Birthday!" (UTCTime (fromGregorian (2017 :: Integer) 7 21) 0)
    ]
