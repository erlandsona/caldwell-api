{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , Api
    ) where


-- Libs
import Data.Aeson
import Data.Aeson.TH
import Data.Time
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
    ( cors
    , corsRequestHeaders
    , simpleCorsResourcePolicy
    )
import Servant
import Servant.Elm (ElmType)

-- Source
import Types

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Venue)

instance ElmType User
instance ElmType Venue

type Api = "api" :>
    (    "users" :> Get '[JSON] [User]
    :<|> "shows" :> Get '[JSON] [Venue]
    )


startApp :: IO ()
startApp = run 3737 app


app :: Application
app = corsWithContentType $ serve api server
    where
        corsWithContentType :: Middleware
        corsWithContentType = cors (const $ Just policy)
            where
              policy = simpleCorsResourcePolicy
                { corsRequestHeaders =
                    [ "Content-Type"
                    , "Access-Control-Allow-Origin"
                    ]
                }

api :: Proxy Api
api = Proxy

server :: Server Api
server
    =    return users
    :<|> return venues

users :: [User]
users = [ User 1 "Austin" "Erlandson" "austin@erlandson.com"
        , User 2 "Emily" "Kroll" "emilykrollmusic@yahoo.com"
        ]

venues :: [Venue]
venues =
    [ Venue 1 (UTCTime (fromGregorian (2017 :: Integer) 7 21) 0) "Austin's Birthday!"
    ]
