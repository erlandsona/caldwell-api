{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Libs
import Elm
    ( Spec (Spec)
    , specsToDir
    , toElmDecoderSource
    , toElmTypeSource
    )
-- import Servant.API  ((:>), Capture, Get, JSON)
import Servant.Elm
    ( Proxy (Proxy)
    , defElmImports
    , generateElmForAPI
    )

-- Source
import Lib
import Types


spec :: Spec
spec = Spec ["Server"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy User)
             : toElmDecoderSource (Proxy :: Proxy User)
             : toElmTypeSource    (Proxy :: Proxy Venue)
             : toElmDecoderSource (Proxy :: Proxy Venue)
             : generateElmForAPI  (Proxy :: Proxy Api))

main :: IO ()
main = specsToDir [spec] "client"
