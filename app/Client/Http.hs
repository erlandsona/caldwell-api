{-# LANGUAGE OverloadedStrings #-}
module Client.Http where

-- Libs
import Elm
    ( Spec (Spec)
    , specsToDir
    , toElmDecoderSource
    , toElmTypeSource
    )
-- import Servant.API  ((:>), Capture, Get, JSON)
import Servant.Elm
    ( ElmType
    , Proxy (Proxy)
    , defElmImports
    , generateElmForAPI
    )

-- Source
import Lib
import Models


instance ElmType User
instance ElmType Venue

spec :: Spec
spec = Spec ["Http"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy User)
             : toElmDecoderSource (Proxy :: Proxy User)
             : toElmTypeSource    (Proxy :: Proxy Venue)
             : toElmDecoderSource (Proxy :: Proxy Venue)
             : generateElmForAPI  (Proxy :: Proxy Api))

main :: IO ()
main = specsToDir [spec] "."
