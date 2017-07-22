{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.Http where

-- Libs
import Elm
    ( Spec (Spec)
    , specsToDir
    , toElmDecoderSource
    , toElmTypeSource
    )
import Database.Persist.Postgresql (Entity(..))
-- import Servant.Elm
--     ( ElmType
--     , Proxy (Proxy)
--     , defElmImports
--     , generateElmForAPI
--     )

-- Source
import Lib
import Models


mkSpecBody :: ElmType a => a -> [Text]
mkSpecBody a =
  [ toElmTypeSource    a
  , toElmDecoderSource a
  , toElmEncoderSource a
  ]


spec :: Spec
spec = Spec ["Http"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy (Entity User))
             : toElmDecoderSource (Proxy :: Proxy (Entity User))
             : toElmTypeSource    (Proxy :: Proxy (Entity Venue))
             : toElmDecoderSource (Proxy :: Proxy (Entity Venue))
             : generateElmForAPI  (Proxy :: Proxy Endpoints))

main :: IO ()
main = specsToDir [spec] "."
