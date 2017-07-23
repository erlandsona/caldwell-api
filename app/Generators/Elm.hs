{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}


module Generators.Elm where

-- Libs
import Data.Proxy
import Data.Text
import Database.Persist.Postgresql (Entity(..))
import Elm
import Servant.Elm
    ( defElmImports
    , generateElmForAPI
    )

-- Source
import Lib
import Models

spec :: Spec
spec = Spec ["Generated"]
            ( defElmImports
            : toElmTypeSource (Proxy :: Proxy (Entity User))
            : toElmDecoderSource (Proxy :: Proxy (Entity User))
            : toElmEncoderSource (Proxy :: Proxy (Entity User))
            : generateElmForAPI (Proxy :: Proxy Endpoints)
            )


mkElmSource :: ElmType a => a -> [Text]
mkElmSource a =
    [ toElmTypeSource a
    , toElmDecoderSource a
    , toElmEncoderSource a
    ]
