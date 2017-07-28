{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}


module Generators.Elm where

-- Libs
import Data.Proxy
import Data.Text as T
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
spec = Spec ["Generated"] elmText


elmText :: [Text]
elmText =
    ( defElmImports
    : mkElmTypeEnDecoders (Proxy :: Proxy (Entity User))
    : generateElmForAPI (Proxy :: Proxy Endpoints)
    )


mkElmTypeEnDecoders :: ElmType a => a -> Text
mkElmTypeEnDecoders a = T.unlines
    [ toElmTypeSource a
    , toElmDecoderSource a
    , toElmEncoderSource a
    ]
