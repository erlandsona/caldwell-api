{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}


module Elm where

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
import Apis
import Models

main :: IO ()
main = do
    -- elmFormat <- createProcess (proc "elm-format" ["--yes", "--stdin"])
    --     { std_in = CreatePipe
    --     , std_out = CreatePipe
    --     }
    -- let formattedSpec = elmFormat $ T.unlines elmText
    specsToDir [spec] "app/Client"



spec :: Spec
spec = Spec ["Generated"] elmText


elmText :: [Text]
elmText =
    ( defElmImports
    : mkElmTypeEnDecoders (Proxy :: Proxy (Entity User))
    : mkElmTypeEnDecoders (Proxy :: Proxy (Entity Venue))
    : generateElmForAPI (Proxy :: Proxy Endpoints)
    )


mkElmTypeEnDecoders :: ElmType a => a -> Text
mkElmTypeEnDecoders a = T.intercalate "\n\n\n"
    [ toElmTypeSource a
    , toElmDecoderSource a
    , toElmEncoderSource a
    ]
