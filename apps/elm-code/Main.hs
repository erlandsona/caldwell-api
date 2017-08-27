{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

-- Libs
import Data.Proxy
import Data.Text as T
import Elm
import Servant.Elm
    ( ElmOptions(..)
    , UrlPrefix(Static)
    , defElmOptions
    , generateElmForAPIWith
    )

-- Source
import Routes
import Models

main :: IO ()
main = do
    -- elmFormat <- createProcess (proc "elm-format" ["--yes", "--stdin"])
    --     { std_in = CreatePipe
    --     , std_out = CreatePipe
    --     }
    -- let formattedSpec = elmFormat $ T.unlines elmText
    specsToDir [spec] "client"



spec :: Spec
spec = Spec ["Server"] elmText

elmText :: [Text]
elmText =
    ( defElmImports
    : mkElmTypeD'Encoders (Proxy :: Proxy Account)
    : mkElmTypeD'Encoders (Proxy :: Proxy Gig)
    : generateElmForAPIWith options (Proxy :: Proxy ApiRouter)
    )


mkElmTypeD'Encoders :: ElmType a => a -> Text
mkElmTypeD'Encoders a = T.intercalate "\n\n\n"
    [ toElmTypeSource a
    , toElmDecoderSource a
    , toElmEncoderSource a
    ]


options :: ElmOptions
options = defElmOptions
    { urlPrefix = Static "http://localhost:3737" }

defElmImports :: Text
defElmImports =
  T.unlines
    [ "import Date exposing (Date(..))"
    , "import Exts.Json.Decode exposing (decodeDate)"
    , "import Http"
    , "import Json.Decode exposing (..)"
    , "import Json.Decode.Pipeline exposing (..)"
    , "import Json.Encode"
    , "import String"
    ]


