{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}


module ElmCode where

-- Libs
import Data.Proxy
import Data.Text as T
import Database.Persist.Postgresql (Entity(..))
import Elm
import Servant.Elm
    ( ElmOptions(..)
    , UrlPrefix(Static)
    , defElmOptions
    , generateElmForAPIWith
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
spec = Spec ["Server"] elmText


elmText :: [Text]
elmText =
    ( defElmImports
    : mkElmTypeEnDecoders (Proxy :: Proxy (Entity User))
    : mkElmTypeEnDecoders (Proxy :: Proxy (Entity Venue))
    : generateElmForAPIWith options (Proxy :: Proxy Endpoints)
    )


mkElmTypeEnDecoders :: ElmType a => a -> Text
mkElmTypeEnDecoders a = T.intercalate "\n\n\n"
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
