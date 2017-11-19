{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Config
import Models
import Routes

main :: IO ()
main = do
    -- elmFormat <- createProcess (proc "elm-format" ["--yes", "--stdin"])
    --     { std_in = CreatePipe
    --     , std_out = CreatePipe
    --     }
    -- let formattedSpec = elmFormat $ T.unlines elmText

    env <- lookupSetting "ENV" Development

    let options :: ElmOptions
        options =
            case env of
                Production -> defElmOptions { urlPrefix = Static "https://api.caldwell.band" }
                _ -> defElmOptions

    let elmText :: [Text]
        elmText =
            ( defElmImports
            : mkElmTypeD'Encoders (Proxy :: Proxy Account)
            : mkElmTypeD'Encoders (Proxy :: Proxy Gig)
            : generateElmForAPIWith options (Proxy :: Proxy ApiRouter)
            )

    let spec :: Spec
        spec = Spec ["Server"] elmText

    specsToDir [spec] "app"



mkElmTypeD'Encoders :: ElmType a => a -> Text
mkElmTypeD'Encoders a = T.intercalate "\n\n\n"
    [ toElmTypeSource a
    , toElmDecoderSource a
    , toElmEncoderSource a
    ]


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


