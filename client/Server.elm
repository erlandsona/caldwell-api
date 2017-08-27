module Server exposing (..)

import Date exposing (Date(..))
import Exts.Json.Decode exposing (decodeDate)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Account =
    { accountFirstName : String
    , accountLastName : String
    , accountEmail : String
    }


decodeAccount : Decoder Account
decodeAccount =
    decode Account
        |> required "accountFirstName" string
        |> required "accountLastName" string
        |> required "accountEmail" string


encodeAccount : Account -> Json.Encode.Value
encodeAccount x =
    Json.Encode.object
        [ ( "accountFirstName", Json.Encode.string x.accountFirstName )
        , ( "accountLastName", Json.Encode.string x.accountLastName )
        , ( "accountEmail", Json.Encode.string x.accountEmail )
        ]


type alias Gig =
    { gigDate : Date
    , gigVenue : String
    }


decodeGig : Decoder Gig
decodeGig =
    decode Gig
        |> required "gigDate" decodeDate
        |> required "gigVenue" string


encodeGig : Gig -> Json.Encode.Value
encodeGig x =
    Json.Encode.object
        [ ( "gigDate", (Json.Encode.string << toString) x.gigDate )
        , ( "gigVenue", Json.Encode.string x.gigVenue )
        ]


getV1Accounts : Http.Request (List Account)
getV1Accounts =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3737"
                , "v1"
                , "accounts"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeAccount)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getV1Shows : Http.Request (List Gig)
getV1Shows =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3737"
                , "v1"
                , "shows"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeGig)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
