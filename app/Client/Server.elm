module Server exposing (..)

import Date exposing (Date(..))
import Exts.Json.Decode exposing (decodeDate)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias User =
    { userId : Maybe Int
    , userFirstName : String
    , userLastName : String
    , userEmail : String
    }


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userId" (maybe int)
        |> required "userFirstName" string
        |> required "userLastName" string
        |> required "userEmail" string


encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "userId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.userId )
        , ( "userFirstName", Json.Encode.string x.userFirstName )
        , ( "userLastName", Json.Encode.string x.userLastName )
        , ( "userEmail", Json.Encode.string x.userEmail )
        ]


type alias Venue =
    { venueId : Maybe Int
    , venueDate : Date
    , venueLocation : String
    }


decodeVenue : Decoder Venue
decodeVenue =
    decode Venue
        |> required "venueId" (maybe int)
        |> required "venueDate" decodeDate
        |> required "venueLocation" string


encodeVenue : Venue -> Json.Encode.Value
encodeVenue x =
    Json.Encode.object
        [ ( "venueId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.venueId )
        , ( "venueDate", (Json.Encode.string << toString) x.venueDate )
        , ( "venueLocation", Json.Encode.string x.venueLocation )
        ]


getApiUsers : Http.Request (List User)
getApiUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3737"
                , "api"
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiShows : Http.Request (List Venue)
getApiShows =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:3737"
                , "api"
                , "shows"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeVenue)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
