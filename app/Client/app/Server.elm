module Server exposing (..)

import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (..)
-- import Json.Encode
import Http
import String

type alias User =
    { userId : Int
    , firstName : String
    , lastName : String
    , email : String
    }


type alias Venue =
    { venueId : Int
    , date : Date
    , location : String
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userId" int
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string


decodeVenue : Decoder Venue
decodeVenue =
    decode Venue
        |> required "venueId" int
        |> required "date" date
        |> required "location" string

getApiUsers : Http.Request (List (User))
getApiUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "localhost:3737/"
                [ ""
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

getApiShows : Http.Request (List (Venue))
getApiShows =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            , Http.header "Content-Type" "application/json"
            ]
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
