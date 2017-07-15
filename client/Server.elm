module Server exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias User =
    { userId : Int
    , firstName : String
    , lastName : String
    , email : String
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userId" int
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string

type alias Gig =
    { showId : Int
    , location : String
    , date : Date
    }

decodeGig : Decoder Gig
decodeGig =
    decode Gig
        |> required "showId" int
        |> required "location" string
        |> required "date" decodeDate

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

getApiShows : Http.Request (List (Gig))
getApiShows =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "localhost:3737/"
                [ ""
                , "api"
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
