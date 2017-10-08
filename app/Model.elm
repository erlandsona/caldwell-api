module Model exposing (..)

import Date exposing (Date)
import Server exposing (Gig)
import Types exposing (..)


type alias Model =
    { history : List Page
    , nav : Nav
    , shows : List Gig
    , today : Date
    }
