module Types exposing (..)

import Http
import Navigation exposing (Location)
-- import Time exposing (Time)

import Server exposing (Venue)

type Page
    = Home
    | Music
    | Shows
    | About
    | Contact

type Msg
    = GoToPage Location
    | SetUrl Page
    | Toggle Nav
    | ShowResponse (Result Http.Error (List Venue))

type HtmlClass
    = Gigs
    | Gig

type Nav = Open | Closed

