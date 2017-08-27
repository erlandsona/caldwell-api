module Types exposing (..)

-- Libs

import Http
import Navigation exposing (Location)


-- Source

import Server exposing (Gig)


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
    | ShowResponse (Result Http.Error (List Gig))


type HtmlClass
    = Gigs
    | Gig
    | Main
    | Navbar


type Nav
    = Open
    | Closed
