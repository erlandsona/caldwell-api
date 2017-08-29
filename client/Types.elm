module Types exposing (..)

-- Libs

import Http
import Navigation exposing (Location)


-- Source

import Server exposing (Gig)
import Scroll exposing (Move)


type Page
    = Home
    | About
    | Shows
    | Music
    | Contact


type Action
    = GoToPage Location
    | SetUrl Page
    | Toggle Nav
    | ShowResponse (Result Http.Error (List Gig))
      -- the message emitted by the input port
      -- brings a tuple with previous and current scroll values
    | Header Move
      -- message to be sent when scrollTop < 400px
    | Darken
      -- message to be sent when scrollTop > 400px
    | Brighten



-- animation's tick
-- | Animate Animation.Msg


type CssClass page mod
    = Gigs
    | Gig
    | Section
    | Main page
    | NavBar mod


type alias ClassName =
    String


type Nav
    = Open
    | Closed
