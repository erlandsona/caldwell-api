module Site exposing (main)

-- Libs

import Scroll
import Css exposing (num, opacity)
import Css.Helpers
import Debug
import Date exposing (Date)
import Date.Extra.Compare exposing (is, Compare2(..))
import Http
import Html exposing (Html, header, node, span, text)
import Html.Events exposing (onClick)
import Html.CssHelpers exposing (withNamespace)
import Json.Decode exposing (decodeValue, int, keyValuePairs, maybe, string)
import Json.Encode exposing (Value)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (unpack)
import Navigation as Nav exposing (programWithFlags, Location)
import String exposing (toLower)
import Time exposing (Time)
import UrlParser as Url exposing (oneOf, s)


-- Modules

import Constants exposing (..)
import Main.View as Main
import Nav.View as Nav
import Model exposing (Model)
import Ports exposing (..)
import Server exposing (Gig, getApiV1Shows, decodeGig)
import Types exposing (..)


-- Source


type alias Initializer =
    { cachedGigs : Maybe Value
    , now : Time
    }


decodedGigs : Value -> List Gig
decodedGigs json =
    case (decodeValue (decodeGig |> keyValuePairs) json) of
        Ok shows ->
            List.map (\( msg, location ) -> (Debug.log msg) location) shows

        Err msg ->
            Debug.log msg []


decodeLocalStorageGigs : Maybe Value -> List Gig
decodeLocalStorageGigs =
    unpack (\() -> Debug.log "No shows in local storage." []) decodedGigs


filterAndSort : Date -> List Gig -> List Gig
filterAndSort today =
    List.filter (.gigDate >> flip (is SameOrAfter) today)
        >> List.sortBy (.gigDate >> Date.toTime)


init : Initializer -> Location -> ( Model, Cmd Action )
init { cachedGigs, now } location =
    let
        today =
            Date.fromTime now

        gigs =
            (decodeLocalStorageGigs cachedGigs)

        _ =
            List.map (Debug.log "Gig Date:" << .gigDate) gigs

        _ =
            Debug.log "Today:" today

        model =
            { history = [ parse location ]
            , nav = Closed
            , today = today
            , shows = filterAndSort today gigs
            }
    in
        model
            ! [ locationToScroll location snapIntoView
              , Http.send ShowResponse getApiV1Shows
              ]


locationToScroll : Location -> (String -> Cmd action) -> Cmd action
locationToScroll location scrollFunction =
    location
        |> parse
        |> Main
        |> Css.Helpers.identifierToString homepage
        |> (++) "."
        |> scrollFunction


parse : Location -> Page
parse location =
    withDefault Home (Url.parsePath urlParser location)


urlParser : Url.Parser (Page -> a) a
urlParser =
    oneOf
        [ Url.map Shows (s "shows")
        , Url.map About (s "about")
        , Url.map Music (s "music")
        , Url.map Contact (s "contact")
        ]


{ id, class, classList } =
    withNamespace homepage


view : Model -> Html Action
view model =
    node container
        [ onClick (Toggle Closed)
        ]
        [ node caldwellBackground [] []
        , node blackOverlay
            (case List.head model.history of
                Just Home ->
                    []

                _ ->
                    [ styles [ opacity (num 0.9) ] ]
            )
            []
        , header [ onClick (SetUrl Home) ]
            [ span [] [ text "C" ]
            , text "aldwell"
            ]
        , Nav.template model.nav
        , Main.template model.nav model.shows
        ]


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        GoToPage location ->
            { model | history = (::) (parse location) model.history }
                ! [ locationToScroll location easeIntoView
                  ]

        SetUrl url ->
            model
                ! [ Nav.newUrl <|
                        if url == Home then
                            "/"
                        else
                            url |> toString |> toLower
                  ]

        Toggle newState ->
            { model | nav = newState } ! []

        ShowResponse response ->
            case response of
                Ok shows ->
                    let
                        _ =
                            List.map (Debug.log "Gig Date:" << .gigDate) shows
                    in
                        { model | shows = filterAndSort model.today shows }
                            ! []

                Err msg ->
                    let
                        _ =
                            Debug.log "Shows Reponse" <| toString msg
                    in
                        model ! []

        Darken ->
            { model | history = (::) About model.history } ! []

        Brighten ->
            { model | history = (::) Home model.history } ! []

        Header move ->
            Scroll.handle
                -- when scrollTop > 10px, send Darken message
                [ Scroll.onCrossDown 300 <| update Darken
                  -- when scrollTop < 10px, send Brighten message
                , Scroll.onCrossUp 300 <| update Brighten
                ]
                move
                model



-- Animate animationAction ->
--     -- here you can apply new styles, to be animated
--     --- ...
--     model ! []


main : Program Initializer Model Action
main =
    programWithFlags GoToPage
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ scroll Header
          -- , Animation.subscription Animate [ model.style ]
        ]
