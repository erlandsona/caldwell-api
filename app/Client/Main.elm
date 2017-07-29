module Site exposing (main)

-- Libs

import Debug
import Date exposing (Date)


-- import Date.Extra.Duration exposing (add, Duration(..))

import Date.Extra.Compare exposing (is, Compare2(..))
import Http
import Html exposing (Html, Attribute, header, node, span, text)
import Html.Events exposing (onClick)


-- import Json.Decode.Pipeline exposing (decode, required)

import Json.Decode exposing (decodeValue, int, keyValuePairs, maybe, string)


-- import Json.Decode.Extra exposing (date)

import Json.Encode exposing (Value)
import Maybe exposing (withDefault)
import Navigation as Nav exposing (programWithFlags, Location)
import String exposing (toLower)


-- import Task exposing (perform)

import Time exposing (Time)
import UrlParser as Url exposing (oneOf, s)


-- Modules

import Constants
    exposing
        ( blackOverlay
        , caldwellBackground
        , container
        )
import Main.View as Main
import Nav.View as Nav
import Model exposing (Model)
import Ports exposing (easeIntoView, snapIntoView)
import Server exposing (Venue, getApiShows, decodeVenue)
import Styles as Styles
import Types exposing (..)


-- Source


type alias Initializer =
    { shows : Maybe Value
    , now : Time
    }


showsDecoder : Date -> Maybe Value -> List Venue
showsDecoder today json =
    let
        noShowsVenue =
            Venue (Just -1) today "No Shows Scheduled"

        decodedVenues json =
            let
                decoded =
                    decodeValue (decodeVenue |> keyValuePairs)
            in
                case (decoded json) of
                    Ok shows ->
                        List.map (\( msg, location ) -> (Debug.log msg) location) shows

                    Err msg ->
                        Debug.log msg [ noShowsVenue ]
    in
        case json of
            Just json ->
                decodedVenues json

            Nothing ->
                Debug.log "No Shows" [ noShowsVenue ]


init : Initializer -> Location -> ( Model, Cmd Msg )
init { shows, now } location =
    let
        today =
            Date.fromTime now

        venues =
            showsDecoder today shows

        model =
            { history = [ parse location ]
            , nav = Closed
            , shows =
                venues
                    |> List.filter (.venueDate >> is SameOrBefore today)
                    |> List.sortBy (Date.toTime << .venueDate)
            }
    in
        model
            ! [ location
                    |> parse
                    |> toString
                    |> snapIntoView
              , Http.send ShowResponse getApiShows
              ]


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


view : Model -> Html Msg
view model =
    node container
        [ onClick (Toggle Closed)
        ]
        [ Styles.css_ model
        , node caldwellBackground [] []
        , node blackOverlay [] []
        , header [ onClick (SetUrl Home) ]
            [ span [] [ text "C" ]
            , text "aldwell"
            ]
        , Nav.template model.nav
        , Main.template model.shows
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToPage location ->
            ( { model | history = (::) (parse location) model.history }
            , location
                |> parse
                |> toString
                |> easeIntoView
            )

        SetUrl url ->
            ( model
            , Nav.newUrl <|
                if url == Home then
                    "/"
                else
                    url |> toString |> toLower
            )

        Toggle newState ->
            ( { model | nav = newState }, Cmd.none )

        ShowResponse results ->
            let
                _ =
                    Debug.log <| toString results
            in
                case results of
                    Ok shows ->
                        ( { model | shows = shows }, Cmd.none )

                    Err msg ->
                        ( model, Cmd.none )


main : Program Initializer Model Msg
main =
    programWithFlags GoToPage
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
