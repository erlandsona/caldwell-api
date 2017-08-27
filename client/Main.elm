module Site exposing (main)

-- Libs

import Debug
import Date exposing (Date)


-- import Date.Extra.Duration exposing (add, Duration(..))

import Date.Extra.Compare exposing (is, Compare2(..))
import Http
import Html exposing (Html, Attribute, header, node, span, text)
import Html.Events exposing (onClick)
import Html.CssHelpers exposing (withNamespace)


-- import Json.Decode.Pipeline exposing (decode, required)

import Json.Decode exposing (decodeValue, int, keyValuePairs, maybe, string)


-- import Json.Decode.Extra exposing (date)

import Json.Encode exposing (Value)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (unpack)
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
        , homepage
        )
import Main.View as Main
import Nav.View as Nav
import Model exposing (Model)
import Ports exposing (easeIntoView, snapIntoView)
import Server exposing (Gig, getV1Shows, decodeGig)
import Styles as Styles
import Types exposing (..)


-- Source


type alias Initializer =
    { cachedGigs : Maybe Value
    , now : Time
    }


decodeLocalStorageGigs : Date -> Maybe Value -> List Gig
decodeLocalStorageGigs today json =
    let
        decodedGigs json =
            case (decodeValue (decodeGig |> keyValuePairs) json) of
                Ok shows ->
                    List.map (\( msg, location ) -> (Debug.log msg) location) shows

                Err msg ->
                    Debug.log msg []
    in
        unpack (\() -> Debug.log "No shows in local storage." []) decodedGigs json


init : Initializer -> Location -> ( Model, Cmd Msg )
init { cachedGigs, now } location =
    let
        today =
            Date.fromTime now

        model =
            { history = [ parse location ]
            , nav = Closed
            , shows =
                (decodeLocalStorageGigs today cachedGigs)
                    |> List.filter (.gigDate >> is SameOrAfter today)
                    |> List.sortBy (Date.toTime << .gigDate)
            }
    in
        model
            ! [ location
                    |> parse
                    |> toString
                    |> snapIntoView
              , Http.send ShowResponse getV1Shows
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


{ id, class, classList } =
    withNamespace homepage


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
            { model | history = (::) (parse location) model.history }
                ! [ location
                        |> parse
                        |> toString
                        |> easeIntoView
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
                            Debug.log "Shows Reponse" <| toString shows
                    in
                        { model | shows = shows } ! []

                Err msg ->
                    let
                        _ =
                            Debug.log "Shows Reponse" <| toString msg
                    in
                        model ! []


main : Program Initializer Model Msg
main =
    programWithFlags GoToPage
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
