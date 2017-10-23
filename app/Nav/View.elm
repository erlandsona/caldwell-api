module Nav.View exposing (template)

-- Libs

import Css exposing (opacity, transform, translate2, translate3d, zero)
import Html exposing (..)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onClick, onWithOptions, Options)
import Json.Decode exposing (succeed)


-- Source

import Constants exposing (..)
import Model exposing (Model)
import Types exposing (..)


{ class } =
    withNamespace homepage


template : Model -> Html Action
template model =
    let
        navOpen =
            navState model
    in
        nav
            [ styles
                (if navOpen then
                    [ transform (translate2 zero zero)
                    , transform (translate3d zero zero zero)
                    ]
                 else
                    []
                )
            , class [ NavBar () ]
            , clickWithStopProp (Toggle Closed)
            ]
            [ aTag Home
            , aTag About
            , aTag Shows
            , aTag Music
            , aTag Contact
            , ul
                [ styles
                    (if navOpen then
                        [ opacity zero
                        ]
                     else
                        []
                    )
                , class [ NavBar "handle" ]
                , clickWithStopProp (Toggle <| not model.nav)
                ]
                [ li [] [] ]
            ]


aTag : Page -> Html Action
aTag page =
    a [ class [ (NavBar "a") ], onClick (ScrollTo page) ]
        [ span [] [ text (toString page) ]
        ]


not : Nav -> Nav
not navState =
    case navState of
        Open ->
            Closed

        Closed ->
            Open


clickWithStopProp : Action -> Attribute Action
clickWithStopProp action =
    -- Options stopProp prevDefault
    onWithOptions "click" (Options True False) (succeed action)


navState : Model -> Bool
navState { currentPage, nav } =
    case currentPage of
        Home ->
            True

        _ ->
            case nav of
                Open ->
                    True

                Closed ->
                    False
