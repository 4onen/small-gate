module Main exposing (main)

import Browser
import Color as Col
import Grid exposing (Grid)
import Grid.Render
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (Svg)
import Svg.Attributes as SA
import Layers


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type Msg
    = Noop
    | Inc
    | Dec


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init () =
    ( 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Noop ->
            model

        Inc ->
            model + 1

        Dec ->
            model - 1
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        grid =
            Grid.initialize 34 34 (\x y -> modBy 11 (x * y) - 2 > model)
        grid2 =
            Grid.initialize 34 34 (\x y -> modBy 11 (x * y) - 3 > model)

        gridRender =
            [ Grid.Render.render grid Layers.renderMetal
                |> Svg.g [ ]
            , Grid.Render.render (Grid.map not grid2) Layers.renderPoly
                |> Svg.g [ ]
            ]
                |> Svg.svg [ HA.width 800, HA.height 800, SA.viewBox ("0 0 " ++ (String.fromInt <| grid.width) ++ " " ++ (String.fromInt <| grid.height)) ]
    in
    Html.div [] [ Html.button [ HE.onClick Inc ] [ Html.text "+" ], Html.button [ HE.onClick Dec ] [ Html.text "-" ], gridRender ]


transparent : Col.Color
transparent =
    Col.rgba 0.0 0.0 0.0 0.0
