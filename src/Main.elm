module Main exposing (main)

import Browser
import Color as Col
import Grid exposing (Grid)
import Grid.Render
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Layers exposing (Layers)
import Svg exposing (Svg)
import Svg.Attributes as SA


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type Msg
    = Noop


type alias Model =
    Layers


init : () -> ( Model, Cmd Msg )
init () =
    ( Layers.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Noop ->
            model
    , Cmd.none
    )


view : Model -> Html Msg
view =
    Layers.view [HA.width 800, HA.height 800]


transparent : Col.Color
transparent =
    Col.rgba 0.0 0.0 0.0 0.0
