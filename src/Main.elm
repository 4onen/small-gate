module Main exposing (main)

import Browser
import Color as Col
import Grid exposing (Grid)
import Grid.Render
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Layers
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
            Grid.initialize (34+model*model) (34+model*model) (\(x,y) -> modBy 4 (x + y) == 0)

        grid2 =
            Grid.initialize 34 34 (\(x,y) -> modBy 11 (x * y) - 3 > model)

        ( ( vx, vy ), ( vw, vh ) ) =
            viewBoxOfList [ grid, grid2 ]

        gridRender =
            [ Grid.Render.render grid Layers.renderMetal
                |> Svg.g []
            , Grid.Render.render grid2 Layers.renderPoly
                |> Svg.g []
            ]
                |> Svg.svg
                    [ HA.width 800
                    , HA.height 800
                    , [ vx, vy, vw, vh ]
                        |> List.map String.fromInt
                        |> List.intersperse " "
                        |> String.concat
                        |> SA.viewBox
                    ]
    in
    Html.div [] [ gridRender, Html.text (String.fromInt model), Html.button [ HE.onClick Inc ] [ Html.text "+" ], Html.button [ HE.onClick Dec ] [ Html.text "-" ] ]


viewBox : ( ( Int, Int ), ( Int, Int ) ) -> ( ( Int, Int ), ( Int, Int ) )
viewBox ( ( x1, y1 ), ( x2, y2 ) ) =
    ( ( x1 - 1, y1 - 1 ), ( x2 + 3 - x1, y2 + 3 - y1 ) )


viewBoxOfList : List Grid -> ( ( Int, Int ), ( Int, Int ) )
viewBoxOfList =
    List.map Grid.activeArea
        >> List.foldl
            (\pta ptb ->
                case ( pta, ptb ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Just pt, Nothing ) ->
                        Just pt

                    ( Nothing, Just pt ) ->
                        Just pt

                    ( Just ( ( x1, y1 ), ( x2, y2 ) ), Just ( ( x3, y3 ), ( x4, y4 ) ) ) ->
                        Just ( ( min x1 x3, min y1 y3 ), ( max x2 x4, max y2 y4 ) )
            )
            Nothing
        >> Maybe.map viewBox
        >> Maybe.withDefault ( ( 0, 0 ), ( 1, 1 ) )


transparent : Col.Color
transparent =
    Col.rgba 0.0 0.0 0.0 0.0
