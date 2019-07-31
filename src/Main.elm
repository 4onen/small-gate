module Main exposing (main)

import Browser
import Color as Col
import Grid
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
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
            Grid.initialize 50 50 (\x y -> (modBy 7 (x * y))*(1-2*modBy 2 (x+y)) < model)

        gridRender =
            grid
                |> Grid.indexedMap
                    (\x y e ->
                        case e of
                            True ->
                                let
                                    rect =
                                        Svg.rect [ SA.width "1", SA.height "1", SA.x (String.fromInt x), SA.y (String.fromInt y), SA.fill "lightgrey" ] []

                                    neighbours =
                                        Grid.fourNeighbours x y grid

                                    north =
                                        case neighbours.north of
                                            Just True ->
                                                Nothing

                                            _ ->
                                                Just <|
                                                    Svg.line
                                                        [ SA.x1 (String.fromInt x)
                                                        , SA.x2 (String.fromInt <| x + 1)
                                                        , SA.y1 (String.fromInt y)
                                                        , SA.y2 (String.fromInt y)
                                                        , SA.stroke "red"
                                                        , SA.strokeWidth "0.1"
                                                        ]
                                                        []

                                    south =
                                        case neighbours.south of
                                            Just True ->
                                                Nothing

                                            _ ->
                                                Just <|
                                                    Svg.line
                                                        [ SA.x1 (String.fromInt <| x + 1)
                                                        , SA.x2 (String.fromInt x)
                                                        , SA.y1 (String.fromInt <| y + 1)
                                                        , SA.y2 (String.fromInt <| y + 1)
                                                        , SA.stroke "red"
                                                        , SA.strokeWidth "0.1"
                                                        ]
                                                        []

                                    east =
                                        case neighbours.east of
                                            Just True ->
                                                Nothing

                                            _ ->
                                                Just <|
                                                    Svg.line
                                                        [ SA.x1 (String.fromInt <| x + 1)
                                                        , SA.x2 (String.fromInt <| x + 1)
                                                        , SA.y1 (String.fromInt y)
                                                        , SA.y2 (String.fromInt <| y + 1)
                                                        , SA.stroke "red"
                                                        , SA.strokeWidth "0.1"
                                                        ]
                                                        []

                                    west =
                                        case neighbours.west of
                                            Just True ->
                                                Nothing

                                            _ ->
                                                Just <|
                                                    Svg.line
                                                        [ SA.x1 (String.fromInt x)
                                                        , SA.x2 (String.fromInt x)
                                                        , SA.y1 (String.fromInt y)
                                                        , SA.y2 (String.fromInt <| y + 1)
                                                        , SA.stroke "red"
                                                        , SA.strokeWidth "0.1"
                                                        ]
                                                        []

                                    borders =
                                        List.filterMap identity [ north, south, east, west ]
                                in
                                Just (rect :: borders)

                            False ->
                                Nothing
                    )
                |> Grid.toList
                |> List.filterMap identity
                |> List.concatMap identity
                |> Svg.svg [ HA.width 1000, HA.height 1000, SA.viewBox "0 0 50 50" ]
    in
    Html.div [] [ Html.button [ HE.onClick Inc ] [ Html.text "+" ], Html.button [ HE.onClick Dec ] [ Html.text "-" ],gridRender ]


transparent : Col.Color
transparent =
    Col.rgba 0.0 0.0 0.0 0.0
