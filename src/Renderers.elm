module Renderers exposing (..)

import Dict exposing (Dict)
import Grid exposing (Grid)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


layerViewerFromID : LayerID -> (Grid -> ( Int, Int ) -> List (Svg msg))
layerViewerFromID l =
    case l of
        Nwell ->
            renderSimple "lightgrey"

        Ndiff ->
            renderSimple "grey"

        Pdiff ->
            renderSimple "lightblue"

        Metal ->
            renderMetal

        Polysilicon ->
            renderPoly

        Contacts ->
            renderContacts


renderSimple : String -> Grid -> ( Int, Int ) -> List (Svg msg)
renderSimple color _ ( x, y ) =
    List.singleton <| Svg.rect [ SA.width "1", SA.height "1", SA.x (String.fromInt x), SA.y (String.fromInt y), SA.fill color ] []


renderBordered : (( Int, Int ) -> List (Svg msg)) -> String -> Grid -> ( Int, Int ) -> List (Svg msg)
renderBordered renderSquare color grid ( x, y ) =
    let
        rect =
            renderSquare ( x, y )

        neighbours =
            Grid.fourNeighbours x y grid

        dirs =
            [ ( .north, ( 0, 1 ), ( 1, 1 ) )
            , ( .south, ( 1, 0 ), ( 0, 0 ) )
            , ( .east, ( 1, 1 ), ( 0, 1 ) )
            , ( .west, ( 0, 0 ), ( 0, 1 ) )
            ]

        fn nbs ( dfn, ( x1, x2 ), ( y1, y2 ) ) =
            if dfn nbs then
                Nothing

            else
                Just <|
                    Svg.line
                        [ SA.x1 (String.fromInt <| x + x1)
                        , SA.x2 (String.fromInt <| x + x2)
                        , SA.y1 (String.fromInt <| y + y1)
                        , SA.y2 (String.fromInt <| y + y2)
                        , SA.stroke color
                        , SA.strokeWidth "0.1"
                        ]
                        []

        borders =
            List.filterMap (fn neighbours) dirs
    in
    rect ++ borders


renderMetal : Grid -> ( Int, Int ) -> List (Svg msg)
renderMetal =
    renderBordered renderMetalSquare "cornflowerblue"


renderMetalSquare : ( Int, Int ) -> List (Svg msg)
renderMetalSquare ( x, y ) =
    [ Svg.line
        [ SA.x1 (String.fromFloat <| toFloat x + 0.5)
        , SA.x2 (String.fromInt x)
        , SA.y1 (String.fromInt y)
        , SA.y2 (String.fromFloat <| toFloat y + 0.5)
        , SA.stroke "cornflowerblue"
        , SA.strokeWidth "0.1"
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromInt x)
        , SA.x2 (String.fromInt <| x + 1)
        , SA.y1 (String.fromInt <| y + 1)
        , SA.y2 (String.fromInt y)
        , SA.stroke "cornflowerblue"
        , SA.strokeWidth "0.1"
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromInt <| x + 1)
        , SA.x2 (String.fromFloat <| toFloat x + 0.5)
        , SA.y1 (String.fromFloat <| toFloat y + 0.5)
        , SA.y2 (String.fromInt <| y + 1)
        , SA.stroke "cornflowerblue"
        , SA.strokeWidth "0.1"
        ]
        []
    ]


renderPoly : Grid -> ( Int, Int ) -> List (Svg msg)
renderPoly =
    renderBordered renderPolySquare "black"


renderPolySquare : ( Int, Int ) -> List (Svg msg)
renderPolySquare ( x, y ) =
    [ Svg.line
        [ SA.x1 (String.fromFloat <| toFloat x + 0.5)
        , SA.x2 (String.fromInt <| x + 1)
        , SA.y1 (String.fromInt y)
        , SA.y2 (String.fromFloat <| toFloat y + 0.5)
        , SA.stroke "black"
        , SA.strokeWidth "0.1"
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromInt <| x + 1)
        , SA.x2 (String.fromInt x)
        , SA.y1 (String.fromInt <| y + 1)
        , SA.y2 (String.fromInt y)
        , SA.stroke "black"
        , SA.strokeWidth "0.1"
        ]
        []
    , Svg.line
        [ SA.x1 (String.fromInt <| x)
        , SA.x2 (String.fromFloat <| toFloat x + 0.5)
        , SA.y1 (String.fromFloat <| toFloat y + 0.5)
        , SA.y2 (String.fromInt <| y + 1)
        , SA.stroke "black"
        , SA.strokeWidth "0.1"
        ]
        []
    ]


renderContacts grid ( x, y ) =
    List.singleton <|
        Svg.rect
            [ SA.width "0.6"
            , SA.height "0.6"
            , SA.x (String.fromFloat (0.2 + toFloat x))
            , SA.y (String.fromFloat (0.2 + toFloat y))
            , SA.fill "black"
            ]
            []


renderLabels : Dict String ( Int, Int ) -> List (Svg msg)
renderLabels =
    let
        viewLabel : ( String, ( Int, Int ) ) -> List (Svg msg)
        viewLabel ( label, ( x, y ) ) =
            [ Svg.rect
                [ SA.x (String.fromInt x)
                , SA.y (String.fromInt y)
                , SA.width "1"
                , SA.height "1"
                , SA.fill "yellow"
                , SA.fillOpacity "0.4"
                ]
                []
            , Svg.text_
                [ SA.x (String.fromInt x)
                , SA.y (String.fromFloat (Basics.toFloat y + 0.5))
                , SA.fontSize "0.4"
                , SA.textLength "1"
                ]
                [ Svg.text label ]
            ]
    in
    Dict.toList
        >> List.concatMap viewLabel
