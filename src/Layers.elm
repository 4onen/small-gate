module Layers exposing (layerRenderers, layers, renderContacts, renderMetal, renderPoly, renderSimple)

import Grid exposing (Grid)
import Svg exposing (Svg)
import Svg.Attributes as SA


layers : List String
layers =
    [ "diffusion"
    , "nmos"
    , "pmos"
    , "metal"
    , "poly"
    , "contacts"
    ]


layerRenderers : List (Grid Bool -> Int -> Int -> Bool -> List (Svg msg))
layerRenderers =
    [ renderSimple "lightgrey"
    , renderSimple "lightblue"
    , renderSimple "grey"
    , renderMetal
    , renderPoly
    , renderContacts
    ]


renderSimple : String -> Grid Bool -> Int -> Int -> Bool -> List (Svg msg)
renderSimple color _ x y e =
    case e of
        True ->
            List.singleton <| Svg.rect [ SA.width "1", SA.height "1", SA.x (String.fromInt x), SA.y (String.fromInt y), SA.fill color ] []

        False ->
            []


renderBordered : (Int -> Int -> List (Svg msg)) -> String -> Grid Bool -> Int -> Int -> Bool -> List (Svg msg)
renderBordered renderSquare color grid x y e =
    case e of
        True ->
            let
                rect =
                    renderSquare x y

                neighbours =
                    Grid.fourNeighbours x y grid

                dirs =
                    [ ( .north, ( 0, 1 ), ( 0, 0 ) )
                    , ( .south, ( 1, 0 ), ( 1, 1 ) )
                    , ( .east, ( 1, 1 ), ( 0, 1 ) )
                    , ( .west, ( 0, 0 ), ( 0, 1 ) )
                    ]

                fn nbs ( dfn, ( x1, x2 ), ( y1, y2 ) ) =
                    case dfn nbs of
                        Just True ->
                            Nothing

                        _ ->
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

        False ->
            []


renderMetal : Grid Bool -> Int -> Int -> Bool -> List (Svg msg)
renderMetal =
    renderBordered renderMetalSquare "cornflowerblue"


renderMetalSquare : Int -> Int -> List (Svg msg)
renderMetalSquare x y =
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


renderPoly : Grid Bool -> Int -> Int -> Bool -> List (Svg msg)
renderPoly =
    renderBordered renderPolySquare "black"


renderPolySquare : Int -> Int -> List (Svg msg)
renderPolySquare x y =
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


renderContacts grid x y e =
    []
