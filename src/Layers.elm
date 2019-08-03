module Layers exposing (Layer, LayerID(..), Layers, funcFromID, init, layerViewerFromID, renderBordered, renderContacts, renderMetal, renderMetalSquare, renderPoly, renderPolySquare, renderSimple, view, viewLayers)

import Grid exposing (Grid)
import Grid.Render
import Html
import Svg exposing (Svg)
import Svg.Attributes as SA


type LayerID
    = Diffusion
    | NMOS
    | PMOS
    | Metal
    | Polysilicon
    | Contacts


layerIDs : List LayerID
layerIDs =
    [ Diffusion, NMOS, PMOS, Metal, Polysilicon, Contacts ]


type alias Layer =
    Grid


type alias Layers =
    { diffusion : Layer
    , nmos : Layer
    , pmos : Layer
    , metal : Layer
    , poly : Layer
    , contacts : Layer
    }


init : Layers
init =
    let
        g1 = Grid.initialize 34 34 (\(x,y) -> modBy 4 (x+y//3+y//5+y//7+y*17) == 0)
        g2 = Grid.initialize 34 34 (\(x,y) -> modBy 4 (x-y//3+y//5+y//7+y*17) == 0)
        g3 = Grid.initialize 34 34 (\(x,y) -> modBy 4 (x+y//3-y//5+y//7+y*17) == 0)
        g4 = Grid.initialize 34 34 (\(x,y) -> modBy 4 (x+y//3+y//5-y//7+y*17) == 0)
        g5 = Grid.initialize 34 34 (\(x,y) -> modBy 4 (x+y//3+y//5+y//7-y*17) == 0)
        g6 = Grid.initialize 34 34 (\(x,y) -> modBy 4 (x-y//3+y//5-y//7+y*17) == 0)
    in
    Layers g1 g2 g3 g4 g5 g6 


view : List (Html.Attribute msg) -> Layers -> Html.Html msg
view attrs layers =
    let
        ( ( vx, vy ), ( vw, vh ) ) =
            layerIDs
                |> List.map (funcFromID >> (\f -> f layers))
                |> viewBoxOfList
    in
    layers
        |> viewLayers
        |> Svg.svg
            (([ vx, vy, vw, vh ]
                |> List.map String.fromInt
                |> List.intersperse " "
                |> String.concat
                |> SA.viewBox
             )
                :: attrs
            )


viewLayers : Layers -> List (Svg msg)
viewLayers layers =
    [ Diffusion
    , NMOS
    , PMOS
    , Metal
    , Polysilicon
    , Contacts
    ]
        |> List.map
            (\id ->
                let
                    layer =
                        (funcFromID id) layers

                    layerViewer =
                        layerViewerFromID id
                in
                Grid.Render.render layer layerViewer
                    |> Svg.g []
            )


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


funcFromID : LayerID -> (Layers -> Layer)
funcFromID l =
    case l of
        Diffusion ->
            .diffusion

        NMOS ->
            .nmos

        PMOS ->
            .pmos

        Metal ->
            .metal

        Polysilicon ->
            .poly

        Contacts ->
            .contacts


layerViewerFromID : LayerID -> (Grid -> ( Int, Int ) -> List (Svg msg))
layerViewerFromID l =
    case l of
        Diffusion ->
            renderSimple "lightgrey"

        NMOS ->
            renderSimple "grey"

        PMOS ->
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
    []
