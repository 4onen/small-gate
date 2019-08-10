module Layers exposing (Layer, LayerID(..), Layers, Msg(..), funcFromID, init, layerIDs, layerViewerFromID, onSvgSpaceClick, renderBordered, renderContacts, renderMetal, renderMetalSquare, renderPoly, renderPolySquare, renderSimple, update, view, viewBox, viewBoxOfList, viewLayers)

import Element exposing (Element)
import Grid exposing (Grid)
import Grid.Render
import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
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


type Msg
    = Click Float Float
    | Noop


init : Layers
init =
    let
        g =
            Grid.empty
    in
    Layers g g g g g g


update : Msg -> Layers -> Layers
update msg layers =
    case msg of
        Noop ->
            layers

        Click fx fy ->
            let
                ( x, y ) =
                    ( floor fx, floor fy )

                newVal =
                    layers.poly
                        |> Grid.get x y
                        |> Basics.not
            in
            { layers | poly = Grid.set x y newVal layers.poly }


view : Layers -> Element Msg
view layers =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ viewLayers layers
        ]


viewLayers : Layers -> Element Msg
viewLayers layers =
    let
        ( ( vx, vy ), ( vw, vh ) ) =
            layerIDs
                |> List.map (funcFromID >> (\f -> f layers))
                |> viewBoxOfList
    in
    layers
        |> viewLayersSVG
        |> (::)
            (Svg.rect
                [ SA.x (String.fromInt <| vx + 1)
                , SA.y (String.fromInt <| vy + 1)
                , SA.width (String.fromInt <| max 0 <| vw - 2)
                , SA.height (String.fromInt <| max 0 <| vh - 2)
                , SA.stroke "grey"
                , SA.strokeWidth "0.1"
                , SA.fillOpacity "0"
                ]
                []
            )
        |> Svg.svg
            [ [ vx, vy, vw, vh ]
                |> List.map String.fromInt
                |> List.intersperse " "
                |> String.concat
                |> SA.viewBox
            , onSvgSpaceClick Click
            , HA.style "width" "auto"
            , HA.style "height" "auto"
            , HA.style "border" "1px solid black"
            ]
        |> Element.html


viewLayersSVG : Layers -> List (Svg msg)
viewLayersSVG layers =
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
                        funcFromID id layers

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


onSvgSpaceClick : (Float -> Float -> msg) -> Html.Attribute msg
onSvgSpaceClick tagger =
    let
        decoder =
            JD.map2 tagger
                (JD.at [ "detail", "x" ] JD.float)
                (JD.at [ "detail", "y" ] JD.float)
    in
    HE.on "svgclick" decoder
