module Layers exposing (Layer, LayerID(..), Layers, Model, Msg, funcFromID, init, layerIDs, update, view)

import Element exposing (Element)
import Element.Background
import Element.Input
import Grid exposing (Grid)
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


type alias Model =
    { layers : Layers
    , selectedLayer : Int
    }


type Msg
    = DrawspaceClick Float Float
    | Inc
    | Dec
    | Noop


init : Model
init =
    let
        g =
            Grid.empty
    in
    Model (Layers g g g g g g) 0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        Inc ->
            { model | selectedLayer = model.selectedLayer + 1 }

        Dec ->
            { model | selectedLayer = model.selectedLayer - 1 }

        DrawspaceClick fx fy ->
            case List.drop model.selectedLayer layerIDs |> List.head of
                Nothing ->
                    model

                Just layerID ->
                    let
                        ( x, y ) =
                            ( floor fx, floor fy )

                        layers =
                            model.layers

                        layer =
                            funcFromID layerID layers

                        newVal =
                            layer
                                |> Grid.get x y
                                |> Basics.not

                        updatedLayer =
                            Grid.set x y newVal layer
                    in
                    { model
                        | layers =
                            case layerID of
                                Diffusion ->
                                    { layers | diffusion = updatedLayer }

                                NMOS ->
                                    { layers | nmos = updatedLayer }

                                PMOS ->
                                    { layers | pmos = updatedLayer }

                                Metal ->
                                    { layers | metal = updatedLayer }

                                Polysilicon ->
                                    { layers | poly = updatedLayer }

                                Contacts ->
                                    { layers | contacts = updatedLayer }
                    }


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.row [ Element.height <| Element.px 50 ] <| viewToolbar model
        , Element.el [ Element.centerX, Element.centerY ] <| viewLayers model.layers
        ]


viewToolbar : Model -> List (Element Msg)
viewToolbar model =
    [ Element.Input.button
        [ Element.height Element.fill
        , Element.Background.color (Element.rgb 0.5 0.0 0.5)
        ]
        { onPress = Just Inc, label = Element.text "Inc" }
    , Element.Input.button
        [ Element.height Element.fill
        , Element.Background.color (Element.rgb 0.5 0.0 0.5)
        ]
        { onPress = Just Dec, label = Element.text "Dec" }
    ]


viewToolbar : Model -> List (Element Msg)
viewToolbar model =
    [ Element.Input.button
        [ Element.width Element.fill
        , Element.Background.color (Element.rgb 0.5 0.0 0.5)
        ]
        { onPress = Just Inc, label = Element.text "Inc" }
    , Element.Input.button
        [ Element.width Element.fill
        , Element.Background.color (Element.rgb 0.5 0.0 0.5)
        ]
        { onPress = Just Dec, label = Element.text "Dec" }
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
            , onSvgSpaceClick DrawspaceClick
            , SA.preserveAspectRatio "xMidYMid meet"
            , HA.style "width" "99vw"
            , HA.style "height" "calc(99vh - 50px)"
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
                Grid.render layer layerViewer
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
    List.singleton <|
        Svg.rect
            [ SA.width "0.6"
            , SA.height "0.6"
            , SA.x (String.fromFloat (0.2 + toFloat x))
            , SA.y (String.fromFloat (0.2 + toFloat y))
            , SA.fill "black"
            ]
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
