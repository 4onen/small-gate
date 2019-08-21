module Render exposing (view)

import Decode exposing (..)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Grid exposing (Grid)
import Html
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.row [ Element.height <| Element.px 50 ] <| viewToolbar model
        , Element.row [ Element.centerX, Element.centerY ]
            [ viewLayers model.layers model.labels
            , Element.column
                [ Element.width <| Element.px 200
                , Element.height Element.fill
                ]
                (Element.text "Labels: (click removes)" :: (model.labels |> Dict.keys |> viewLabels))
            ]
        ]


viewLabels : List String -> List (Element Msg)
viewLabels =
    let
        viewLabel : String -> Element Msg
        viewLabel label =
            Element.Input.button []
                { onPress = Just (RemoveLabel label)
                , label = Element.text label
                }
    in
    List.map viewLabel


viewToolbar : Model -> List (Element Msg)
viewToolbar model =
    let
        selectedColor =
            Element.rgb 1.0 1.0 0.0

        borderColor =
            Element.rgb 0.0 0.0 1.0

        toolbarButton { color, selected, onPress, label } =
            Element.Input.button
                [ Element.height Element.fill
                , Element.Background.color color
                , Element.Border.solid
                , Element.Border.width 5
                , Element.Border.color
                    (if selected then
                        selectedColor

                     else
                        borderColor
                    )
                ]
                { onPress = onPress, label = label }

        drawingTool layer tool =
            case tool of
                Drawing layerID _ ->
                    layerID == layer

                _ ->
                    False
    in
    [ toolbarButton
        { color = Element.rgb 0.82421875 0.82421875 0.82421875
        , selected = drawingTool Diffusion model.tool
        , onPress = Just <| PickTool <| DrawTool <| Diffusion
        , label = Element.text "Diffusion"
        }
    , toolbarButton
        { color = Element.rgb 0.5 0.5 0.5
        , selected = drawingTool NMOS model.tool
        , onPress = Just <| PickTool <| DrawTool <| NMOS
        , label = Element.text "NMOS"
        }
    , toolbarButton
        { color = Element.rgb 0.67578125 0.84375 0.8984375
        , selected = drawingTool PMOS model.tool
        , onPress = Just <| PickTool <| DrawTool <| PMOS
        , label = Element.text "PMOS"
        }
    , toolbarButton
        { color = Element.rgb 0.390625 0.58203125 0.92578125
        , selected = drawingTool Metal model.tool
        , onPress = Just <| PickTool <| DrawTool <| Metal
        , label = Element.text "Metal"
        }
    , Element.Input.button
        [ Element.height Element.fill
        , Element.Background.color (Element.rgb 0.0 0.0 0.0)
        , Element.Font.color (Element.rgb 1.0 1.0 1.0)
        , Element.Border.solid
        , Element.Border.width 5
        , Element.Border.color
            (if drawingTool Polysilicon model.tool then
                selectedColor

             else
                borderColor
            )
        ]
        { onPress = Just <| PickTool <| DrawTool <| Polysilicon, label = Element.text "Poly" }
    , toolbarButton
        { color = Element.rgb 1.0 1.0 1.0
        , selected = drawingTool Contacts model.tool
        , onPress = Just <| PickTool <| DrawTool <| Contacts
        , label = Element.text "Contacts"
        }
    , case model.tool of
        TypingLabel str _ ->
            Element.Input.text
                [ Element.height Element.fill
                , Element.width <| Element.px 100
                , Element.focused []
                , Element.Border.solid
                , Element.Border.width 5
                , Element.Border.color selectedColor
                ]
                { onChange = ChangeLabel
                , text = str
                , placeholder = Nothing
                , label =
                    Element.Input.labelHidden "Enter Label"
                }

        _ ->
            Element.Input.button
                [ Element.height Element.fill
                , Element.Border.solid
                , Element.Border.width 5
                , Element.Border.color borderColor
                ]
                { onPress = Just (PickTool LabelTool)
                , label = Element.text "Label"
                }
    ]


viewLayers : Layers -> Dict String ( Int, Int ) -> Element Msg
viewLayers layers labels =
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
        |> (\l -> l ++ renderLabels layers labels)
        |> Svg.svg
            [ [ vx, vy, vw, vh ]
                |> List.map String.fromInt
                |> List.intersperse " "
                |> String.concat
                |> SA.viewBox
            , onSvgDown DragDown
            , onSvgMove DragMove
            , onSvgUp DragUp
            , SA.preserveAspectRatio "xMidYMid meet"
            , HA.style "width" "calc(98vw - 200px)"
            , HA.style "height" "calc(98vh - 50px)"
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


renderLabels : Layers -> Dict String ( Int, Int ) -> List (Svg msg)
renderLabels layers =
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
