module Render exposing (view)

import Dict exposing (Dict)
import Element
import Grid exposing (Grid)
import Html
import Html.Attributes as HA
import Renderers exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA
import SvgClick
import Types exposing (..)


view : Model -> Element.Element Msg
view model =
    let
        activeLayers =
            model.views
                |> List.map
                    (\v ->
                        case v of
                            LabelsView ->
                                model.labels
                                    |> renderLabels
                                    |> Svg.g []

                            LayerView id ->
                                Grid.render
                                    (funcFromID id model.layers)
                                    (layerViewerFromID id)
                                    |> Svg.g []

                            LabelConnec label ->
                                Dict.fromList [ ( "ConnecNotSupported", ( 0, 0 ) ) ]
                                    |> renderLabels
                                    |> Svg.g []
                    )

        ( ( vx, vy ), ( vw, vh ) ) =
            layerIDs
                |> List.map (funcFromID >> (\f -> f model.layers))
                |> viewBoxOfList
    in
    Element.html <|
        Svg.svg
            [ [ vx, vy, vw, vh ]
                |> List.map String.fromInt
                |> List.intersperse " "
                |> String.concat
                |> SA.viewBox
            , SvgClick.onDown DragDown
            , SvgClick.onMove DragMove
            , SvgClick.onUp DragUp
            , SA.preserveAspectRatio "xMidYMid meet"
            , HA.style "width" "calc(98vw - 200px)"
            , HA.style "height" "calc(98vh - 50px)"
            , HA.style "border" "1px solid black"
            ]
            activeLayers


viewLayersSVG : Layers -> List (Svg msg)
viewLayersSVG layers =
    layerIDs
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
