module Layout.Tools exposing (ToolFunction, toolFunction)

import Dict
import Layout.Grid
import Layout.Types exposing (..)


type alias ToolFunction =
    { dragDown : Int -> Int -> Model -> Model
    , dragMove : Int -> Int -> Model -> Model
    , dragUp : Int -> Int -> Model -> Model
    , update : Msg -> Model -> Model
    }


toolFunction : Tool -> ToolFunction
toolFunction tool =
    case tool of
        Drawing _ ->
            drawTool

        TypingLabel _ ->
            labelTool


drawTool : ToolFunction
drawTool =
    { dragDown =
        \x y model ->
            { model | tool = Drawing <| Just (Drag ( x, y ) ( x, y )) }
    , dragMove =
        \x y model ->
            case model.tool of
                Drawing (Just { start, curr }) ->
                    { model | tool = Drawing <| Just (Drag start ( x, y )) }

                _ ->
                    model
    , dragUp =
        \x y model ->
            case model.tool of
                Drawing (Just { start }) ->
                    { model
                        | tool = Drawing Nothing
                        , layers = flipRect start ( x, y ) model.selectedLayer model.layers
                    }

                _ ->
                    model
    , update = always identity
    }


flipRect : ( Int, Int ) -> ( Int, Int ) -> LayerID -> Layers -> Layers
flipRect ( startX, startY ) ( x, y ) selectedLayer layers =
    let
        layer =
            funcFromID selectedLayer layers

        newVal =
            layer
                |> Layout.Grid.get startX startY
                |> Basics.not

        newLayer =
            List.range (min startX x) (max startX x)
                |> List.concatMap
                    (\thisX ->
                        List.range (min startY y) (max startY y)
                            |> List.map (Tuple.pair thisX)
                    )
                |> List.foldl (\( thisX, thisY ) -> Layout.Grid.set thisX thisY newVal) layer
    in
    updateLayer selectedLayer newLayer layers


updateLayer : LayerID -> Layer -> Layers -> Layers
updateLayer id updatedLayer layers =
    case id of
        Nwell ->
            { layers | nwell = updatedLayer }

        Ndiff ->
            { layers | ndiff = updatedLayer }

        Pdiff ->
            { layers | pdiff = updatedLayer }

        Metal ->
            { layers | metal = updatedLayer }

        Polysilicon ->
            { layers | poly = updatedLayer }

        Contacts ->
            { layers | contacts = updatedLayer }


labelTool : ToolFunction
labelTool =
    { dragDown = always <| always identity
    , dragMove = always <| always identity
    , dragUp =
        \x y model ->
            case model.tool of
                TypingLabel label ->
                    { model
                        | tool = Drawing Nothing
                        , labels =
                            case String.filter Char.isAlphaNum label of
                                "" ->
                                    model.labels

                                cleanLabel ->
                                    Dict.insert cleanLabel ( x, y ) model.labels
                    }

                _ ->
                    model
    , update =
        \msg model ->
            case msg of
                ChangeLabel str ->
                    { model | tool = TypingLabel (String.filter Char.isAlphaNum str) }

                _ ->
                    model
    }
