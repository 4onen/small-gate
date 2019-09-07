module Tools exposing (ToolFunctions, flipRect, updateDrawing, updateLayer)

import Grid
import Types exposing (..)


type alias ToolFunctions =
    { dragDown : Int -> Int -> Model -> Model
    , dragMove : Int -> Int -> Model -> Model
    , dragUp : Int -> Int -> Model -> Model
    , update : Msg -> Model -> Model
    }


updateDrawing : Msg -> Maybe Drag -> Model -> Model
updateDrawing msg mdrag model =
    case msg of
        DragDown x y ->
            { model | tool = Drawing <| Just (Drag ( x, y ) ( x, y )) }

        DragMove x y ->
            case mdrag of
                Nothing ->
                    model

                Just { start, curr } ->
                    { model | tool = Drawing <| Just (Drag start ( x, y )) }

        DragUp x y ->
            case mdrag of
                Nothing ->
                    model

                Just { start } ->
                    { model
                        | tool = Drawing Nothing
                        , layers = flipRect start ( x, y ) model.selectedLayer model.layers
                    }

        _ ->
            model


flipRect : ( Int, Int ) -> ( Int, Int ) -> LayerID -> Layers -> Layers
flipRect ( startX, startY ) ( x, y ) selectedLayer layers =
    let
        layer =
            funcFromID selectedLayer layers

        newVal =
            layer
                |> Grid.get startX startY
                |> Basics.not

        newLayer =
            List.range (min startX x) (max startX x)
                |> List.concatMap
                    (\thisX ->
                        List.range (min startY y) (max startY y)
                            |> List.map (Tuple.pair thisX)
                    )
                |> List.foldl (\( thisX, thisY ) -> Grid.set thisX thisY newVal) layer
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
