module Tools exposing (..)

import Grid
import Types exposing (..)


updateDrawing : Msg -> LayerID -> Maybe Drag -> Model -> Model
updateDrawing msg layer mdrag model =
    case msg of
        DragDown x y ->
            { model | tool = Drawing layer <| Just (Drag ( x, y ) ( x, y )) }

        DragMove x y ->
            case mdrag of
                Nothing ->
                    model

                Just { start, curr } ->
                    { model | tool = Drawing layer <| Just (Drag start ( x, y )) }

        DragUp x y ->
            case mdrag of
                Nothing ->
                    model

                Just { start } ->
                    { model
                        | tool = Drawing layer Nothing
                        , layers = flipRect start ( x, y ) layer model.layers
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
