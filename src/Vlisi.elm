module Vlisi exposing (main)

import Browser
import Element
import Grid
import Render exposing (..)
import Types exposing (..)


main =
    Browser.document
        { init = \() -> ( init, Cmd.none )
        , view = \model -> { title = "VLISI", body = [ Element.layout [] <| view model ] }
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        PickLayer layer ->
            { model | selectedLayer = layer }

        DragDown x y ->
            { model | mdrag = Just (Drag ( x, y ) ( x, y )) }

        DragMove x y ->
            case model.mdrag of
                Nothing ->
                    model

                Just { start, curr } ->
                    { model | mdrag = Just (Drag start ( x, y )) }

        DragUp x y ->
            case model.mdrag of
                Nothing ->
                    model

                Just { start } ->
                    { model | mdrag = Nothing, layers = flipRect start ( x, y ) model.selectedLayer model.layers }


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
