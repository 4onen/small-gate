module Layout exposing (init, update, view)

import Dict
import Layout.Grid as Grid
import Layout.Tools as Tools exposing (..)
import Layout.Types exposing (..)
import Layout.Views exposing (defaultViews)
import Layout.Workspace exposing (view)


init : Model
init =
    let
        metal =
            List.foldl
                (Grid.fromList >> Grid.union)
                (Grid.fromList [ ( 0, 0 ), ( 0, -1 ), ( 0, 4 ), ( 0, 5 ) ])
                [ List.range 0 4
                    |> List.concatMap (\x -> List.map (Tuple.pair x) [ -2, 6 ])
                , List.range 0 4
                    |> List.map (Tuple.pair 4)
                ]

        layers =
            { nwell =
                List.range 0 4
                    |> List.concatMap (\x -> List.map (Tuple.pair x) (List.range -2 1))
                    |> Grid.fromList
            , ndiff =
                List.range 0 4
                    |> List.map (\x -> Tuple.pair x 4)
                    |> Grid.fromList
            , pdiff =
                List.range 0 4
                    |> List.map (\x -> Tuple.pair x 0)
                    |> Grid.fromList
            , metal = metal
            , poly =
                List.range -1 5
                    |> List.map (Tuple.pair 2)
                    |> Grid.fromList
            , contacts = Grid.fromList [ ( 0, 0 ), ( 4, 0 ), ( 0, 4 ), ( 4, 4 ) ]
            }
    in
    Model
        layers
        Dict.empty
        Metal
        (Drawing Nothing)
        defaultViews


update : Msg -> Model -> Model
update msg model =
    case msg of
        PickTool tool ->
            { model | tool = tool }

        PickLayer id ->
            { model | selectedLayer = id, tool = Drawing Nothing }

        ToggleView v ->
            { model
                | views =
                    if List.member v model.views then
                        List.filter (\e -> v /= e) model.views

                    else
                        v :: model.views
            }

        RemoveLabel label ->
            { model | labels = Dict.remove label model.labels }

        DragDown x y ->
            (.dragDown <| Tools.toolFunction model.tool) x y model

        DragMove x y ->
            (.dragMove <| Tools.toolFunction model.tool) x y model

        DragUp x y ->
            (.dragUp <| Tools.toolFunction model.tool) x y model

        ChangeLabel _ ->
            (.update <| Tools.toolFunction model.tool) msg model

        Noop ->
            (.update <| Tools.toolFunction model.tool) msg model


view =
    Layout.Workspace.view
