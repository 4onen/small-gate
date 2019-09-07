module Vlisi exposing (main)

import Browser
import Dict
import Element
import Grid
import Tools exposing (..)
import Types exposing (..)
import Views exposing (defaultViews)
import Workspace exposing (view)


main =
    Browser.document
        { init = \() -> ( init, Cmd.none )
        , view = \model -> { title = "VLISI", body = [ Element.layout [] <| view model ] }
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


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

        _ ->
            case model.tool of
                Drawing mdrag ->
                    updateDrawing msg mdrag model

                TypingLabel label ->
                    case msg of
                        ChangeLabel str ->
                            { model | tool = TypingLabel (String.filter Char.isAlphaNum str) }

                        DragUp x y ->
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
