module Types exposing (..)

import Dict exposing (Dict)
import Grid exposing (Grid)


type LayerID
    = Nwell
    | Ndiff
    | Pdiff
    | Metal
    | Polysilicon
    | Contacts


layerIDs : List LayerID
layerIDs =
    [ Nwell, Ndiff, Pdiff, Metal, Polysilicon, Contacts ]


type alias Layer =
    Grid


type alias Layers =
    { nwell : Layer
    , ndiff : Layer
    , pdiff : Layer
    , metal : Layer
    , poly : Layer
    , contacts : Layer
    }


funcFromID : LayerID -> (Layers -> Layer)
funcFromID l =
    case l of
        Nwell ->
            .nwell

        Ndiff ->
            .ndiff

        Pdiff ->
            .pdiff

        Metal ->
            .metal

        Polysilicon ->
            .poly

        Contacts ->
            .contacts


type alias Drag =
    { start : ( Int, Int )
    , curr : ( Int, Int )
    }


type Tool
    = DrawTool LayerID
    | LabelTool


type ToolStatus
    = Drawing LayerID (Maybe Drag)
    | TypingLabel String (Maybe ( Int, Int ))


type View
    = LabelsView
    | LayerView LayerID
    | LabelConnec String


defaultViews : List View
defaultViews =
    LabelsView :: List.map LayerView layerIDs


type alias Model =
    { layers : Layers
    , labels : Dict String ( Int, Int )
    , tool : ToolStatus
    , views : List View
    }


type Msg
    = DragDown Int Int
    | DragMove Int Int
    | DragUp Int Int
    | PickTool Tool
    | ChangeLabel String
    | RemoveLabel String
    | ToggleView View
    | Noop


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
        (Drawing Nwell Nothing)
        defaultViews
