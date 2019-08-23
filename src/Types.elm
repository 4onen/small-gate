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
