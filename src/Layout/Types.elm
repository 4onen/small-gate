module Layout.Types exposing (Drag, Layer, LayerID(..), Layers, Model, Msg(..), Tool(..), View(..), funcFromID, layerIDs)

import Dict exposing (Dict)
import Layout.Grid exposing (Grid)


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
    = Drawing (Maybe Drag)
    | TypingLabel String


type View
    = LabelsView
    | LayerView LayerID
    | LabelConnec String


type alias Model =
    { layers : Layers
    , labels : Dict String ( Int, Int )
    , selectedLayer : LayerID
    , tool : Tool
    , views : List View
    }


type Msg
    = DragDown Int Int
    | DragMove Int Int
    | DragUp Int Int
    | PickTool Tool
    | PickLayer LayerID
    | ChangeLabel String
    | RemoveLabel String
    | ToggleView View
    | Noop
