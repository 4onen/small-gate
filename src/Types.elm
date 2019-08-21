module Types exposing (..)

import Dict exposing (Dict)
import Grid exposing (Grid)


type LayerID
    = Diffusion
    | NMOS
    | PMOS
    | Metal
    | Polysilicon
    | Contacts


layerIDs : List LayerID
layerIDs =
    [ Diffusion, NMOS, PMOS, Metal, Polysilicon, Contacts ]


type alias Layer =
    Grid


type alias Layers =
    { diffusion : Layer
    , nmos : Layer
    , pmos : Layer
    , metal : Layer
    , poly : Layer
    , contacts : Layer
    }


funcFromID : LayerID -> (Layers -> Layer)
funcFromID l =
    case l of
        Diffusion ->
            .diffusion

        NMOS ->
            .nmos

        PMOS ->
            .pmos

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


type alias Model =
    { layers : Layers
    , labels : Dict String ( Int, Int )
    , tool : ToolStatus
    }


init : Model
init =
    let
        g =
            Grid.empty
    in
    Model (Layers g g g g g g) Dict.empty (Drawing Diffusion Nothing)


type Msg
    = DragDown Int Int
    | DragMove Int Int
    | DragUp Int Int
    | PickTool Tool
    | ChangeLabel String
    | RemoveLabel String
    | Noop
