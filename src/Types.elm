module Types exposing (..)

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


type alias Model =
    { layers : Layers
    , selectedLayer : LayerID
    , mdrag : Maybe Drag
    }


init : Model
init =
    let
        g =
            Grid.empty
    in
    Model (Layers g g g g g g) Diffusion Nothing


type Msg
    = DragDown Int Int
    | DragMove Int Int
    | DragUp Int Int
    | PickLayer LayerID
    | Noop
