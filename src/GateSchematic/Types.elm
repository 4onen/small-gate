module GateSchematic.Types exposing (..)

import Strand exposing (Alignment)
import Strand.Pathed exposing (Path)


type alias Model =
    { labelToAdd : String
    , gate : Alignment Input
    , showNumLogicInputs : Maybe Int
    , showDelays : Bool
    }


type Msg
    = Delete Path
    | ChangeLabel String
    | AddParallel Path
    | AddSeries Path
    | ToggleLogic
    | ChangeLogicInputs Int
    | ToggleDelays


type alias Width =
    Int


type alias Input =
    ( String, ( Width, Width ) )


type TransistorKind
    = PMOS
    | NMOS
