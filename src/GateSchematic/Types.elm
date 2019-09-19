module GateSchematic.Types exposing (..)

import Strand exposing (Alignment)
import Strand.Pathed exposing (Path)


type alias Model =
    { labelToAdd : String
    , gate : Alignment Input
    , numInputsToShow : Maybe Int
    }


type Msg
    = Delete Path
    | ChangeLabel String
    | AddParallel Path
    | AddSeries Path
    | ToggleLogic
    | ChangeLogicInputs Int


type alias Width =
    Int


type alias Input =
    ( String, Width )


type TransistorKind
    = PMOS
    | NMOS
