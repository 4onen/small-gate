module GateSchematic.Types exposing (..)

import Utils.Either exposing (Either)
import Utils.Strand exposing (Alignment)
import Utils.Strand.Pathed exposing (Path)


type alias Model =
    { label : Either String Path
    , gate : CMOS
    , clickTrash : Bool
    , showNumLogicInputs : Maybe Int
    , showDelays : Bool
    }


type Msg
    = Select Path
    | ChangeLabel String
    | AddParallel Path
    | AddSeries Path
    | ToggleClickTrash
    | ToggleLogic
    | ChangeLogicInputs Int
    | ToggleDelays
    | SolveDelays


type alias Width =
    Float


type alias Transistor =
    ( String, ( Width, Width ) )


type alias CMOS =
    Alignment Transistor


type TransistorKind
    = PMOS
    | NMOS
