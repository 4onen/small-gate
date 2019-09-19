module GateSchematic.Types exposing (..)

import Strand exposing (Alignment)


type alias Model =
    { labelToAdd : String
    , gate : Alignment Input
    }


type alias Input =
    String


type TransistorKind
    = PMOS
    | NMOS
