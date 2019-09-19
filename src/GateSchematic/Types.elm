module GateSchematic.Types exposing (..)

import Strand exposing (Alignment)


type alias Model =
    { labelToAdd : String
    , gate : Alignment Input
    , numInputsToShow : Maybe Int
    }


type alias Input =
    String


type TransistorKind
    = PMOS
    | NMOS
