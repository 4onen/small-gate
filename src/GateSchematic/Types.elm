module GateSchematic.Types exposing (..)

import Strand exposing (Alignment)


type alias Model =
    { labelToAdd : String
    , gate : Alignment Input
    , numInputsToShow : Maybe Int
    }


type alias Width =
    Int


type alias Input =
    ( String, Width )


type TransistorKind
    = PMOS
    | NMOS
