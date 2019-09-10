module GateSchematic.Types exposing (..)

import Strand exposing (Strand)


type alias Model =
    Strand Input


type alias Input =
    String


type TransistorKind
    = PMOS
    | NMOS
