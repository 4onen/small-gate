module GateSchematic.Types exposing (..)

import Strand exposing (Alignment)


type alias Model =
    Alignment Input


type alias Input =
    String


type TransistorKind
    = PMOS
    | NMOS
