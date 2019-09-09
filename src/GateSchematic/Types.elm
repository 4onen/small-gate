module GateSchematic.Types exposing (..)

import Strand exposing (Strand)


type alias Model =
    { gate : Gate Input }


type alias Input =
    String


type alias Gate transistor =
    { pmos : Strand transistor, nmos : Strand transistor }


type TransistorKind
    = PMOS
    | NMOS
