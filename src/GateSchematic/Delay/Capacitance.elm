module GateSchematic.Delay.Capacitance exposing (input)

import Dict exposing (Dict)
import GateSchematic.Types exposing (Transistor, Width)
import Strand exposing (Alignment(..))


input : Alignment Transistor -> Dict String Width
input =
    let
        sum_dicts =
            List.foldl
                (Dict.merge
                    Dict.insert
                    (\key a b -> Dict.insert key (a + b))
                    Dict.insert
                    Dict.empty
                )
                Dict.empty
    in
    Strand.fold
        { single = \( name, ( pmos, nmos ) ) -> Dict.singleton name (pmos + nmos)
        , strand = sum_dicts
        , fray = sum_dicts
        }
