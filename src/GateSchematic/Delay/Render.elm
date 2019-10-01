module GateSchematic.Delay.Render exposing (view)

import Dict exposing (Dict)
import Element exposing (..)
import GateSchematic.Delay
import GateSchematic.Delay.Capacitance
import Set exposing (Set)


view gate =
    GateSchematic.Delay.computeRiseFall gate 12.0
        |> Tuple.mapBoth
            (List.map (viewDelay True >> text) >> (::) (text "Rise times:"))
            (List.map (viewDelay False >> text) >> (::) (text "Fall times:"))
        |> (\( a, b ) -> [ column [] a, column [] b ])
        |> (::) (viewInputCapacitance gate)


viewDelay : Bool -> ( Set String, Float ) -> String
viewDelay inverted ( vals, delay ) =
    (vals
        |> Set.toList
        |> (if inverted then
                List.map
                    (\s ->
                        if String.endsWith "'" s then
                            String.dropRight 1 s

                        else
                            s ++ "'"
                    )

            else
                identity
           )
        |> List.intersperse ", "
    )
        ++ [ ": "
           , delay |> String.fromFloat
           ]
        |> String.concat


viewInputCapacitance gate =
    GateSchematic.Delay.Capacitance.input gate
        |> Dict.toList
        |> List.map (\( name, capacitance ) -> name ++ ": " ++ String.fromFloat capacitance)
        |> (::) "Input capacitances: "
        |> List.map text
        |> column []
