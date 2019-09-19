module GateSchematic.RandomColor exposing (fromString)

import Element
import Random exposing (..)


fromInt : Int -> Element.Color
fromInt =
    Random.initialSeed
        >> Random.step
            (Random.map3 Element.rgb
                (Random.float 0.2 0.9)
                (Random.float 0.2 0.9)
                (Random.float 0.2 0.9)
            )
        >> Tuple.first


fromString : String -> Element.Color
fromString =
    String.toList
        >> List.map Char.toCode
        >> List.sum
        >> fromInt
