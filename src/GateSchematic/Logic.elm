module GateSchematic.Logic exposing (..)

import Strand exposing (Alignment(..))


toText : Alignment String -> String
toText =
    Strand.fold
        { single = identity
        , strand =
            \is ->
                "(" ++ (is |> List.intersperse "." |> String.concat) ++ ")"
        , fray =
            \is ->
                "(" ++ (is |> List.intersperse "+" |> String.concat) ++ ")"
        }
