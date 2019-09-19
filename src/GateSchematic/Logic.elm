module GateSchematic.Logic exposing (..)

import Set exposing (Set)
import Strand exposing (Alignment(..))


toText : Alignment String -> String
toText =
    Strand.fold
        { single = identity
        , strand =
            \is ->
                "(" ++ (is |> List.sort |> List.intersperse "∗" |> String.concat) ++ ")"
        , fray =
            \is ->
                "(" ++ (is |> List.sort |> List.intersperse "+" |> String.concat) ++ ")"
        }
        << Strand.reverse


retrieveInputs : Alignment String -> Set String
retrieveInputs =
    Strand.fold
        { single = Set.singleton
        , strand = List.foldl Set.union Set.empty
        , fray = List.foldl Set.union Set.empty
        }


simulate : Set comparable -> Alignment comparable -> Bool
simulate activeInputs =
    Strand.fold
        { single = \i -> Set.member i activeInputs
        , strand = List.all identity
        , fray = List.any identity
        }
        >> Basics.not
