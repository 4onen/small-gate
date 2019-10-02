module GateSchematic.Logic exposing (..)

import Bitwise
import List.Extra
import Set exposing (Set)
import Utils.Strand as Strand exposing (Alignment(..))


cleanLabel : String -> String
cleanLabel str =
    let
        valid =
            String.filter Char.isAlphaNum str

        invert =
            if
                str
                    |> String.filter ((==) '\'')
                    |> String.length
                    |> Bitwise.and 1
                    |> (==) 1
            then
                "'"

            else
                ""
    in
    valid ++ invert


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


retrieveInputs : Alignment String -> Set String
retrieveInputs =
    Strand.fold
        { single =
            String.filter ((/=) '\'')
                >> (\s ->
                        if s /= "" then
                            s

                        else
                            "_"
                   )
                >> Set.singleton
        , strand = List.foldl Set.union Set.empty
        , fray = List.foldl Set.union Set.empty
        }


simulate : Set String -> Alignment String -> Bool
simulate activeInputs =
    Strand.fold
        { single =
            \i ->
                let
                    inverted =
                        i |> String.filter ((==) '\'') |> String.length |> Bitwise.and 1 |> (==) 1

                    input =
                        i
                            |> String.filter ((/=) '\'')
                            |> (\s ->
                                    if s /= "" then
                                        s

                                    else
                                        "_"
                               )
                in
                Set.member input activeInputs
                    |> xor inverted
        , strand = List.all identity
        , fray = List.any identity
        }
        >> Basics.not


{-| Retrieve the minimum sets of inputs that must be off to activate the PMOS side of a gate.

Unions of these sets should also produce active sets.

    activeSets (Parallel (Fray [ Right "A", Right "B" ])) == [ Set.fromList [ "A" ], Set.fromList [ "B" ] ]

Note that `Set.fromList ["A","B"]` was not included, as that is not a minimum set of active inputs to trigger the output rising to high.

-}
activeSets : Alignment String -> List (Set String)
activeSets =
    Strand.fold
        { single = Set.singleton >> List.singleton
        , strand = List.Extra.cartesianProduct >> List.map (List.foldl Set.union Set.empty)
        , fray = List.concat
        }
