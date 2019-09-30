module Widths exposing (solve)

import Either exposing (Either(..))
import GateSchematic.Types exposing (Transistor)
import Strand exposing (Alignment(..), Fray(..), Strand(..))


{-| Will estimate ideal transistor widths for equal rise/fall times in a static CMOS gate.

Ignores parasitic effects.

-}
solve : Alignment String -> Alignment Transistor
solve =
    Strand.fold
        { single =
            \s -> Single (Tuple.pair s ( toFloat 2, toFloat 1 ))
        , strand =
            \l ->
                let
                    multiplier =
                        toFloat <| List.length l

                    multiplyWidth =
                        Tuple.mapSecond (Tuple.mapBoth ((*) multiplier) identity)
                in
                l
                    |> List.concatMap
                        (\e ->
                            case e of
                                Single a ->
                                    [ Right (multiplyWidth a) ]

                                Parallel fray ->
                                    fray
                                        |> Strand.mapFray multiplyWidth
                                        |> Left
                                        |> List.singleton

                                Series (Strand strand) ->
                                    List.map (Either.map (Strand.mapFray multiplyWidth) multiplyWidth) strand
                        )
                    |> Strand
                    |> Series
        , fray =
            \l ->
                let
                    multiplier =
                        toFloat <| List.length l

                    multiplyWidth =
                        Tuple.mapSecond (Tuple.mapBoth identity ((*) multiplier))
                in
                l
                    |> List.concatMap
                        (\e ->
                            case e of
                                Single a ->
                                    [ Right (multiplyWidth a) ]

                                Parallel (Fray fray) ->
                                    List.map (Either.map (Strand.mapStrand multiplyWidth) multiplyWidth) fray

                                Series strand ->
                                    strand
                                        |> Strand.mapStrand multiplyWidth
                                        |> Left
                                        |> List.singleton
                        )
                    |> Fray
                    |> Parallel
        }
