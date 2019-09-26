module GateSchematic.Delay exposing (..)

import GateSchematic.Logic
import GateSchematic.Types exposing (Transistor, TransistorKind(..))
import List.Extra
import Set exposing (Set)
import Strand exposing (Alignment(..))


computeRiseFall : Alignment Transistor -> Float -> ( List ( Set String, Float ), List ( Set String, Float ) )
computeRiseFall gate outputCapacitance =
    let
        nmos =
            Strand.map (Tuple.mapSecond Tuple.second) gate

        pmos =
            Strand.map (Tuple.mapSecond Tuple.first) (Strand.reverse gate)

        rise =
            computeRise pmos nmos outputCapacitance
    in
    ( rise, rise )


computeRise : Alignment ( String, Float ) -> Alignment ( String, Float ) -> Float -> List ( Set String, Float )
computeRise pmos nmos outputCapacitance =
    let
        lactives =
            GateSchematic.Logic.activeSets (Strand.map Tuple.first pmos)

        transistor transistorKind width =
            let
                resistanceFactor =
                    case transistorKind of
                        PMOS ->
                            2.0

                        NMOS ->
                            1.0

                resistance =
                    resistanceFactor / width
            in
            { resistance = resistance
            , delay =
                \beforeResistance otherPathsAfter ->
                    case otherPathsAfter of
                        Just otherPath ->
                            beforeResistance * width + otherPath * width

                        Nothing ->
                            beforeResistance * width + (beforeResistance + resistance) * width
            }

        wire =
            { resistance = 0, delay = always always 0 }

        open =
            { resistance = 1 / 0, delay = always always (1 / 0) }
    in
    List.map
        (\active ->
            let
                pmosSide =
                    pmos
                        |> Strand.fold
                            { single =
                                \( name, width ) ->
                                    if Set.member name active then
                                        Just <| transistor PMOS width

                                    else
                                        Nothing
                            , strand =
                                List.foldl
                                    (Maybe.map2
                                        (\new total ->
                                            { resistance = total.resistance + new.resistance
                                            , delay =
                                                \beforeResistance otherPathsAfter ->
                                                    total.delay beforeResistance Nothing + new.delay (total.resistance + beforeResistance) otherPathsAfter
                                            }
                                        )
                                    )
                                    (Just wire)
                            , fray =
                                List.foldl
                                    (\mnew mtotal ->
                                        case ( mnew, mtotal ) of
                                            ( Just new, Nothing ) ->
                                                mnew

                                            ( Nothing, Just total ) ->
                                                mtotal

                                            ( Nothing, Nothing ) ->
                                                Nothing

                                            ( Just new, Just total ) ->
                                                Just
                                                    { resistance = parallel new.resistance total.resistance
                                                    , delay =
                                                        \beforeResistance _ ->
                                                            Basics.min
                                                                (new.delay beforeResistance Nothing)
                                                                (total.delay beforeResistance Nothing)
                                                    }
                                    )
                                    Nothing
                                    >> Maybe.map
                                        (\({ resistance, delay } as paths) ->
                                            { paths
                                                | delay =
                                                    \beforeResistance _ ->
                                                        paths.delay beforeResistance (Just <| beforeResistance + paths.resistance)
                                            }
                                        )
                            }
                        |> Maybe.withDefault open

                parasiticDelay =
                    pmosSide.delay 0 Nothing

                effortDelay =
                    pmosSide.resistance * outputCapacitance
            in
            ( active, parasiticDelay + effortDelay )
        )
        lactives


{-| The operation for combining resistors in parallel

Sometimes called the "reciprocal formula"

See: <https://en.wikipedia.org/wiki/Parallel_(operator)>

-}
parallel : Float -> Float -> Float
parallel a b =
    a * b / (a + b)
