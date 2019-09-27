module GateSchematic.Delay exposing (..)

import GateSchematic.Logic
import GateSchematic.Types exposing (Transistor, TransistorKind(..), Width)
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


type alias Path =
    { currentCarrying : Maybe Float
    , delay : Float -> Float
    , outputCapacitance : Float
    }


computeRise : Alignment ( String, Float ) -> Alignment ( String, Float ) -> Float -> List ( Set String, Float )
computeRise pmos nmos outputCapacitance =
    let
        lactives =
            GateSchematic.Logic.activeSets (Strand.map Tuple.first pmos)

        transistor active transistorKind width =
            let
                resistanceFactor =
                    case transistorKind of
                        PMOS ->
                            2.0

                        NMOS ->
                            1.0

                resistance =
                    if active then
                        Just <| resistanceFactor / width

                    else
                        Nothing
            in
            Path resistance (\beforeResistance -> beforeResistance * width) width

        wire =
            Path (Just 0) (always 0) 0

        open =
            Path Nothing (always 0) 0
    in
    List.filterMap
        (\active ->
            let
                pmosSide : Path
                pmosSide =
                    Strand.fold
                        { single =
                            \( name, width ) ->
                                transistor (Set.member name active) PMOS width
                        , strand =
                            List.foldl
                                (\new total ->
                                    case total.currentCarrying of
                                        Nothing ->
                                            Path Nothing total.delay new.outputCapacitance

                                        Just currentResistance ->
                                            Path
                                                (Maybe.map ((+) currentResistance) new.currentCarrying)
                                                (\beforeResistance ->
                                                    (+)
                                                        ((+) (total.delay beforeResistance) (total.outputCapacitance * (beforeResistance + currentResistance)))
                                                        (new.delay <| beforeResistance + currentResistance)
                                                )
                                                new.outputCapacitance
                                )
                                wire
                        , fray =
                            List.foldl
                                (\new total ->
                                    case total.currentCarrying of
                                        Nothing ->
                                            Path new.currentCarrying
                                                (\beforeResistance ->
                                                    (+)
                                                        (total.delay beforeResistance)
                                                        (new.delay beforeResistance)
                                                )
                                                (new.outputCapacitance + total.outputCapacitance)

                                        Just currentResistance ->
                                            let
                                                newResistance =
                                                    new.currentCarrying
                                                        |> Maybe.map (parallel currentResistance)
                                                        |> Maybe.withDefault currentResistance
                                            in
                                            Path
                                                (Just newResistance)
                                                (\before ->
                                                    (+)
                                                        (total.delay before)
                                                        (new.delay before)
                                                )
                                                (total.outputCapacitance + new.outputCapacitance)
                                )
                                open
                        }
                        pmos
            in
            pmosSide.currentCarrying
                |> Maybe.map
                    (\r ->
                        let
                            parasiticDelay =
                                r * pmosSide.outputCapacitance + pmosSide.delay 0

                            effortDelay =
                                r * outputCapacitance
                        in
                        ( active, parasiticDelay + effortDelay )
                    )
        )
        lactives


{-| The operation for combining resistors in parallel

Sometimes called the "reciprocal formula"

See: <https://en.wikipedia.org/wiki/Parallel_(operator)>

-}
parallel : Float -> Float -> Float
parallel a b =
    a * b / (a + b)
