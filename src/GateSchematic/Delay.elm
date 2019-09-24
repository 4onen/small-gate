module GateSchematic.Delay exposing (..)

import GateSchematic.Logic
import GateSchematic.Types
import List.Extra
import Set exposing (Set)
import Strand exposing (Alignment(..))


computeRiseFall : Alignment ( String, ( Int, Int ) ) -> Float -> ( List ( Set String, Float ), List ( Set String, Float ) )
computeRiseFall gate outputCapacitance =
    let
        nmos =
            Strand.map (Tuple.mapSecond (Tuple.second >> toFloat)) gate

        pmos =
            Strand.map (Tuple.mapSecond (Tuple.first >> toFloat)) (Strand.reverse gate)

        rise =
            computeRise pmos nmos outputCapacitance
    in
    ( rise, rise )


computeRise : Alignment ( String, Float ) -> Alignment ( String, Float ) -> Float -> List ( Set String, Float )
computeRise pmos nmos outputCapacitance =
    let
        lactives =
            GateSchematic.Logic.activeSets (Strand.map Tuple.first pmos)

        widthFactor =
            2.0

        transistor width =
            { resistance = widthFactor / width, delay = \beforeResistance -> beforeResistance * width + (beforeResistance + width) * width }

        wire =
            { resistance = 0, delay = always 0 }
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
                                        Just <| transistor width

                                    else
                                        Nothing
                            , strand =
                                List.foldl
                                    (\mnew mtotal ->
                                        case ( mnew, mtotal ) of
                                            ( Just new, Just total ) ->
                                                Just
                                                    { resistance = total.resistance + new.resistance
                                                    , delay =
                                                        \beforeResistance ->
                                                            total.delay beforeResistance + new.delay (total.resistance + beforeResistance)
                                                    }

                                            _ ->
                                                Nothing
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
                                                    { resistance = 1.0 / (1.0 / new.resistance + 1.0 / total.resistance)
                                                    , delay =
                                                        \beforeResistance ->
                                                            Basics.min
                                                                (new.delay beforeResistance)
                                                                (total.delay beforeResistance)
                                                    }
                                    )
                                    Nothing
                            }
                        |> Maybe.withDefault wire

                parasiticDelay =
                    pmosSide.delay 0

                effortDelay =
                    pmosSide.resistance * outputCapacitance
            in
            ( active, parasiticDelay + effortDelay )
         --TODO: Add NMOS parasitics
        )
        lactives
