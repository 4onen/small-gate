module GateSchematic.Delay exposing (..)

import GateSchematic.Delay.CurrentPath exposing (..)
import GateSchematic.Logic
import GateSchematic.Types exposing (Transistor, TransistorKind(..), Width)
import List.Extra
import Set exposing (Set)
import Utils.Strand as Strand exposing (Alignment(..))


computeRiseFall : Alignment Transistor -> Float -> ( List ( Set String, Float ), List ( Set String, Float ) )
computeRiseFall gate outputCapacitance =
    let
        nmos =
            Strand.map (Tuple.mapSecond Tuple.second) gate

        pmos =
            Strand.map (Tuple.mapSecond Tuple.first) (Strand.reverse gate)

        rise =
            computeRise pmos nmos outputCapacitance

        fall =
            computeFall pmos nmos outputCapacitance
    in
    ( rise, fall )


computeRise : Alignment ( String, Float ) -> Alignment ( String, Float ) -> Float -> List ( Set String, Float )
computeRise pmos nmos outputCapacitance =
    let
        lactives =
            GateSchematic.Logic.activeSets (Strand.map Tuple.first pmos)
    in
    List.filterMap
        (\active ->
            let
                pmosSide : CurrentPath
                pmosSide =
                    risingCurrentPath PMOS active pmos
            in
            case pmosSide.currentCarrying of
                Nothing ->
                    Nothing

                Just r ->
                    let
                        nmosSide : CurrentPath
                        nmosSide =
                            risingCurrentPath NMOS active nmos

                        pmosParasitics =
                            pmosSide.delay (Just 0) Nothing

                        nmosParasitics =
                            nmosSide.delay pmosSide.currentCarrying Nothing

                        parasitics =
                            pmosParasitics + nmosParasitics

                        load =
                            outputCapacitance * r
                    in
                    Just ( active, load + parasitics )
        )
        lactives


computeFall : Alignment ( String, Float ) -> Alignment ( String, Float ) -> Float -> List ( Set String, Float )
computeFall pmos nmos outputCapacitance =
    let
        lactives : List (Set String)
        lactives =
            GateSchematic.Logic.activeSets (Strand.map Tuple.first nmos)
    in
    List.filterMap
        (\active ->
            let
                nmosSide : CurrentPath
                nmosSide =
                    fallingCurrentPath NMOS active nmos
            in
            case nmosSide.currentCarrying of
                Nothing ->
                    Nothing

                Just r ->
                    let
                        pmosSide : CurrentPath
                        pmosSide =
                            fallingCurrentPath PMOS active pmos

                        parasitic =
                            (+)
                                (pmosSide.delay Nothing nmosSide.currentCarrying)
                                (nmosSide.delay Nothing (Just 0))

                        load =
                            outputCapacitance * r
                    in
                    Just ( active, load + parasitic )
        )
        lactives


risingCurrentPath : TransistorKind -> Set String -> Alignment ( String, Float ) -> CurrentPath
risingCurrentPath tkind low =
    let
        lowactive =
            tkind == PMOS
    in
    Strand.fold
        { single =
            \( name, width ) ->
                let
                    isactive =
                        lowactive == Set.member name low
                in
                transistor isactive tkind width
        , strand = manySeries Rising
        , fray = manyParallel Rising
        }


fallingCurrentPath : TransistorKind -> Set String -> Alignment ( String, Float ) -> CurrentPath
fallingCurrentPath tkind high =
    let
        highactive =
            tkind == NMOS
    in
    Strand.fold
        { single =
            \( name, width ) ->
                let
                    isactive =
                        highactive == Set.member name high
                in
                transistor isactive tkind width
        , strand = manySeries Falling
        , fray = manyParallel Falling
        }
