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

        fall =
            computeFall pmos nmos outputCapacitance
    in
    ( rise, fall )


type alias CurrentPath =
    { currentCarrying : Maybe Float
    , delay : Maybe Float -> Maybe Float -> Float
    }


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
                    currentPath PMOS False active pmos

                nmosSide : CurrentPath
                nmosSide =
                    currentPath NMOS False active nmos

                parasitic =
                    pmosSide.delay (Just 0) Nothing

                load =
                    Maybe.map ((*) outputCapacitance) pmosSide.currentCarrying
            in
            Maybe.map ((+) parasitic >> Tuple.pair active) load
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
                pmosSide : CurrentPath
                pmosSide =
                    currentPath PMOS True active pmos

                nmosSide : CurrentPath
                nmosSide =
                    currentPath NMOS True active nmos

                parasitic =
                    nmosSide.delay (Just 0) Nothing

                load =
                    Maybe.map ((*) outputCapacitance) nmosSide.currentCarrying
            in
            Maybe.map ((+) parasitic >> Tuple.pair active) load
        )
        lactives


currentPath : TransistorKind -> Bool -> Set String -> Alignment ( String, Float ) -> CurrentPath
currentPath tkind reverse active =
    Strand.fold
        { single =
            \( name, width ) ->
                transistor (Set.member name active) tkind width
        , strand =
            (if reverse then
                List.reverse

             else
                identity
            )
                >> List.foldl
                    (\new total ->
                        CurrentPath
                            (Maybe.map2 (+) total.currentCarrying new.currentCarrying)
                            (\mbefore mafter ->
                                (+)
                                    (total.delay mbefore (Maybe.map2 (+) new.currentCarrying mafter))
                                    (new.delay (Maybe.map2 (+) total.currentCarrying mbefore) mafter)
                            )
                    )
                    wire
        , fray =
            \lp ->
                List.Extra.indexedFoldl
                    (\i new total ->
                        CurrentPath
                            (maybeParallel new.currentCarrying total.currentCarrying)
                            (let
                                otherPaths =
                                    lp
                                        |> List.Extra.removeAt i
                                        |> List.foldl (.currentCarrying >> maybeParallel) Nothing
                             in
                             \mbefore mafter ->
                                (+)
                                    (total.delay mbefore mafter)
                                    (new.delay mbefore (maybeParallel otherPaths mafter))
                            )
                    )
                    open
                    lp
        }


wire =
    CurrentPath (Just 0) (always (always 0))


open =
    CurrentPath Nothing (always (always 0))


transistor : Bool -> TransistorKind -> Float -> CurrentPath
transistor active transistorKind width =
    let
        resistanceFactor =
            case transistorKind of
                PMOS ->
                    2.0

                NMOS ->
                    1.0

        currentCarrying =
            if active then
                Just <| resistanceFactor / width

            else
                Nothing

        delay mbefore mafter =
            case ( mbefore, mafter ) of
                ( Nothing, Nothing ) ->
                    0

                ( Just rbefore, Nothing ) ->
                    case currentCarrying of
                        Just r ->
                            (rbefore * width) + ((rbefore + r) * width)

                        Nothing ->
                            rbefore * width

                ( Nothing, Just rafter ) ->
                    case currentCarrying of
                        Just r ->
                            ((rafter + r) * width) + (rafter * width)

                        Nothing ->
                            rafter * width

                ( Just rbefore, Just rafter ) ->
                    case currentCarrying of
                        Just r ->
                            (+)
                                (width * parallel rbefore (rafter + r))
                                (width * parallel (rbefore + r) rafter)

                        Nothing ->
                            (+)
                                (width * rbefore)
                                (width * rafter)
    in
    CurrentPath currentCarrying delay


{-| The operation for combining resistors in parallel

Sometimes called the "reciprocal formula"

See: <https://en.wikipedia.org/wiki/Parallel_(operator)>

-}
parallel : Float -> Float -> Float
parallel a b =
    a * b / (a + b)


maybeParallel : Maybe Float -> Maybe Float -> Maybe Float
maybeParallel ma mb =
    case ( ma, mb ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Just a, Nothing ) ->
            Just a

        ( Nothing, Just b ) ->
            Just b

        ( Just a, Just b ) ->
            Just <| parallel a b
