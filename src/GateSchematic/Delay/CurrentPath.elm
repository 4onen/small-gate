module GateSchematic.Delay.CurrentPath exposing (..)

import GateSchematic.Types exposing (TransistorKind(..))
import List.Extra


type alias CurrentPath =
    { currentCarrying : Maybe Float
    , delay : Maybe Float -> Maybe Float -> Float
    }


type CurrentDirection
    = Rising
    | Falling


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
                            let
                                ( top, far, bottom ) =
                                    delta2wye r rbefore rafter
                            in
                            width * ((top + far) + (bottom + far))

                        Nothing ->
                            (+)
                                (width * rbefore)
                                (width * rafter)
    in
    CurrentPath currentCarrying delay


maybeAdd : Maybe Float -> Maybe Float -> Maybe Float
maybeAdd =
    Maybe.map2 (+)


manyParallel : CurrentDirection -> List CurrentPath -> CurrentPath
manyParallel currentDirection lpaths =
    List.Extra.indexedFoldl
        (\i new total ->
            CurrentPath
                (maybeParallel new.currentCarrying total.currentCarrying)
                (let
                    otherPaths =
                        lpaths
                            |> List.Extra.removeAt i
                            |> List.foldl (.currentCarrying >> maybeParallel) Nothing
                 in
                 \mbefore mafter ->
                    (+)
                        (total.delay mbefore mafter)
                        (case ( mbefore, mafter ) of
                            ( Nothing, Nothing ) ->
                                0

                            ( Just rbefore, Nothing ) ->
                                new.delay (Just rbefore) (Maybe.map ((+) rbefore) otherPaths)

                            ( Nothing, Just rafter ) ->
                                new.delay (Maybe.map ((+) rafter) otherPaths) (Just rafter)

                            ( Just rbefore, Just rafter ) ->
                                case otherPaths of
                                    Just rparallel ->
                                        let
                                            ( top, far, bottom ) =
                                                delta2wye rparallel rbefore rafter
                                        in
                                        new.delay (Just (top + far)) (Just (bottom + far))

                                    Nothing ->
                                        new.delay (Just rbefore) (Just rafter)
                        )
                )
        )
        open
        lpaths


manySeries : CurrentDirection -> List CurrentPath -> CurrentPath
manySeries currentDirection =
    List.foldl
        (\new total ->
            CurrentPath
                (new.currentCarrying |> maybeAdd total.currentCarrying)
                (\mbefore mafter ->
                    (+)
                        (total.delay mbefore (mafter |> maybeAdd new.currentCarrying))
                        (new.delay (mbefore |> maybeAdd total.currentCarrying) mafter)
                )
        )
        wire


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


{-| Converts a delta formation of transistors into a wye

```
              top
              |  \ < topLeg
    nearLeg > |   far
              |  / < bottomLeg
              bottom
```

becomes

```
    top
      \
       O--far
      /
    bottom
```

-}
delta2wye : Float -> Float -> Float -> ( Float, Float, Float )
delta2wye nearLeg topLeg bottomLeg =
    let
        recipSum =
            1.0 / (nearLeg + topLeg + bottomLeg)
    in
    ( nearLeg * topLeg / recipSum
    , topLeg * bottomLeg / recipSum
    , nearLeg * bottomLeg / recipSum
    )
