module Strand exposing (..)


type Strand bead
    = Single bead
    | Series (List (Strand bead))
    | Parallel (List (Strand bead))


map : (a -> b) -> Strand a -> Strand b
map f sa =
    case sa of
        Single a ->
            Single <| f a

        Series ls ->
            Series <| List.map (map f) ls

        Parallel ls ->
            Parallel <| List.map (map f) ls


fold : { single : a -> b, series : List b -> b, parallel : List b -> b } -> Strand a -> b
fold functions strand =
    case strand of
        Single a ->
            functions.single a

        Series ls ->
            ls
                |> List.map (fold functions)
                |> functions.series

        Parallel ls ->
            ls
                |> List.map (fold functions)
                |> functions.parallel


reverse : Strand a -> Strand a
reverse =
    fold
        { single = Single
        , series = Parallel
        , parallel = Series
        }
