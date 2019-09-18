module Strand exposing (..)

import Either exposing (Either(..))


type Strand bead
    = Strand (List (Either (Fray bead) bead))


type Fray bead
    = Fray (List (Either (Strand bead) bead))


type Alignment bead
    = Series (Strand bead)
    | Single bead
    | Parallel (Fray bead)


map : (a -> b) -> Strand a -> Strand b
map f (Strand strand) =
    Strand (List.map (Either.map (mapFray f) f) strand)


mapFray : (a -> b) -> Fray a -> Fray b
mapFray f (Fray fray) =
    Fray (List.map (Either.map (map f) f) fray)


fold : { single : a -> b, strand : List b -> b, fray : List b -> b } -> Strand a -> b
fold functions (Strand strand) =
    strand
        |> List.map (Either.fold (foldFray functions) functions.single)
        |> functions.strand


foldFray : { single : a -> b, strand : List b -> b, fray : List b -> b } -> Fray a -> b
foldFray functions (Fray fray) =
    fray
        |> List.map (Either.fold (fold functions) functions.single)
        |> functions.fray


reverse : Alignment a -> Alignment a
reverse align =
    case align of
        Single _ ->
            align

        Series strand ->
            Parallel <| reverseStrand strand

        Parallel fray ->
            Series <| reverseFray fray


reverseStrand : Strand a -> Fray a
reverseStrand (Strand strand) =
    Fray (List.map (Either.map reverseFray identity) strand)


reverseFray : Fray a -> Strand a
reverseFray (Fray fray) =
    Strand (List.map (Either.map reverseStrand identity) fray)
