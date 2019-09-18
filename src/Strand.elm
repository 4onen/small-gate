module Strand exposing (..)


type Either a b
    = Left a
    | Right b


type Strand bead
    = Single (Either (Fray bead) bead)
    | Series (Either (Fray bead) bead) (Strand bead)


type Fray bead
    = Loose (Either (Strand bead) bead)
    | Parallel (Either (Strand bead) bead) (Fray bead)


map : (a -> b) -> Strand a -> Strand b
map f strand =
    case strand of
        Single below ->
            Single
                (mapEither (mapFray f) f below)

        Series below rest ->
            Series
                (mapEither (mapFray f) f below)
                (map f rest)


mapFray : (a -> b) -> Fray a -> Fray b
mapFray f fray =
    case fray of
        Loose below ->
            Loose (mapEither (map f) f below)

        Parallel below rest ->
            Parallel
                (mapEither (map f) f below)
                (mapFray f rest)


mapEither : (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither fl fr either =
    case either of
        Left vl ->
            Left (fl vl)

        Right vr ->
            Right (fr vr)


foldEither : (a -> c) -> (b -> c) -> Either a b -> c
foldEither fl fr either =
    case either of
        Left vl ->
            fl vl

        Right vr ->
            fr vr


fold : { single : a -> b, loose : a -> b, series : List b -> b, parallel : List b -> b } -> Strand a -> b
fold functions strand =
    case strand of
        Single e ->
            foldEither (foldFray functions) functions.single e

        Series _ _ ->
            strand
                |> listifyStrand (foldFray functions) functions.single
                |> functions.series


foldFray : { single : a -> b, loose : a -> b, series : List b -> b, parallel : List b -> b } -> Fray a -> b
foldFray functions fray =
    case fray of
        Loose e ->
            foldEither (fold functions) functions.loose e

        Parallel _ _ ->
            fray
                |> listifyFray (fold functions) functions.loose
                |> functions.parallel


listifyStrand : (Fray a -> b) -> (a -> b) -> Strand a -> List b
listifyStrand fixFray fixSingle strand =
    case strand of
        Single below ->
            List.singleton (foldEither fixFray fixSingle below)

        Series below rest ->
            foldEither fixFray fixSingle below :: listifyStrand fixFray fixSingle rest


listifyFray : (Strand a -> b) -> (a -> b) -> Fray a -> List b
listifyFray fixStrand fixLoose strand =
    case strand of
        Loose below ->
            List.singleton (foldEither fixStrand fixLoose below)

        Parallel below rest ->
            foldEither fixStrand fixLoose below :: listifyFray fixStrand fixLoose rest
