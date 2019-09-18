module Strand.Pathed exposing (Path, delete, empty, fold, map)

import Either exposing (Either(..))
import List.Extra
import Strand exposing (Alignment(..), Fray(..), Strand(..))


type alias Path =
    List Int


empty : Path
empty =
    []


delete : Path -> Alignment a -> Maybe (Alignment a)
delete path align =
    case align of
        Single _ ->
            Nothing

        Series strand ->
            deleteInStrand path strand

        Parallel fray ->
            deleteInFray path fray


deleteInStrand : Path -> Strand a -> Maybe (Alignment a)
deleteInStrand path (Strand strand) =
    let
        computeResultStrand : List (Either (Fray a) a) -> Maybe (Alignment a)
        computeResultStrand list =
            case list of
                [] ->
                    Nothing

                (Left fray) :: [] ->
                    Just (Parallel fray)

                (Right el) :: [] ->
                    Just (Single el)

                ls ->
                    Just (Series (Strand ls))

        deleteAt idx =
            List.Extra.removeAt idx >> computeResultStrand
    in
    case path of
        [] ->
            Nothing

        idx :: [] ->
            deleteAt idx strand

        idx :: remainingPath ->
            case List.Extra.getAt idx strand of
                Nothing ->
                    Just (Series (Strand strand))

                Just (Right _) ->
                    deleteAt idx strand

                Just (Left fray) ->
                    case deleteInFray remainingPath fray of
                        Nothing ->
                            deleteAt idx strand

                        Just alignment ->
                            Just <|
                                Series <|
                                    Strand <|
                                        case alignment of
                                            Single val ->
                                                strand |> List.Extra.setAt idx (Right val)

                                            Parallel val ->
                                                strand |> List.Extra.setAt idx (Left val)

                                            Series (Strand val) ->
                                                strand
                                                    |> List.Extra.removeAt idx
                                                    |> List.Extra.splitAt idx
                                                    |> (\( l, r ) -> l ++ val ++ r)


deleteInFray : Path -> Fray a -> Maybe (Alignment a)
deleteInFray path (Fray fray) =
    let
        computeResultFray : List (Either (Strand a) a) -> Maybe (Alignment a)
        computeResultFray list =
            case list of
                [] ->
                    Nothing

                (Left strand) :: [] ->
                    Just (Series strand)

                (Right el) :: [] ->
                    Just (Single el)

                ls ->
                    Just (Parallel (Fray ls))

        deleteAt idx =
            List.Extra.removeAt idx >> computeResultFray
    in
    case path of
        [] ->
            Nothing

        idx :: [] ->
            deleteAt idx fray

        idx :: remainingPath ->
            case List.Extra.getAt idx fray of
                Nothing ->
                    Just (Parallel (Fray fray))

                Just (Right _) ->
                    deleteAt idx fray

                Just (Left strand) ->
                    case deleteInStrand remainingPath strand of
                        Nothing ->
                            deleteAt idx fray

                        Just alignment ->
                            Just <|
                                Parallel <|
                                    Fray <|
                                        case alignment of
                                            Single val ->
                                                fray |> List.Extra.setAt idx (Right val)

                                            Series val ->
                                                fray |> List.Extra.setAt idx (Left val)

                                            Parallel (Fray val) ->
                                                fray
                                                    |> List.Extra.removeAt idx
                                                    |> List.Extra.splitAt idx
                                                    |> (\( l, r ) -> l ++ val ++ r)


map : (Path -> a -> b) -> Strand a -> Strand b
map function strand =
    pathedMapHelper empty function strand


pathedMapHelper : Path -> (Path -> a -> b) -> Strand a -> Strand b
pathedMapHelper p f (Strand strand) =
    Strand (List.indexedMap (\idx -> Either.map (pathedMapFrayHelper (idx :: p) f) (f (idx :: p))) strand)


pathedMapFrayHelper : Path -> (Path -> a -> b) -> Fray a -> Fray b
pathedMapFrayHelper p f (Fray fray) =
    Fray (List.indexedMap (\idx -> Either.map (pathedMapHelper (idx :: p) f) (f (idx :: p))) fray)


fold : { single : Path -> a -> b, strand : Path -> List b -> b, fray : Path -> List b -> b } -> Alignment a -> b
fold functions alignment =
    case alignment of
        Single val ->
            functions.single [] val

        Series strand ->
            pathedFoldStrandHelper empty functions strand

        Parallel fray ->
            pathedFoldFrayHelper empty functions fray


pathedFoldStrandHelper : Path -> { single : Path -> a -> b, strand : Path -> List b -> b, fray : Path -> List b -> b } -> Strand a -> b
pathedFoldStrandHelper p functions (Strand strand) =
    strand
        |> List.indexedMap
            (\idx ->
                Either.fold
                    (pathedFoldFrayHelper (idx :: p) functions)
                    (functions.single (idx :: p))
            )
        |> functions.strand p


pathedFoldFrayHelper : Path -> { single : Path -> a -> b, strand : Path -> List b -> b, fray : Path -> List b -> b } -> Fray a -> b
pathedFoldFrayHelper p functions (Fray fray) =
    fray
        |> List.indexedMap
            (\idx ->
                Either.fold
                    (pathedFoldStrandHelper (idx :: p) functions)
                    (functions.single (idx :: p))
            )
        |> functions.fray p
