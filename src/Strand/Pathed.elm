module Strand.Pathed exposing (Path, above, below, delete, empty, fold, insertParallel, insertSeries, left, map, right)

import Either exposing (Either(..))
import List.Extra
import Strand exposing (Alignment(..), Fray(..), Strand(..))


type alias Path =
    List Int


empty : Path
empty =
    []


above : Path -> Path
above =
    List.reverse >> (::) 0 >> List.reverse


below : Path -> Path
below =
    List.reverse >> (::) 1 >> List.reverse


left : Path -> Path
left =
    above


right : Path -> Path
right =
    below


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


insertSeries : Path -> a -> Alignment a -> Alignment a
insertSeries =
    let
        insertHere : List (Either (Fray a) a) -> Alignment a
        insertHere =
            Series << Strand
    in
    insert insertHere


insertParallel : Path -> a -> Alignment a -> Alignment a
insertParallel =
    let
        insertHere : List (Either (Strand a) a) -> Alignment a
        insertHere =
            Parallel << Fray
    in
    insert insertHere


insert : (List (Either subtype a) -> Alignment a) -> Path -> a -> Alignment a -> Alignment a
insert inserter path val alignment =
    let
        insertHere : Int -> Alignment a
        insertHere idx =
            case alignment of
                Single a ->
                    if idx > 0 then
                        inserter [ Right a, Right val ]

                    else
                        inserter [ Right val, Right a ]

                Series (Strand ls) ->
                    ls
                        |> List.Extra.splitAt idx
                        |> (\( a, b ) -> a ++ (Right val :: b))
                        |> Strand
                        |> Series

                Parallel (Fray ls) ->
                    ls
                        |> List.Extra.splitAt idx
                        |> (\( a, b ) -> a ++ (Right val :: b))
                        |> Fray
                        |> Parallel

        insertBelow idx remainingPath parallelConstructor strand =
            (case List.Extra.getAt idx strand of
                Nothing ->
                    Nothing

                Just (Right a) ->
                    Just (Single a)

                Just (Left fray) ->
                    Just (parallelConstructor fray)
            )
                |> Maybe.map (insert inserter remainingPath val)
    in
    case path of
        [] ->
            alignment

        idx :: [] ->
            insertHere idx

        idx :: remainingPath ->
            case alignment of
                Single a ->
                    insertHere idx

                Series (Strand strand) ->
                    case insertBelow idx remainingPath Parallel strand of
                        Nothing ->
                            alignment

                        Just (Single _) ->
                            alignment

                        Just (Series (Strand newStranding)) ->
                            strand
                                |> List.Extra.removeAt idx
                                |> List.Extra.splitAt idx
                                |> (\( l, r ) -> l ++ newStranding ++ r)
                                |> Strand
                                |> Series

                        Just (Parallel (Fray newFraying)) ->
                            strand
                                |> List.Extra.setAt idx (Left (Fray newFraying))
                                |> Strand
                                |> Series

                Parallel (Fray fray) ->
                    case insertBelow idx remainingPath Series fray of
                        Nothing ->
                            alignment

                        Just (Single _) ->
                            alignment

                        Just (Series (Strand newStranding)) ->
                            fray
                                |> List.Extra.setAt idx (Left (Strand newStranding))
                                |> Fray
                                |> Parallel

                        Just (Parallel (Fray newFraying)) ->
                            fray
                                |> List.Extra.removeAt idx
                                |> List.Extra.splitAt idx
                                |> (\( l, r ) -> l ++ newFraying ++ r)
                                |> Fray
                                |> Parallel


map : (Path -> a -> b) -> Strand a -> Strand b
map function strand =
    pathedMapHelper empty function strand


pathedMapHelper : Path -> (Path -> a -> b) -> Strand a -> Strand b
pathedMapHelper p f (Strand strand) =
    Strand (List.indexedMap (\idx -> Either.map (pathedMapFrayHelper (idx :: p) f) (f (List.reverse <| idx :: p))) strand)


pathedMapFrayHelper : Path -> (Path -> a -> b) -> Fray a -> Fray b
pathedMapFrayHelper p f (Fray fray) =
    Fray (List.indexedMap (\idx -> Either.map (pathedMapHelper (idx :: p) f) (f (List.reverse <| idx :: p))) fray)


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
                    (functions.single (List.reverse <| idx :: p))
            )
        |> functions.strand p


pathedFoldFrayHelper : Path -> { single : Path -> a -> b, strand : Path -> List b -> b, fray : Path -> List b -> b } -> Fray a -> b
pathedFoldFrayHelper p functions (Fray fray) =
    fray
        |> List.indexedMap
            (\idx ->
                Either.fold
                    (pathedFoldStrandHelper (idx :: p) functions)
                    (functions.single (List.reverse <| idx :: p))
            )
        |> functions.fray p
