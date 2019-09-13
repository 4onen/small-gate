module Strand.Path exposing (..)

import List.Extra
import List.MyExtra
import Strand exposing (Strand(..))


type alias Path =
    List Int


empty : Path
empty =
    []


delete : Path -> Strand a -> Maybe (Strand a)
delete path strand =
    let
        subStrandOp form result =
            case result of
                [] ->
                    Nothing

                a :: [] ->
                    Just a

                ls ->
                    Just (form ls)
    in
    case path of
        [] ->
            Nothing

        idx :: [] ->
            case strand of
                Single _ ->
                    Nothing

                Series ls ->
                    subStrandOp Series (List.Extra.removeAt idx ls)

                Parallel ls ->
                    subStrandOp Parallel (List.Extra.removeAt idx ls)

        idx :: remainingPath ->
            case strand of
                Single _ ->
                    Just strand

                Series ls ->
                    subStrandOp Series <| List.MyExtra.updateFilterAt idx (delete remainingPath) ls

                Parallel ls ->
                    subStrandOp Parallel <| List.MyExtra.updateFilterAt idx (delete remainingPath) ls


pathedMap : (Path -> a -> b) -> Strand a -> Strand b
pathedMap function strand =
    pathedMapHelper empty function strand


pathedMapHelper : Path -> (Path -> a -> b) -> Strand a -> Strand b
pathedMapHelper p f sa =
    case sa of
        Single a ->
            Single <| f p a

        Series ls ->
            Series <| List.indexedMap (\i -> pathedMapHelper (i :: p) f) ls

        Parallel ls ->
            Parallel <| List.indexedMap (\i -> pathedMapHelper (i :: p) f) ls


pathedFold : { single : Path -> a -> b, series : Path -> List b -> b, parallel : Path -> List b -> b } -> Strand a -> b
pathedFold functions strand =
    pathedFoldHelper empty functions strand


pathedFoldHelper : Path -> { single : Path -> a -> b, series : Path -> List b -> b, parallel : Path -> List b -> b } -> Strand a -> b
pathedFoldHelper p functions strand =
    case strand of
        Single a ->
            functions.single p a

        Series ls ->
            ls
                |> List.indexedMap (\i sa -> pathedFoldHelper (i :: p) functions sa)
                |> functions.series p

        Parallel ls ->
            ls
                |> List.indexedMap (\i sa -> pathedFoldHelper (i :: p) functions sa)
                |> functions.parallel p
