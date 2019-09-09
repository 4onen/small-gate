module Strand.Path exposing (..)

import Strand exposing (Strand(..))


type alias Path =
    List Int


empty : Path
empty =
    []


pathedMap : (Path -> a -> b) -> Strand a -> Strand b
pathedMap =
    pathedMapHelper empty


pathedMapHelper : Path -> (Path -> a -> b) -> Strand a -> Strand b
pathedMapHelper p f sa =
    case sa of
        Single a ->
            Single <| f p a

        Series ls ->
            Series <| List.indexedMap (\i -> pathedMapHelper (i :: p) f) ls

        Parallel ls ->
            Parallel <| List.indexedMap (\i -> pathedMapHelper (i :: p) f) ls
