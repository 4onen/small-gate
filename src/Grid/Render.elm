module Grid.Render exposing (..)

import Grid exposing (..)
import Svg exposing (Svg)


type alias Renderer a msg = Grid a -> Int -> Int -> a -> List (Svg msg)


render : Grid a -> Renderer a msg -> List (Svg msg)
render grid fn =
    grid
        |> Grid.indexedMap (fn grid)
        |> Grid.toList
        |> List.concatMap identity
