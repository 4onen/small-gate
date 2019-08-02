module Grid.Render exposing (..)

import Grid exposing (..)
import Svg exposing (Svg)


type alias Renderer msg = Grid -> (Int,Int) -> List (Svg msg)


render : Grid -> Renderer msg -> List (Svg msg)
render grid fn =
    grid 
        |> Grid.toList
        |> List.concatMap (fn grid)
