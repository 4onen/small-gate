module Grid exposing (Grid, activeArea, empty, fourNeighbours, get, indexedMap, initialize, map, set, toList)

import Set exposing (Set)


type alias Grid =
    Set ( Int, Int )


empty : Grid
empty =
    Set.empty


initialize : Int -> Int -> (( Int, Int ) -> Bool) -> Grid
initialize width height f =
    List.range 0 (width - 1)
        |> List.concatMap (\x -> List.map (Tuple.pair x) (List.range 0 (height - 1)))
        |> List.filter f
        |> Set.fromList


toList : Grid -> List ( Int, Int )
toList =
    Set.toList


activeArea : Grid -> Maybe ( ( Int, Int ), ( Int, Int ) )
activeArea =
    let
        f ( x, y ) mpts =
            case mpts of
                Nothing ->
                    Just ( ( x, y ), ( x, y ) )

                Just ( ( x1, y1 ), ( x2, y2 ) ) ->
                    Just ( ( min x x1, min y y1 ), ( max x x2, max y y2 ) )
    in
    Set.foldl f Nothing


get : Int -> Int -> Grid -> Bool
get x y =
    Set.member ( x, y )


set : Int -> Int -> Bool -> Grid -> Grid
set x y e =
    if e then
        Set.insert ( x, y )

    else
        Set.remove ( x, y )


map : (Bool -> Bool) -> Grid -> Grid
map f =
    indexedMap (\_ -> f)


indexedMap : (( Int, Int ) -> Bool -> Bool) -> Grid -> Grid
indexedMap f grid =
    case activeArea grid of
        Nothing ->
            grid

        Just ( ( x1, y1 ), ( x2, y2 ) ) ->
            let
                rangey =
                    List.range y1 y2

                range =
                    List.concatMap
                        (\x -> List.map (Tuple.pair x) rangey)
                        (List.range x1 x2)

                step ( x, y ) out =
                    if f ( x, y ) (Set.member ( x, y ) grid) then
                        Set.insert ( x, y ) out

                    else
                        out
            in
            List.foldl step Set.empty range


filter : (( Int, Int ) -> Bool) -> Grid -> Grid
filter =
    Set.filter


union : Grid -> Grid -> Grid
union =
    Set.union


intersect : Grid -> Grid -> Grid
intersect =
    Set.intersect


type alias FourNeighbours =
    { north : Bool, south : Bool, east : Bool, west : Bool }


fourNeighbours : Int -> Int -> Grid -> FourNeighbours
fourNeighbours x y grid =
    FourNeighbours
        (Set.member ( x, y + 1 ) grid)
        (Set.member ( x, y - 1 ) grid)
        (Set.member ( x + 1, y ) grid)
        (Set.member ( x - 1, y ) grid)
