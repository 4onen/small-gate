module Grid exposing (..)

import Array exposing (Array)


type alias Grid a =
    { width : Int, height : Int, data : Array a }


repeat : Int -> Int -> a -> Grid a
repeat width height e =
    { width = width
    , height = height
    , data = Array.repeat (width * height) e
    }


initialize : Int -> Int -> (Int -> Int -> a) -> Grid a
initialize width height f =
    let
        fn i =
            f (modBy width i) (i // width)
    in
    { width = width
    , height = height
    , data = Array.initialize (width * height) fn
    }


toArray : Grid a -> Array a
toArray { data } =
    data


to2DArray : Grid a -> Array (Array a)
to2DArray { width, height, data } =
    Array.initialize height (\y -> Array.slice (y * width) ((y + 1) * width) data)


toList : Grid a -> List a
toList =
    toArray >> Array.toList


to2DList : Grid a -> List (List a)
to2DList =
    to2DArray >> Array.map Array.toList >> Array.toList


get : Int -> Int -> Grid a -> Maybe a
get x y { width, data } =
    if x >= 0 && x < width then
        Array.get (y * width + x) data

    else
        Nothing


set : Int -> Int -> a -> Grid a -> Grid a
set x y e grid =
    { grid | data = Array.set (x * y) e grid.data }


map : (a -> b) -> Grid a -> Grid b
map f { width, height, data } =
    { width = width
    , height = height
    , data = Array.map f data
    }


indexedMap : (Int -> Int -> a -> b) -> Grid a -> Grid b
indexedMap f { width, height, data } =
    let
        fn i =
            f (modBy width i) (i // width)
    in
    { width = width
    , height = height
    , data = Array.indexedMap fn data
    }


foldl : (a -> b -> b) -> b -> Grid a -> b
foldl f b { data } =
    Array.foldl f b data


foldr : (a -> b -> b) -> b -> Grid a -> b
foldr f b { data } =
    Array.foldr f b data


type alias FourNeighbours a =
    { north : Maybe a, west : Maybe a, east : Maybe a, south : Maybe a }


inGrid : Int -> Int -> Grid a -> Bool
inGrid x y { width, height } =
    x > -1 && x < width && y > -1 && y < height



{-
   fourNeighbourIndices : Int -> Int -> Grid a -> FourNeighbours ( Int, Int )
   fourNeighbourIndices x y g =
       { north =
           if inGrid x (y - 1) g then
               Just ( x, y - 1 )

           else
               Nothing
       , west =
           if inGrid (x - 1) y g then
               Just ( x - 1, y )

           else
               Nothing
       , east =
           if inGrid (x + 1) y g then
               Just ( x + 1, y )

           else
               Nothing
       , south =
           if inGrid x (y + 1) g then
               Just ( x, y + 1 )

           else
               Nothing
       }
-}


fourNeighbours : Int -> Int -> Grid a -> FourNeighbours a
fourNeighbours x y g =
    { north = get x (y - 1) g
    , west = get (x - 1) y g
    , east = get (x + 1) y g
    , south = get x (y + 1) g
    }
