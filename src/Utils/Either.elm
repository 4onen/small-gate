module Utils.Either exposing (Either(..), fold, map, mapLeft, mapRight)


type Either a b
    = Left a
    | Right b


map : (a -> c) -> (b -> d) -> Either a b -> Either c d
map left right either =
    case either of
        Left val ->
            Left (left val)

        Right val ->
            Right (right val)


mapLeft : (a -> c) -> Either a b -> Either c b
mapLeft f =
    map f identity


mapRight : (b -> c) -> Either a b -> Either a c
mapRight f =
    map identity f


fold : (a -> c) -> (b -> c) -> Either a b -> c
fold left right either =
    case either of
        Left val ->
            left val

        Right val ->
            right val
