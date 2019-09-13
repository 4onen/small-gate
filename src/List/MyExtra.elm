module List.MyExtra exposing (updateFilterAt)

import List.Extra


updateFilterAt : Int -> (a -> Maybe a) -> List a -> List a
updateFilterAt idx f ls =
    case List.Extra.getAt idx ls |> Maybe.andThen f of
        Just val ->
            List.Extra.setAt idx val ls

        Nothing ->
            List.Extra.removeAt idx ls
