module SvgClick exposing (onDown, onMove, onUp)

import Html
import Html.Events
import Json.Decode as JD


onPointDecode : String -> (Int -> Int -> msg) -> Html.Attribute msg
onPointDecode evt tagger =
    let
        decoder =
            JD.map2 tagger
                (JD.at [ "detail", "x" ] JD.float |> JD.map floor)
                (JD.at [ "detail", "y" ] JD.float |> JD.map floor)
    in
    Html.Events.on evt decoder


onDown : (Int -> Int -> msg) -> Html.Attribute msg
onDown =
    onPointDecode "svgdown"


onUp : (Int -> Int -> msg) -> Html.Attribute msg
onUp =
    onPointDecode "svgup"


onMove : (Int -> Int -> msg) -> Html.Attribute msg
onMove =
    onPointDecode "svgmove"
