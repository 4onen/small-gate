module Decode exposing (onSvgDown, onSvgMove, onSvgUp)

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


onSvgDown : (Int -> Int -> msg) -> Html.Attribute msg
onSvgDown =
    onPointDecode "svgdown"


onSvgUp : (Int -> Int -> msg) -> Html.Attribute msg
onSvgUp =
    onPointDecode "svgup"


onSvgMove : (Int -> Int -> msg) -> Html.Attribute msg
onSvgMove =
    onPointDecode "svgmove"
