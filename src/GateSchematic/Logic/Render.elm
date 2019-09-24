module GateSchematic.Logic.Render exposing (view)

import Bitwise
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import GateSchematic.Logic
import GateSchematic.Types exposing (Msg(..))
import Set
import Strand exposing (Alignment)


view : Int -> Alignment String -> Element Msg
view numInputs gate =
    let
        inputs =
            GateSchematic.Logic.retrieveInputs gate

        inputCount =
            min numInputs (Set.size inputs)
    in
    Element.column [ alignTop, spacing 10 ]
        [ Element.Input.slider []
            { label = Element.Input.labelBelow [] <| text <| "Max inputs to render: " ++ String.fromInt numInputs
            , max = 9
            , min = 0
            , onChange = round >> ChangeLogicInputs
            , step = Just 1
            , thumb = Element.Input.defaultThumb
            , value = toFloat numInputs
            }
        , if numInputs <= 0 then
            Element.none

          else
            let
                keptInputs =
                    inputs
                        |> Set.toList
                        |> List.take inputCount

                tf : Bool -> Element msg
                tf bool =
                    if bool then
                        el [ Element.Background.color (rgb 0.9 1.0 0.9) ] (text "T")

                    else
                        el [ Element.Background.color (rgb 1.0 0.9 0.9) ] (text "F")
            in
            table [ spacingXY 15 0 ]
                { columns =
                    { header = Element.text "#"
                    , width = shrink
                    , view = String.fromInt >> Element.text
                    }
                        :: ((keptInputs
                                |> List.indexedMap
                                    (\n i ->
                                        { header = text i
                                        , width = shrink
                                        , view =
                                            Bitwise.shiftRightZfBy (inputCount - n - 1)
                                                >> Bitwise.and 1
                                                >> (==) 1
                                                >> tf
                                        }
                                    )
                            )
                                ++ [ { header =
                                        el
                                            [ Element.Font.center
                                            , Element.Border.widthEach
                                                { top = 1, bottom = 0, left = 0, right = 0 }
                                            ]
                                            (Element.text (GateSchematic.Logic.toText gate))
                                     , width = shrink
                                     , view =
                                        \j ->
                                            keptInputs
                                                |> List.indexedMap (\n -> j |> Bitwise.shiftRightZfBy (inputCount - n - 1) |> Bitwise.and 1 |> (==) 1 |> Tuple.pair)
                                                |> List.filterMap
                                                    (\( b, i ) ->
                                                        if b then
                                                            Just i

                                                        else
                                                            Nothing
                                                    )
                                                |> Set.fromList
                                                |> (\s -> GateSchematic.Logic.simulate s gate)
                                                |> tf
                                     }
                                   ]
                           )
                , data = List.range 0 (2 ^ inputCount - 1)
                }
        ]


viewOptionLabel : Int -> Element Msg
viewOptionLabel i =
    Element.text <|
        case i of
            0 ->
                "Off"

            _ ->
                String.fromInt i
