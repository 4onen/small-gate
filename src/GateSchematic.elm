module GateSchematic exposing (init, update, view)

import Bitwise
import Browser
import Either exposing (Either(..))
import Element exposing (..)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import GateSchematic.Logic
import GateSchematic.Logic.Render
import GateSchematic.RandomColor
import GateSchematic.Types exposing (..)
import List.Extra
import Set
import Strand exposing (Alignment(..), Fray(..), Strand(..))
import Strand.Pathed exposing (Path)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = Element.layout [] << view
        }


init : Model
init =
    Model "" (Single ( "A", 1 )) Nothing


update : Msg -> Model -> Model
update msg ({ gate, labelToAdd } as model) =
    case msg of
        Delete path ->
            case Strand.Pathed.delete path gate of
                Just newmodel ->
                    { model | gate = newmodel }

                Nothing ->
                    model

        AddParallel path ->
            if String.length labelToAdd > 0 then
                { model | gate = Strand.Pathed.insertParallel path ( labelToAdd, 1 ) gate, labelToAdd = "" }

            else
                model

        AddSeries path ->
            if String.length labelToAdd > 0 then
                { model | gate = Strand.Pathed.insertSeries path ( labelToAdd, 1 ) gate, labelToAdd = "" }

            else
                model

        ChangeLabel newLabel ->
            { model | labelToAdd = String.filter Char.isAlphaNum newLabel }

        ToggleLogic ->
            { model
                | numInputsToShow =
                    case model.numInputsToShow of
                        Nothing ->
                            Just 2

                        Just _ ->
                            Nothing
            }

        ChangeLogicInputs i ->
            { model
                | numInputsToShow =
                    if i == 0 || model.numInputsToShow == Nothing then
                        Nothing

                    else
                        Just i
            }


view : Model -> Element Msg
view ({ gate, labelToAdd, numInputsToShow } as model) =
    Element.wrappedRow
        [ Element.centerX, Element.centerY, spacing 20 ]
        (viewGate model
            :: Element.Input.button [ alignTop ]
                { label = Element.text (numInputsToShow |> Maybe.map (always "<") |> Maybe.withDefault "Logic >")
                , onPress = Just ToggleLogic
                }
            :: (case numInputsToShow of
                    Just n ->
                        [ GateSchematic.Logic.Render.view n (Strand.map Tuple.first gate) ]

                    Nothing ->
                        []
               )
        )


viewGate : Model -> Element Msg
viewGate ({ gate, labelToAdd } as model) =
    Element.column [ centerX, alignTop ]
        [ Element.Input.text []
            { label = Element.Input.labelLeft [] Element.none
            , onChange = ChangeLabel
            , placeholder = Just <| Element.Input.placeholder [] (Element.text "A")
            , text = labelToAdd
            }
        , viewVdd
        , viewStrand PMOS (String.length labelToAdd > 0) (Strand.reverse gate)
        , viewOutput "Y"
        , viewStrand NMOS (String.length labelToAdd > 0) gate
        , viewGND
        ]


viewStrand : TransistorKind -> Bool -> Alignment Input -> Element Msg
viewStrand tkind canAdd =
    let
        ttext =
            case tkind of
                PMOS ->
                    "-○"

                NMOS ->
                    "-"

        filler =
            Element.el
                [ width fill
                , height <| minimum 10 <| fill
                , Element.Border.widthEach { right = 2, top = 0, bottom = 0, left = 0 }
                ]
                Element.none
    in
    Strand.Pathed.fold
        { single =
            \p ( i, _ ) ->
                column
                    [ centerX
                    , height fill
                    , Element.Font.color <| GateSchematic.RandomColor.fromString i
                    ]
                    [ filler
                    , Element.row
                        ([ Element.alignRight
                         , width <| px 55
                         ]
                            ++ (if canAdd then
                                    [ onRight <|
                                        Element.Input.button [ Element.centerY ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddParallel (Strand.Pathed.right p))
                                            }
                                    , onLeft <|
                                        Element.Input.button [ Element.centerY ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddParallel (Strand.Pathed.left p))
                                            }
                                    , above <|
                                        Element.Input.button [ centerX ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddSeries (Strand.Pathed.above p))
                                            }
                                    , below <|
                                        Element.Input.button [ centerX ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddSeries (Strand.Pathed.below p))
                                            }
                                    ]

                                else
                                    []
                               )
                        )
                        [ Element.el
                            [ onLeft <| Element.text <| String.append i ttext
                            , Element.Events.onClick (Delete p)
                            , moveLeft 2
                            , alignRight
                            , height fill
                            , Element.Border.width 1
                            ]
                            none
                        , Element.el
                            [ Element.Border.widthEach { bottom = 2, top = 2, left = 2, right = 0 }
                            , alignRight
                            ]
                            (Element.text " ")
                        ]
                    , filler
                    ]
        , strand =
            Element.column
                [ Element.centerX
                , Element.height Element.fill
                ]
                |> always
        , fray =
            Element.row
                [ Element.Border.widthXY 0 1
                , Element.centerX
                , height fill
                , Element.spacing 20
                ]
                |> always
        }
        >> Element.el [ centerX, Element.Border.widthXY 0 1 ]
        >> (case tkind of
                NMOS ->
                    identity

                PMOS ->
                    Element.map
                        (\msg ->
                            case msg of
                                AddParallel p ->
                                    AddSeries p

                                AddSeries p ->
                                    AddParallel p

                                other ->
                                    other
                        )
           )


viewVdd : Element msg
viewVdd =
    Element.el
        [ Element.centerX ]
        (Element.text "Vdd")


viewGND : Element msg
viewGND =
    Element.el
        [ Element.centerX ]
        (Element.text "GND")


viewOutput : String -> Element msg
viewOutput label =
    Element.el
        [ Element.centerX
        , height <| px 80
        , Element.onRight
            (Element.row []
                [ Element.el
                    [ Element.width <| Element.px 100
                    , Element.Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
                    ]
                    Element.none
                , Element.text label
                ]
            )
        , Element.Border.width 1
        ]
        Element.none
