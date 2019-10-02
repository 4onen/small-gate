module GateSchematic.Render exposing (view)

import Element exposing (..)
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import GateSchematic.RandomColor
import GateSchematic.Types exposing (CMOS, Model, Msg(..), Transistor, TransistorKind(..))
import Utils.Either as Either
import Utils.Strand as Strand exposing (Alignment)
import Utils.Strand.Pathed as PathedStrand


view : Model -> Element Msg
view ({ gate, label } as model) =
    let
        isNewLabel =
            Either.fold ((<) 0 << String.length) (always False) label

        labelText =
            Either.fold identity (\labelPath -> PathedStrand.getAt labelPath gate |> Maybe.map Tuple.first |> Maybe.withDefault "") label
    in
    column [ centerX, alignTop ]
        [ Element.row []
            [ Element.Input.text []
                { label = Element.Input.labelLeft [] Element.none
                , onChange = ChangeLabel
                , placeholder = Nothing
                , text = labelText
                }
            , Element.Input.button []
                { label =
                    Element.text <|
                        if not model.clickTrash then
                            "✎"

                        else
                            "🗑️"
                , onPress = Just ToggleClickTrash
                }
            ]
        , viewVdd
        , viewGateSide PMOS isNewLabel (Strand.reverse gate)
        , viewOutput "Y"
        , viewGateSide NMOS isNewLabel gate
        , viewGND
        ]


viewGateSide : TransistorKind -> Bool -> CMOS -> Element Msg
viewGateSide tkind canAdd =
    let
        ( ttext, deviceWidth ) =
            case tkind of
                PMOS ->
                    ( "-○", Tuple.second >> Tuple.first )

                NMOS ->
                    ( "-", Tuple.second >> Tuple.second )

        filler =
            Element.el
                [ width fill
                , height <| minimum 10 <| fill
                , Element.Border.widthEach { right = 2, top = 0, bottom = 0, left = 0 }
                ]
                Element.none
    in
    PathedStrand.fold
        { single =
            \p (( i, _ ) as device) ->
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
                                            , onPress = Just (AddParallel (PathedStrand.right p))
                                            }
                                    , onLeft <|
                                        Element.Input.button [ Element.centerY ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddParallel (PathedStrand.left p))
                                            }
                                    , above <|
                                        Element.Input.button [ centerX ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddSeries (PathedStrand.above p))
                                            }
                                    , below <|
                                        Element.Input.button [ centerX ]
                                            { label = Element.text "+"
                                            , onPress = Just (AddSeries (PathedStrand.below p))
                                            }
                                    ]

                                else
                                    [ onRight
                                        (el [ Element.Font.size 12, centerY ]
                                            (deviceWidth device |> String.fromFloat |> Element.text)
                                        )
                                    ]
                               )
                        )
                        [ Element.el
                            [ onLeft <| Element.text <| String.append i ttext
                            , Element.Events.onClick (Select p)
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
