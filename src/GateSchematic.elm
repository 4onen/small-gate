module GateSchematic exposing (init, update, view)

import Browser
import Element exposing (..)
import Element.Border
import GateSchematic.Types exposing (..)
import Strand exposing (Strand(..))


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = Element.layout [] << view
        }


init : Model
init =
    let
        logic =
            Series
                [ Single "G"
                , Parallel [ Single "A", Single "B" ]
                , Parallel [ Single "C", Single "D", Series [ Single "E", Single "F" ] ]
                , Parallel [ Series [ Single "X", Single "Y", Single "Wat", Single "V" ], Single "Z" ]
                ]
    in
    Model <|
        Gate
            (Strand.reverse logic)
            logic


update : msg -> Model -> Model
update =
    always <| always init


view : { a | gate : Gate Input } -> Element msg
view { gate } =
    Element.column [ Element.centerX, Element.centerY ]
        [ viewVdd
        , viewStrand PMOS gate.pmos
        , viewOutput "Y"
        , viewStrand NMOS gate.nmos
        , viewGND
        ]


viewStrand : TransistorKind -> Strand Input -> Element msg
viewStrand tkind =
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
    Strand.fold
        { single =
            \i ->
                column [ centerX, height fill ]
                    [ filler
                    , Element.row [ Element.alignRight, width <| px 55 ]
                        [ Element.el
                            [ onLeft <| Element.text <| String.append i ttext
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
        , series =
            Element.column
                [ Element.centerX
                , Element.height Element.fill
                ]
        , parallel =
            Element.row
                [ Element.Border.widthXY 0 1
                , Element.centerX
                , Element.spacing 20
                ]
        }
        >> Element.el [ centerX, Element.Border.widthXY 0 2 ]


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


centerText =
    Element.el [ Element.centerX ] << Element.text
