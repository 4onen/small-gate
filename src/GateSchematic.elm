module GateSchematic exposing (init, update, view)

import Browser
import Either exposing (Either(..))
import Element exposing (..)
import Element.Border
import Element.Events
import Element.Input
import GateSchematic.Types exposing (..)
import List.Extra
import Strand exposing (Alignment(..), Fray(..), Strand(..))
import Strand.Pathed exposing (Path)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = Element.layout [] << view
        }


type Msg
    = Delete Path
    | AddParallel Path
    | AddSeries Path


init : Model
init =
    let
        logic =
            Series <|
                Strand
                    [ Right "G"
                    , Left <| Fray [ Right "A", Right "B" ]
                    , Left <| Fray [ Right "C", Right "D", Left <| Strand [ Right "E", Right "F" ] ]
                    , Left <|
                        Fray
                            [ Left <| Strand [ Right "X", Right "Y", Right "Wat", Right "V" ]
                            , Right "Z"
                            ]
                    ]
    in
    Strand.reverse logic


update : Msg -> Model -> Model
update msg model =
    case msg of
        Delete path ->
            case Strand.Pathed.delete (Debug.log "path" path) model of
                Just newmodel ->
                    newmodel

                Nothing ->
                    model

        AddParallel path ->
            Strand.Pathed.insertParallel path "K" model

        AddSeries path ->
            Strand.Pathed.insertSeries path "L" model


view : Model -> Element Msg
view model =
    Element.column [ Element.centerX, Element.centerY ]
        [ viewVdd
        , viewStrand PMOS model
        , viewOutput "Y"
        , viewStrand NMOS <| Strand.reverse model
        , viewGND
        ]


viewStrand : TransistorKind -> Alignment Input -> Element Msg
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
    Strand.Pathed.fold
        { single =
            \p i ->
                column
                    [ centerX
                    , height fill
                    , onRight <|
                        Element.Input.button [ Element.centerY ]
                            { label = Element.text "+"
                            , onPress = Just (AddParallel (Strand.Pathed.right p))
                            }
                    , onLeft <|
                        Element.Input.button [ Element.centerY ]
                            { label = Element.text "+"
                            , onPress = Just (AddParallel (Strand.Pathed.left p))
                            }
                    ]
                    [ filler
                    , Element.row
                        [ Element.alignRight
                        , width <| px 55
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
