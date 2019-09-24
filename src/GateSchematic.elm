module GateSchematic exposing (init, update, view)

import Browser
import Either exposing (Either(..))
import Element exposing (..)
import Element.Input
import GateSchematic.Logic
import GateSchematic.Logic.Render
import GateSchematic.RandomColor
import GateSchematic.Render
import GateSchematic.RiseFall
import GateSchematic.Types exposing (..)
import Set exposing (Set)
import Strand exposing (Alignment(..))
import Strand.Pathed exposing (Path)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = Element.layout [] << view
        }


init : Model
init =
    Model "" (Single ( "A", ( 1, 1 ) )) Nothing False


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
                { model | gate = Strand.Pathed.insertParallel path ( labelToAdd, ( 1, 1 ) ) gate, labelToAdd = "" }

            else
                model

        AddSeries path ->
            if String.length labelToAdd > 0 then
                { model | gate = Strand.Pathed.insertSeries path ( labelToAdd, ( 1, 1 ) ) gate, labelToAdd = "" }

            else
                model

        ChangeLabel newLabel ->
            { model | labelToAdd = String.filter Char.isAlphaNum newLabel }

        ToggleLogic ->
            { model
                | showNumLogicInputs =
                    case model.showNumLogicInputs of
                        Nothing ->
                            Just 2

                        Just _ ->
                            Nothing
            }

        ChangeLogicInputs i ->
            { model
                | showNumLogicInputs =
                    if i == 0 || model.showNumLogicInputs == Nothing then
                        Nothing

                    else
                        Just i
            }

        ToggleDelays ->
            { model | showDelays = not model.showDelays }


view : Model -> Element Msg
view ({ gate, labelToAdd, showNumLogicInputs, showDelays } as model) =
    Element.wrappedRow
        [ Element.centerX, Element.centerY, spacing 20 ]
        [ GateSchematic.Render.view model
        , Element.Input.button [ alignTop ]
            { label = Element.text (showNumLogicInputs |> Maybe.map (always "<") |> Maybe.withDefault "Logic >")
            , onPress = Just ToggleLogic
            }
        , case showNumLogicInputs of
            Just n ->
                GateSchematic.Logic.Render.view n (Strand.map Tuple.first gate)

            Nothing ->
                Element.none
        , Element.Input.button [ alignTop ]
            { label =
                Element.text
                    (if showDelays then
                        "<"

                     else
                        "Delays >"
                    )
            , onPress = Just ToggleDelays
            }
        , if showDelays then
            Element.column [ alignTop ] <|
                (::) (Element.text "Rise times:") <|
                    List.map (Element.text << viewDelay) <|
                        Tuple.first <|
                            GateSchematic.RiseFall.computeRiseFall gate 12.0

          else
            Element.none
        ]


viewDelay : ( Set String, Float ) -> String
viewDelay ( vals, delay ) =
    (vals
        |> Set.toList
        |> List.intersperse "', "
    )
        ++ [ "': "
           , delay |> String.fromFloat
           ]
        |> String.concat
