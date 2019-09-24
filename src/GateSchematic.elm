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
    Model (Left "") (Single ( "A", ( 1, 1 ) )) False Nothing False


update : Msg -> Model -> Model
update msg ({ gate, label } as model) =
    case msg of
        Select path ->
            if model.clickTrash then
                Strand.Pathed.delete path gate
                    |> Maybe.map
                        (\newgate ->
                            { model | gate = newgate }
                        )
                    |> Maybe.withDefault model

            else
                { model | label = Right path }

        AddParallel path ->
            case label of
                Left "" ->
                    model

                Right _ ->
                    model

                Left text ->
                    { model | gate = Strand.Pathed.insertParallel path ( text, ( 1, 1 ) ) gate, label = Left "" }

        AddSeries path ->
            case label of
                Left "" ->
                    model

                Right _ ->
                    model

                Left text ->
                    { model | gate = Strand.Pathed.insertSeries path ( text, ( 1, 1 ) ) gate, label = Left "" }

        ChangeLabel newLabel ->
            Either.fold
                (always { model | label = Left <| String.filter Char.isAlphaNum newLabel })
                (\labelPath -> { model | gate = Strand.Pathed.updateAt labelPath (Tuple.mapFirst <| always <| String.filter Char.isAlphaNum newLabel) model.gate })
                model.label

        ToggleClickTrash ->
            { model | clickTrash = not model.clickTrash, label = Either.fold Left (always <| Left "") model.label }

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
view ({ gate, label, showNumLogicInputs, showDelays } as model) =
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
