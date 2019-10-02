module GateSchematic exposing (init, update, view)

import Browser
import Element exposing (..)
import Element.Input
import GateSchematic.Delay
import GateSchematic.Delay.Render
import GateSchematic.Delay.Widths
import GateSchematic.Logic
import GateSchematic.Logic.Render
import GateSchematic.RandomColor
import GateSchematic.Render
import GateSchematic.Types exposing (..)
import Set exposing (Set)
import Utils.Either as Either exposing (Either(..))
import Utils.Strand as Strand exposing (Alignment(..))
import Utils.Strand.Pathed as PathedStrand exposing (Path)


init : Model
init =
    Model (Left "") (Single ( "A", ( 2, 1 ) )) False Nothing False


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select path ->
            if model.clickTrash then
                PathedStrand.delete path model.gate
                    |> Maybe.map
                        (\newgate ->
                            { model | gate = newgate }
                        )
                    |> Maybe.withDefault model

            else
                { model | label = Right path }

        AddParallel path ->
            case model.label of
                Left "" ->
                    model

                Right _ ->
                    model

                Left text ->
                    { model | gate = PathedStrand.insertParallel path ( text, ( 2, 1 ) ) model.gate, label = Left "" }

        AddSeries path ->
            case model.label of
                Left "" ->
                    model

                Right _ ->
                    model

                Left text ->
                    { model | gate = PathedStrand.insertSeries path ( text, ( 2, 1 ) ) model.gate, label = Left "" }

        ChangeLabel newLabel ->
            Either.fold
                (always { model | label = Left <| GateSchematic.Logic.cleanLabel newLabel })
                (\labelPath -> { model | gate = PathedStrand.updateAt labelPath (Tuple.mapFirst <| always <| GateSchematic.Logic.cleanLabel newLabel) model.gate })
                model.label

        ToggleClickTrash ->
            { model | clickTrash = not model.clickTrash, label = Either.fold Left (always <| Left "") model.label }

        ToggleLogic ->
            { model
                | showNumLogicInputs =
                    case model.showNumLogicInputs of
                        Nothing ->
                            Just 6

                        Just _ ->
                            Nothing
            }

        ChangeLogicInputs i ->
            { model | showNumLogicInputs = Just i }

        ToggleDelays ->
            { model | showDelays = not model.showDelays }

        SolveDelays ->
            { model | gate = GateSchematic.Delay.Widths.solve (Strand.map Tuple.first model.gate) }


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
                (::) (Element.Input.button [] { onPress = Just SolveDelays, label = Element.text "MakeEqual" }) <|
                    GateSchematic.Delay.Render.view gate

          else
            Element.none
        ]
