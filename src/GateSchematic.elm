module GateSchematic exposing (init, update, view)

import Browser
import Either exposing (Either(..))
import Element exposing (..)
import Element.Input
import GateSchematic.Logic
import GateSchematic.Logic.Render
import GateSchematic.RandomColor
import GateSchematic.Render
import GateSchematic.Types exposing (..)
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
        (GateSchematic.Render.view model
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
