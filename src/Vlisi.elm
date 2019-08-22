module Vlisi exposing (main)

import Browser
import Dict
import Element
import Grid
import Tools exposing (..)
import Types exposing (..)
import Workspace exposing (..)


main =
    Browser.document
        { init = \() -> ( init, Cmd.none )
        , view = \model -> { title = "VLISI", body = [ Element.layout [] <| view model ] }
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PickTool tool ->
            case tool of
                DrawTool layerID ->
                    { model | tool = Drawing layerID Nothing }

                LabelTool ->
                    { model | tool = TypingLabel "" Nothing }

        ToggleView v ->
            { model
                | views =
                    if List.member v model.views then
                        List.filter (\e -> v /= e) model.views

                    else
                        v :: model.views
            }

        RemoveLabel label ->
            { model | labels = Dict.remove label model.labels }

        _ ->
            case model.tool of
                Drawing layer mdrag ->
                    updateDrawing msg layer mdrag model

                TypingLabel label mdrag ->
                    case msg of
                        ChangeLabel str ->
                            { model | tool = TypingLabel (String.filter Char.isAlphaNum str) mdrag }

                        DragDown x y ->
                            { model | tool = TypingLabel label <| Just ( x, y ) }

                        DragMove x y ->
                            { model | tool = TypingLabel label <| Just ( x, y ) }

                        DragUp x y ->
                            { model
                                | tool = Drawing Nwell Nothing
                                , labels =
                                    case String.filter Char.isAlphaNum label of
                                        "" ->
                                            model.labels

                                        cleanLabel ->
                                            Dict.insert cleanLabel ( x, y ) model.labels
                            }

                        _ ->
                            model
