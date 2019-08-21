module Vlisi exposing (main)

import Browser
import Dict
import Element
import Grid
import Render exposing (..)
import Tools exposing (..)
import Types exposing (..)


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

        RemoveLabel label ->
            { model | labels = Dict.remove label model.labels }

        _ ->
            case model.tool of
                Drawing layer mdrag ->
                    updateDrawing msg layer mdrag model

                TypingLabel label mdrag ->
                    case msg of
                        ChangeLabel str ->
                            { model | tool = TypingLabel str mdrag }

                        DragDown x y ->
                            { model | tool = TypingLabel label <| Just ( x, y ) }

                        DragMove x y ->
                            { model | tool = TypingLabel label <| Just ( x, y ) }

                        DragUp x y ->
                            { model
                                | tool = Drawing Nwell Nothing
                                , labels = Dict.insert label ( x, y ) model.labels
                            }

                        _ ->
                            model
