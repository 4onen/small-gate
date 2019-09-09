module Layout.Workspace exposing (view)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Layout.Grid exposing (Grid)
import Layout.Render
import Layout.Types exposing (..)
import Layout.Views as Views


view : Model -> Element Msg
view model =
    let
        arrangedViews =
            Views.arrange model.views Views.defaultViews

        sortedViews =
            Views.sort arrangedViews
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ toolbar [ Element.height <| Element.px 50 ] model
        , Element.row [ Element.centerX, Element.centerY ]
            [ Layout.Render.view sortedViews model
            , Element.column
                [ Element.width <| Element.px 200
                , Element.height Element.fill
                ]
                [ Views.view arrangedViews
                , viewLabels [] (model.labels |> Dict.keys)
                ]
            ]
        ]


viewLabels : List (Element.Attribute Msg) -> List String -> Element Msg
viewLabels attrs labels =
    let
        viewLabel : String -> Element Msg
        viewLabel label =
            Element.row
                [ Element.width Element.fill
                , Element.Background.color (Element.rgb 1.0 1.0 0.7)
                ]
                [ Element.Input.button
                    [ Element.width Element.fill ]
                    { onPress = Just (PickTool <| TypingLabel label)
                    , label = Element.text label
                    }
                , Element.Input.button
                    [ Element.width Element.shrink ]
                    { onPress = Just (RemoveLabel label)
                    , label = Element.text "T"
                    }
                ]
    in
    labels
        |> List.map viewLabel
        |> (::) (Element.text "Labels: (T trashes)")
        |> Element.column attrs


toolbar : List (Element.Attribute Msg) -> Model -> Element Msg
toolbar attrs model =
    let
        selectedColor =
            Element.rgb 1.0 1.0 0.0

        borderColor =
            Element.rgb 0.0 0.0 1.0

        toolbarButton { color, selected, onPress, label } =
            Element.Input.button
                [ Element.height Element.fill
                , Element.Border.solid
                , Element.Border.width 5
                , Element.Border.color
                    (if selected then
                        selectedColor

                     else
                        borderColor
                    )
                ]
                { onPress = onPress, label = label }

        drawingTool tool =
            case tool of
                Drawing _ ->
                    True

                _ ->
                    False
    in
    Element.row attrs
        [ toolbarButton
            { color = Element.rgb 0.82421875 0.82421875 0.82421875
            , selected = drawingTool model.tool
            , onPress = Just <| PickTool <| Drawing Nothing
            , label = Element.text "Draw"
            }
        , case model.tool of
            TypingLabel str ->
                Element.Input.text
                    [ Element.height Element.fill
                    , Element.width <| Element.px 100
                    , Element.focused []
                    , Element.Border.solid
                    , Element.Border.width 5
                    , Element.Border.color selectedColor
                    ]
                    { onChange = ChangeLabel
                    , text = str
                    , placeholder = Nothing
                    , label =
                        Element.Input.labelHidden "Enter Label"
                    }

            _ ->
                Element.Input.button
                    [ Element.height Element.fill
                    , Element.Border.solid
                    , Element.Border.width 5
                    , Element.Border.color borderColor
                    ]
                    { onPress = Just (PickTool <| TypingLabel "")
                    , label = Element.text "Label"
                    }
        ]
