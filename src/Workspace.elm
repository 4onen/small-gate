module Workspace exposing (view)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Grid exposing (Grid)
import Render
import Types exposing (..)
import Views


view : Model -> Element Msg
view model =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.row [ Element.height <| Element.px 50 ] <| viewToolbar model
        , Element.row [ Element.centerX, Element.centerY ]
            [ Render.view model
            , Element.column
                [ Element.width <| Element.px 200
                , Element.height Element.fill
                ]
                [ Views.view model.views defaultViews
                , viewLabels (model.labels |> Dict.keys)
                ]
            ]
        ]


viewLabels : List String -> Element Msg
viewLabels labels =
    let
        viewLabel : String -> Element Msg
        viewLabel label =
            Element.Input.button
                [ Element.width Element.fill
                , Element.Background.color (Element.rgb 1.0 1.0 0.7)
                ]
                { onPress = Just (RemoveLabel label)
                , label = Element.text label
                }
    in
    labels
        |> List.map viewLabel
        |> (::) (Element.text "Labels: (click delete)")
        |> Element.column [ Element.width Element.fill, Element.height Element.fill ]


viewToolbar : Model -> List (Element Msg)
viewToolbar model =
    let
        selectedColor =
            Element.rgb 1.0 1.0 0.0

        borderColor =
            Element.rgb 0.0 0.0 1.0

        toolbarButton { color, selected, onPress, label } =
            Element.Input.button
                [ Element.height Element.fill
                , Element.Background.color color
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

        drawingTool layer tool =
            case tool of
                Drawing layerID _ ->
                    layerID == layer

                _ ->
                    False
    in
    [ toolbarButton
        { color = Element.rgb 0.82421875 0.82421875 0.82421875
        , selected = drawingTool Nwell model.tool
        , onPress = Just <| PickTool <| DrawTool <| Nwell
        , label = Element.text "Nwell"
        }
    , toolbarButton
        { color = Element.rgb 0.5 0.5 0.5
        , selected = drawingTool Ndiff model.tool
        , onPress = Just <| PickTool <| DrawTool <| Ndiff
        , label = Element.text "Ndiff"
        }
    , toolbarButton
        { color = Element.rgb 0.67578125 0.84375 0.8984375
        , selected = drawingTool Pdiff model.tool
        , onPress = Just <| PickTool <| DrawTool <| Pdiff
        , label = Element.text "Pdiff"
        }
    , toolbarButton
        { color = Element.rgb 0.390625 0.58203125 0.92578125
        , selected = drawingTool Metal model.tool
        , onPress = Just <| PickTool <| DrawTool <| Metal
        , label = Element.text "Metal"
        }
    , Element.Input.button
        [ Element.height Element.fill
        , Element.Background.color (Element.rgb 0.0 0.0 0.0)
        , Element.Font.color (Element.rgb 1.0 1.0 1.0)
        , Element.Border.solid
        , Element.Border.width 5
        , Element.Border.color
            (if drawingTool Polysilicon model.tool then
                selectedColor

             else
                borderColor
            )
        ]
        { onPress = Just <| PickTool <| DrawTool <| Polysilicon, label = Element.text "Poly" }
    , toolbarButton
        { color = Element.rgb 1.0 1.0 1.0
        , selected = drawingTool Contacts model.tool
        , onPress = Just <| PickTool <| DrawTool <| Contacts
        , label = Element.text "Contacts"
        }
    , case model.tool of
        TypingLabel str _ ->
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
                { onPress = Just (PickTool LabelTool)
                , label = Element.text "Label"
                }
    ]
