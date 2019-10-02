module Main exposing (main)

import Array exposing (Array)
import Browser
import Element
import Element.Border
import Element.Input
import GateSchematic
import GateSchematic.Types
import Layout
import Layout.Types


main =
    Browser.document
        { init = \() -> ( init (), Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias Gate =
    { name : String
    , layout : Layout.Types.Model
    , schematic : GateSchematic.Types.Model
    }


type alias GateID =
    Int


type Status
    = Schematic GateID
    | Layout GateID


type alias Model =
    { workspaces : Array Gate
    , status : Maybe Status
    }


type Msg
    = LayoutMsg GateID Layout.Types.Msg
    | SchematicMsg GateID GateSchematic.Types.Msg
    | AddGate
    | SelectGate (Maybe Status)
    | ChangeGateName Int String
    | DeleteGate Int


init () =
    Model Array.empty Nothing


initGate =
    Gate "Inverter" Layout.init GateSchematic.init


update msg ({ status, workspaces } as model) =
    case msg of
        LayoutMsg gateID submsg ->
            { model
                | workspaces =
                    arrayUpdate gateID
                        (\gateData -> { gateData | layout = Layout.update submsg gateData.layout })
                        workspaces
            }

        SchematicMsg gateID submsg ->
            { model
                | workspaces =
                    arrayUpdate gateID
                        (\gateData -> { gateData | schematic = GateSchematic.update submsg gateData.schematic })
                        workspaces
            }

        AddGate ->
            { model | workspaces = Array.push initGate workspaces, status = Just (Schematic (Array.length workspaces)) }

        SelectGate mstat ->
            { model | status = mstat }

        ChangeGateName i newName ->
            { model | workspaces = arrayUpdate i (\a -> { a | name = newName }) workspaces }

        DeleteGate i ->
            { model
                | workspaces =
                    Array.append
                        (Array.slice 0 i workspaces)
                        (Array.slice (i + 1) (1 + Array.length workspaces) workspaces)
            }


arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate i f arr =
    case Array.get i arr of
        Nothing ->
            arr

        Just a ->
            Array.set i (f a) arr


view : Model -> Browser.Document Msg
view { workspaces, status } =
    let
        currentWorkspace =
            status
                |> Maybe.andThen
                    (\statusi ->
                        case statusi of
                            Schematic i ->
                                Maybe.map (.schematic >> GateSchematic.view >> Element.map (SchematicMsg i)) (Array.get i workspaces)

                            Layout i ->
                                Maybe.map (.layout >> Layout.view >> Element.map (LayoutMsg i)) (Array.get i workspaces)
                    )
    in
    { title = "VLISI"
    , body =
        [ Element.layout [] <|
            Element.column [ Element.width Element.fill, Element.height Element.fill ]
                [ workspaces
                    |> Array.toIndexedList
                    |> List.map
                        (\( i, a ) ->
                            if status == Just (Layout i) || status == Just (Schematic i) then
                                Element.column [ buttonBorder ]
                                    [ Element.Input.text [ Element.width Element.fill ]
                                        { label = Element.Input.labelHidden "GateName"
                                        , onChange = ChangeGateName i
                                        , placeholder = Just (Element.Input.placeholder [] (Element.text "MyGateName"))
                                        , text = a.name
                                        }
                                    , Element.row [ Element.spacing 10 ]
                                        [ Element.Input.button [ Element.centerX ]
                                            { onPress = Just (SelectGate (Just (Schematic i))), label = Element.text "Schematic" }
                                        , Element.Input.button [ Element.centerX ]
                                            { onPress = Just (SelectGate (Just (Layout i))), label = Element.text "Layout" }
                                        ]
                                    ]

                            else
                                Element.Input.button
                                    [ Element.height Element.fill, buttonBorder ]
                                    { onPress = Just (SelectGate (Just (Schematic i))), label = Element.text a.name }
                        )
                    |> (\l ->
                            l
                                ++ [ Element.Input.button [ Element.alignTop, buttonBorder ] { onPress = Just AddGate, label = Element.text "+New Gate" }
                                   , Element.Input.button [ Element.height Element.fill, Element.width Element.fill ] { onPress = Just (SelectGate Nothing), label = Element.none }
                                   ]
                       )
                    |> Element.row [ Element.width Element.fill, buttonBorder ]
                , currentWorkspace |> Maybe.withDefault Element.none
                ]
        ]
    }


buttonBorder : Element.Attribute msg
buttonBorder =
    Element.Border.width 2
