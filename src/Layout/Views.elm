module Layout.Views exposing (arrange, defaultViews, sort, view)

import Element exposing (Element)
import Element.Font
import Element.Input
import Layout.Types exposing (..)


defaultViews : List View
defaultViews =
    List.map LayerView layerIDs ++ [ LabelsView ]


type ViewStatus
    = ActivePossible
    | InactivePossible
    | ActiveImpossible


arrange : List View -> List View -> List ( View, ViewStatus )
arrange activeViews possibleViews =
    let
        possibleDict =
            List.map (\v -> Tuple.pair v InactivePossible) possibleViews

        viewDictStep v dict =
            if List.member ( v, InactivePossible ) dict then
                List.map
                    (\( key, val ) ->
                        if key == v then
                            ( key, ActivePossible )

                        else
                            ( key, val )
                    )
                    dict

            else if not <| List.any (\( key, _ ) -> key == v) dict then
                ( v, ActiveImpossible ) :: dict

            else
                -- Already inserted.
                dict
    in
    List.foldl
        viewDictStep
        possibleDict
        activeViews


sort : List ( View, ViewStatus ) -> List View
sort =
    List.filterMap
        (\( v, val ) ->
            case val of
                ActivePossible ->
                    Just v

                _ ->
                    Nothing
        )


view : List ( View, ViewStatus ) -> Element Msg
view views =
    Element.table
        [ Element.height Element.fill
        , Element.spacingXY 0 5
        , Element.scrollbars
        ]
        { data = List.reverse views
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view = viewViewStatus
              }
            , { header = Element.none
              , width = Element.shrink
              , view = viewSelectLayerButton
              }
            , { header = Element.text "Views:"
              , width = Element.fill
              , view = viewViewLabel
              }
            ]
        }


viewSelectLayerButton : ( View, ViewStatus ) -> Element Msg
viewSelectLayerButton v =
    case v of
        ( LayerView id, ActivePossible ) ->
            Element.Input.button []
                { onPress = Just <| PickLayer id
                , label = Element.text "✎"
                }

        _ ->
            Element.none


viewViewStatus : ( View, ViewStatus ) -> Element Msg
viewViewStatus ( v, val ) =
    let
        button ( eye, attrs ) =
            Element.Input.button
                (attrs ++ [ Element.Font.center ])
                { onPress = Just <| ToggleView v, label = Element.text eye }
    in
    button <|
        case val of
            ActivePossible ->
                ( "👁", [] )

            ActiveImpossible ->
                ( "👁", [ Element.Font.strike ] )

            InactivePossible ->
                ( "-", [] )


viewViewLabel : ( View, ViewStatus ) -> Element Msg
viewViewLabel ( v, vs ) =
    case v of
        LabelsView ->
            Element.text "Labels"

        LayerView id ->
            Element.Input.button []
                { onPress = Just <| PickLayer id
                , label =
                    Element.text <|
                        case id of
                            Nwell ->
                                "N-well"

                            Ndiff ->
                                "N-diffusion"

                            Pdiff ->
                                "P-diffusion"

                            Metal ->
                                "Metal"

                            Polysilicon ->
                                "Polysilicon"

                            Contacts ->
                                "Contacts"
                }

        LabelConnec label ->
            Element.text <| label ++ " connectivity"
