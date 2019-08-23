module Views exposing (view)

import Element exposing (Element)
import Element.Font
import Element.Input
import Types exposing (..)


type ViewStatus
    = ActivePossible
    | InactivePossible
    | ActiveImpossible


view : List View -> List View -> Element Msg
view activeViews possibleViews =
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

        viewDict =
            List.foldl
                viewDictStep
                possibleDict
                activeViews
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 5
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.Font.center
            ]
          <|
            Element.text "Views:"
        , Element.table
            [ Element.height Element.fill
            , Element.spacingXY 0 5
            , Element.scrollbars
            ]
            { data = viewDict
            , columns =
                [ { header = Element.none
                  , width = Element.px 40
                  , view = viewViewStatus
                  }
                , { header = Element.none
                  , width = Element.fill
                  , view = Tuple.first >> viewViewLabel
                  }
                ]
            }
        ]


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


viewViewLabel : View -> Element msg
viewViewLabel v =
    Element.text <|
        case v of
            LabelsView ->
                "Labels"

            LayerView id ->
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

            LabelConnec label ->
                label ++ " connectivity"
