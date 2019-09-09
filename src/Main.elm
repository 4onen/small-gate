module Main exposing (main)

import Browser
import Element
import Layout


main =
    Browser.document
        { init = \() -> ( Layout.init, Cmd.none )
        , view = \model -> { title = "VLISI", body = [ Element.layout [] <| Layout.view model ] }
        , update = \msg model -> ( Layout.update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
