module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (Device, Element)
import Element.Border
import Grid exposing (Grid)
import Grid.Render
import Layers exposing (Layers)
import Svg exposing (Svg)
import Svg.Attributes as SA


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = LayersMsg Layers.Msg
    | DeviceClassified Device
    | Noop


type alias Model =
    { layers : Layers
    , device : Device
    }


init : { innerWidth : Int, innerHeight : Int } -> ( Model, Cmd Msg )
init { innerWidth, innerHeight } =
    let
        layers =
            Layers.init

        device =
            Element.classifyDevice { width = innerWidth, height = innerHeight }
    in
    ( { layers = layers, device = device }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Noop ->
            model

        DeviceClassified device ->
            model

        LayersMsg lmsg ->
            { model | layers = Layers.update lmsg model.layers }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "VLISI"
    , body =
        Layers.view model.layers
            |> Element.map LayersMsg
            |> Element.layout []
            |> List.singleton
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize <|
        \width height ->
            DeviceClassified (Element.classifyDevice { width = width, height = height })
