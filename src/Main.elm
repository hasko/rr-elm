module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Railroad exposing (TrainState, move)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (height, rx, ry, width, x, y)
import Task
import Time


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : TrainState
    , lastTick : Maybe Int
    }


type Msg
    = Tick Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = TrainState "Happy Train" 30.0 10.0 0 50.0, lastTick = Nothing }, Cmd.none )


subscriptions _ =
    Time.every 200 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            case model.lastTick of
                Just lastMillis ->
                    let
                        newMillis =
                            Time.posixToMillis time

                        elapsedMillis =
                            toFloat (newMillis - lastMillis)
                    in
                    ( { model
                        | state = Railroad.move elapsedMillis model.state
                        , lastTick = Just newMillis
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | lastTick = Just (Time.posixToMillis time) }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ svg
            [ width "67%", height "50%" ]
            [ rect
                [ x "10", y "10", width "100", height "100", rx "15", ry "15" ]
                []
            ]
        ]
