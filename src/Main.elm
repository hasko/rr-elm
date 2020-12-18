module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Railroad exposing (TrainState, move)
import Svg exposing (line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, ry, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Task
import Time


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : TrainState
    , lastTick : Maybe Int
    , running : Bool
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = TrainState "Happy Train" 30.0 10.0 0 50.0, lastTick = Nothing, running = True }, Cmd.none )


subscriptions _ =
    Time.every 200 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            if model.running then
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

            else
                ( { model | lastTick = Just (Time.posixToMillis time) }, Cmd.none )

        Start ->
            ( { model | running = True }, Cmd.none )

        Stop ->
            ( { model | running = False }, Cmd.none )

        Reset ->
            init ()


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ svg
            [ width "100%", height "50%", viewBox "0 0 100000 5000" ]
            [ line [ x1 "0", y1 "1783", x2 "100000", y2 "1783", stroke "black", strokeWidth "100" ] []
            , line [ x1 "0", y1 "3218", x2 "100000", y2 "3218", stroke "black", strokeWidth "100" ] []
            , rect
                [ x (String.fromFloat (1000.0 * (model.state.trackPosition - model.state.length)))
                , y "1005"
                , width (String.fromFloat (model.state.length * 1000.0))
                , height "2990"
                , rx "15"
                , ry "15"
                , fill "#3B3332"
                ]
                []
            ]
        , button [ onClick Start, style "margin" "12px 12px 0 12px" ] [ text "Start" ]
        , button [ onClick Stop, style "margin" "12px 12px 0 0" ] [ text "Stop" ]
        , button [ onClick Reset, style "margin" "12px 12px 0 0" ] [ text "Reset" ]
        , pre []
            [ text
                ("{ name='"
                    ++ model.state.name
                    ++ "'\n, length="
                    ++ String.fromFloat model.state.length
                    ++ "\n, speed="
                    ++ String.fromFloat model.state.speed
                    ++ "\n, track="
                    ++ String.fromInt model.state.track
                    ++ "\n, trackPosition="
                    ++ String.fromFloat model.state.trackPosition
                    ++ "\n}"
                )
            ]
        ]
