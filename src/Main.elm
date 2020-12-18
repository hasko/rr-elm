module Main exposing (Msg(..), main, update, view)

import Browser
import Graph exposing (empty, insertData, insertEdge)
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Maybe.Extra
import Railroad exposing (..)
import Svg exposing (Svg, g, line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, ry, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Task
import Time


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : TrainState
    , layout : Layout
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
    ( { state = TrainState "Happy Train" 30.0 10.0 0 40.0
      , layout = initialLayout
      , lastTick = Nothing
      , running = True
      }
    , Cmd.none
    )


initialLayout : Layout
initialLayout =
    Graph.empty
        |> insertEdge 0 1
        |> insertEdge 1 0
        |> insertData 0 (Track 50.0)
        |> insertData 1 (Track 100.0)


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
                            | state = Railroad.move elapsedMillis model.layout model.state
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
            let
                ( m, cmd ) =
                    init ()
            in
            ( { m | running = model.running }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ svg
            [ width "100%", height "50%", viewBox "0 0 100000 5000" ]
            [ tracksToSvg model.layout
            , rect
                [ x (String.fromFloat (1000.0 * ((toFloat model.state.track * 50.0) + model.state.trackPosition - model.state.length)))
                , y "1005"
                , width (String.fromFloat (model.state.length * 1000.0))
                , height "2990"
                , rx "15"
                , ry "15"
                , fill "#3B3332"
                ]
                []
            ]
        , button [ onClick Start, style "margin" "12px 12px 12px 12px" ] [ text "Start" ]
        , button [ onClick Stop, style "margin" "12px 12px 12px 0" ] [ text "Stop" ]
        , button [ onClick Reset, style "margin" "12px 12px 12px 0" ] [ text "Reset" ]
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


tracksToSvg : Layout -> Svg Msg
tracksToSvg layout =
    g []
        (List.map
            trackToSvg
            (Graph.nodes layout)
        )


trackToSvg : ( Int, Maybe Track ) -> Svg Msg
trackToSvg ( track_id, maybe_track ) =
    case maybe_track of
        Nothing ->
            g [] []

        Just track ->
            let
                px =
                    track_id * 50000

                lx =
                    track.length * 1000 |> floor
            in
            rect
                [ x (String.fromInt px)
                , y "1783"
                , width (String.fromInt lx)
                , height "1435"
                , fill ([ "red", "blue" ] |> List.drop track_id |> List.head |> Maybe.withDefault "green")
                , strokeWidth "100"
                ]
                []
