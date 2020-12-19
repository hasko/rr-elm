module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Graph exposing (empty, insertData, insertEdge)
import Html exposing (Html, button, div, pre, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Railroad exposing (..)
import Set exposing (Set)
import Svg exposing (Svg, g, line, path, rect, svg)
import Svg.Attributes exposing (d, fill, id, stroke, strokeWidth, viewBox, width, x1, x2, y1, y2)
import Time
import Tuple


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
    ( { state = TrainState "Happy Train" 30 10.0 0 40.0
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
        |> insertEdge 1 2
        |> insertEdge 2 0
        |> insertData 0 (StraightTrack { length = 50.0 })
        |> insertData 1 (CurvedTrack { radius = 300.0, angle = 15.0 })
        |> insertData 2 (StraightTrack { length = 100.0 })


subscriptions _ =
    Time.every 40 Tick


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
            [ width "100%", viewBox "0 -2.5 150 50" ]
            [ lazy viewLayout model.layout
            , viewTrain model.state model.layout
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


viewLayout : Layout -> Svg Msg
viewLayout layout =
    g [ id "layout" ]
        (Dict.foldl
            (\trackId cursor acc ->
                case Graph.getData trackId layout of
                    Nothing ->
                        -- TODO Something went wrong, our layout is inconsistent.
                        acc

                    Just track ->
                        viewTrack track cursor :: acc
            )
            []
            (Railroad.cursors layout)
        )


viewTrack : Track -> Cursor -> Svg Msg
viewTrack track cursor =
    let
        newCursor =
            Railroad.moveCursor cursor track
    in
    case track of
        StraightTrack s ->
            line
                [ x1 (cursor.x |> String.fromFloat)
                , y1 (cursor.y |> String.fromFloat)
                , x2 (newCursor.x |> String.fromFloat)
                , y2 (newCursor.y |> String.fromFloat)
                , stroke "blue"
                , strokeWidth "1.435"
                ]
                []

        CurvedTrack c ->
            path
                [ d
                    ("M "
                        ++ (cursor.x |> String.fromFloat)
                        ++ " "
                        ++ (cursor.y |> String.fromFloat)
                        ++ " A "
                        ++ (c.radius |> String.fromFloat)
                        ++ " "
                        ++ (c.radius |> String.fromFloat)
                        ++ " 0 "
                        ++ (if c.angle >= 180.0 then
                                "1"

                            else
                                "0"
                           )
                        ++ " 1 "
                        ++ (newCursor.x |> String.fromFloat)
                        ++ " "
                        ++ (newCursor.y |> String.fromFloat)
                    )
                , fill "none"
                , stroke "green"
                , strokeWidth "1.435"
                ]
                []


viewTrain : TrainState -> Layout -> Svg Msg
viewTrain train layout =
    case Railroad.cursors layout |> Dict.get train.track of
        -- TODO Something went wrong, our layout is inconsistent.
        Nothing ->
            g [] []

        Just c ->
            case Graph.getData train.track layout of
                -- TODO Something went wrong, our layout is inconsistent.
                Nothing ->
                    g [] []

                Just t ->
                    let
                        c1 =
                            Railroad.getPositionOnTrack train.trackPosition c t

                        ( ntp, nti ) =
                            Railroad.normalizePosition ( train.trackPosition - train.length, train.track ) layout

                        nt =
                            Graph.getData nti layout |> Maybe.withDefault (StraightTrack { length = 1000000.0 })

                        c2 =
                            Railroad.getPositionOnTrack (train.trackPosition - train.length) c nt
                    in
                    line
                        [ Svg.Attributes.id "train"
                        , x1 (c1.x |> String.fromFloat)
                        , y1 (c1.y |> String.fromFloat)
                        , x2 (c2.x |> String.fromFloat)
                        , y2 (c2.y |> String.fromFloat)
                        , stroke "#3B3332"
                        , strokeWidth "2.990"
                        ]
                        []
