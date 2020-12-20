module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Graph exposing (empty, insertData, insertEdge)
import Html exposing (Html, button, div, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Railroad exposing (..)
import Railroad.Layout as Layout exposing (..)
import Railroad.Train as Train exposing (..)
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
        |> insertEdge 0 3
        |> insertEdge 3 0
        |> insertData 3 (StraightTrack { length = 100.0 })


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
                            | state = Train.move elapsedMillis model.layout model.state
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
        , div [ class "row" ]
            [ div [ class "col-3" ]
                [ button [ class "btn btn-secondary", onClick Start, style "margin" "12px 12px 12px 12px" ] [ text "Start" ]
                , button [ class "btn btn-secondary", onClick Stop, style "margin" "12px 12px 12px 0" ] [ text "Stop" ]
                , button [ class "btn btn-secondary", onClick Reset, style "margin" "12px 12px 12px 0" ] [ text "Reset" ]
                ]
            , div [ class "col" ]
                [ viewSwitches model.layout
                ]
            ]
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
    -- Create a g element to contain the layout.
    g [ id "layout" ]
        -- Collect the rendered elements into a list.
        (Dict.foldl
            -- To render the current track id into the list of elements:
            (\trackId cursor elements ->
                case Graph.getData trackId layout of
                    Nothing ->
                        -- If there is no such track, just return the current list unchanged.
                        elements

                    Just track ->
                        -- Render the track and add the result to the front of the list.
                        viewTrack track cursor :: elements
            )
            -- Start with an empty list of elements.
            []
            -- Iterate over all the cursors in the layout.
            (cursors layout)
        )


viewTrack : Track -> Cursor -> Svg Msg
viewTrack track cursor =
    let
        newCursor =
            moveCursor cursor track
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
    case cursors layout |> Dict.get train.track of
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
                            getPositionOnTrack train.trackPosition c t

                        ( ntp, nti ) =
                            Railroad.normalizePosition ( train.trackPosition - train.length, train.track ) layout

                        nt =
                            Graph.getData nti layout |> Maybe.withDefault (StraightTrack { length = 1000000.0 })

                        c2 =
                            getPositionOnTrack (train.trackPosition - train.length) c nt
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


viewSwitches : Layout -> Html Msg
viewSwitches layout =
    table [ class "table" ]
        [ thead [] [ tr [] [ th [] [ text "ID" ], th [] [ text "Connections" ], th [] [ text "Active" ], th [] [] ] ]
        , tbody []
            (Layout.switches layout
                |> List.indexedMap
                    (\i conns ->
                        tr []
                            [ td [] [ text (String.fromInt i) ]
                            , td []
                                [ List.map (\( inc, out ) -> String.fromInt inc ++ Html.Entity.rarr ++ String.fromInt out) conns
                                    |> String.join ", "
                                    |> text
                                ]
                            , td [] []
                            , td [] []
                            ]
                    )
            )
        ]
