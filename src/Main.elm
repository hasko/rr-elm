module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Graph
import Html exposing (Html, br, button, div, li, pre, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, rowspan, style)
import Html.Entity
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy, lazy2)
import List.Extra
import Maybe exposing (andThen, withDefault)
import Railroad.Layout as Layout exposing (..)
import Railroad.Track as Track exposing (Track(..))
import Railroad.Train as Train exposing (..)
import Rect
import Round
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
    , switchState : Dict Int Int
    , lastTick : Maybe Int
    , running : Bool
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | Reset
    | ChangeSwitch Int Switch


init : () -> ( Model, Cmd Msg )
init _ =
    let
        l =
            Layout.initialLayout
    in
    ( { state =
            { name = "Happy Train"
            , length = 30
            , speed = 10.0
            , location = Train.initialLocation l
            }
      , layout = l
      , switchState = Dict.empty
      , lastTick = Nothing
      , running = True
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 40 Tick



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            updateTick (Time.posixToMillis time) model

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

        ChangeSwitch i switch ->
            let
                -- Get the next configuration number, cycling around if necessary.
                newCfg =
                    modBy (List.length switch.configs) ((Dict.get i model.switchState |> withDefault 0) + 1)

                -- Get the new switch state for the model.
                newState =
                    Dict.insert i newCfg model.switchState
            in
            -- Construct the new model based on the new switch state.
            ( { model | switchState = newState }, Cmd.none )


updateTick : Int -> Model -> ( Model, Cmd Msg )
updateTick newMillis model =
    if model.running then
        case model.lastTick of
            Just lastMillis ->
                let
                    elapsedMillis =
                        newMillis - lastMillis

                    newTrainState =
                        Train.move model.layout model.switchState elapsedMillis model.state
                in
                case newTrainState.location of
                    Nothing ->
                        -- Train could not move, stop it immediately.
                        ( { model | state = Train.stopped model.state }, Cmd.none )

                    Just loc ->
                        ( { model
                            | state = newTrainState
                            , lastTick = Just newMillis
                          }
                        , Cmd.none
                        )

            Nothing ->
                -- First tick in the game
                ( { model | lastTick = Just newMillis }, Cmd.none )

    else
        -- Not running
        ( { model | lastTick = Just newMillis }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ svg
            [ width "100%"
            , viewBox
                (model.layout
                    |> Layout.boundingBox
                    |> Rect.expand 5
                    |> Rect.rectToString
                )
            ]
            [ lazy Layout.toSvg model.layout
            , viewTrain model.state model.layout model.switchState
            ]
        , div [ class "row" ]
            [ div [ class "col-3" ]
                [ button [ class "btn btn-secondary", onClick Start, style "margin" "12px 12px 12px 12px" ] [ text "Start" ]
                , button [ class "btn btn-secondary", onClick Stop, style "margin" "12px 12px 12px 0" ] [ text "Stop" ]
                , button [ class "btn btn-secondary", onClick Reset, style "margin" "12px 12px 12px 0" ] [ text "Reset" ]
                ]
            , div [ class "col" ] [ lazy2 viewSwitches model.layout model.switchState ]
            , div [ class "col-3" ]
                [ table [ class "table" ]
                    [ tr [] [ th [] [ text "Name" ], td [] [ text model.state.name ] ]
                    , tr [] [ th [] [ text "Length" ], td [] [ text (String.fromFloat model.state.length) ] ]
                    , tr []
                        [ th [] [ text "Speed" ]
                        , td []
                            [ text
                                (String.fromFloat model.state.speed
                                    ++ " m/s ("
                                    ++ Round.round 1 (model.state.speed * 3.6)
                                    ++ " km/h)"
                                )
                            ]
                        ]
                    , tr []
                        [ th [] [ text "Location" ]
                        , td []
                            (case model.state.location of
                                Nothing ->
                                    [ text "Nowhere" ]

                                Just loc ->
                                    [ text
                                        ("edge ("
                                            ++ String.fromInt (Tuple.first loc.edge)
                                            ++ ", "
                                            ++ String.fromInt (Tuple.second loc.edge)
                                            ++ ")"
                                        )
                                    , br [] []
                                    , text
                                        ("pos "
                                            ++ Round.round 2 loc.pos
                                            ++ " m"
                                        )
                                    ]
                            )
                        ]
                    ]
                ]
            ]
        ]


viewTrain : TrainState -> Layout -> Dict Int Int -> Svg Msg
viewTrain train layout switchState =
    case train.location of
        Nothing ->
            g [] []

        Just loc ->
            case Layout.coordsFor loc.pos loc.edge layout of
                -- If the coordinates are unknown ...
                Nothing ->
                    -- ... draw nothing.
                    g [] []

                Just c1 ->
                    case { loc | pos = loc.pos - train.length } |> Train.normalizeLocation layout switchState of
                        Nothing ->
                            -- If the train end fits on no track, draw nothing.
                            g [] []

                        Just trainEndLocation ->
                            case Layout.coordsFor trainEndLocation.pos trainEndLocation.edge layout of
                                -- Train end is not on any track.
                                Nothing ->
                                    g [] []

                                Just c2 ->
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


viewSwitches : Layout -> Dict Int Int -> Html Msg
viewSwitches layout switchState =
    table [ class "table" ]
        [ thead [] [ tr [] [ th [] [ text "ID" ], th [] [ text "Connections" ], th [] [ text "Active" ], th [] [] ] ]
        , tbody []
            (Layout.switches layout |> List.map (\( i, switch ) -> viewSwitch i switch (Dict.get i switchState |> withDefault 0)))
        ]


viewSwitch : Int -> Switch -> Int -> Html Msg
viewSwitch i switch state =
    tr []
        [ td [] [ text (String.fromInt i) ]
        , td []
            [ ul [] (List.map viewSwitchConfig switch.configs |> List.map (\t -> li [] [ t ])) ]
        , td []
            [ case List.Extra.getAt state switch.configs of
                Nothing ->
                    -- Switch state is inconsistent
                    text "inconsistent"

                Just cfg ->
                    viewSwitchConfig cfg
            ]
        , td [] [ button [ class "btn btn-secondary btn-sm", onClick (ChangeSwitch i switch) ] [ text "Change" ] ]
        ]


viewSwitchConfig : List ( Int, Int ) -> Html Msg
viewSwitchConfig routes =
    routes
        |> List.map (\( from, to ) -> String.fromInt from ++ Html.Entity.rarr ++ String.fromInt to)
        |> String.join ", "
        |> text
