module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, br, button, div, li, p, table, td, text, th, tr, ul)
import Html.Attributes as Att exposing (attribute, disabled, scope)
import Html.Entity exposing (nbsp)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Railroad as RR
import Railroad.Layout as Layout
import Railroad.Orientation as Orientation exposing (Orientation(..), byOrientation)
import Round
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes exposing (..)
import Time exposing (posixToMillis)


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { initialState : Maybe RR.State
    , state : Maybe RR.State
    , scale : Float
    , millis : Maybe Int
    , frame : Int
    , isRunning : Bool
    }


type Msg
    = Tick Time.Posix
    | Start
    | Stop
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    let
        s =
            RR.sample
    in
    ( { initialState = s
      , state = s
      , scale = 1
      , millis = Nothing
      , frame = 0
      , isRunning = False
      }
    , Cmd.none
    )


subscriptions _ =
    Time.every 100 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                newMillis =
                    posixToMillis newTime

                duration =
                    case model.millis of
                        Nothing ->
                            0

                        Just m ->
                            newMillis - m
            in
            ( { model
                | millis = Just newMillis
                , state =
                    case model.state of
                        Just s ->
                            if model.isRunning then
                                Just (RR.moved duration s)

                            else
                                model.state

                        Nothing ->
                            Nothing
                , frame = duration
              }
            , Cmd.none
            )

        Start ->
            ( { model | isRunning = True }, Cmd.none )

        Stop ->
            ( { model | isRunning = False }, Cmd.none )

        Reset ->
            ( { model | state = model.initialState }, Cmd.none )


type alias Document msg =
    { title : String, body : List (Html msg) }


view : Model -> Document Msg
view model =
    { title = "Railroad"
    , body =
        [ div [ class "container" ]
            [ div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ svg
                        [ width "100%"
                        , height "100%"
                        , Att.style "border" "1px solid black"
                        , model.scale |> scaleTransform |> transform
                        ]
                        (case model.state of
                            Just state ->
                                [ g [] (List.map trackToSvg (Layout.tracks state.layout))
                                , g [] (List.map connectorToSvg (Layout.connectors state.layout))
                                , g [] (List.map trainToSvg state.trains)
                                ]

                            Nothing ->
                                []
                        )
                    ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ lazy viewSimulationControls model.isRunning ]
                ]
            , div [ class "row mt-3" ]
                [ div [ class "col" ]
                    [ case model.state of
                        Just stateRR ->
                            lazy viewTrains stateRR.trains

                        Nothing ->
                            div [] []
                    ]
                ]
            ]
        ]
    }


viewSimulationControls : Bool -> Html Msg
viewSimulationControls isRunning =
    div []
        [ if isRunning then
            button [ class "btn btn-primary", onClick Stop ] [ text "Stop simulation" ]

          else
            button [ class "btn btn-primary", onClick Start ] [ text "Start simulation" ]
        , button
            [ class "btn btn-secondary ml-2"
            , if isRunning then
                disabled True

              else
                onClick Reset
            ]
            [ text "Reset" ]
        ]


viewTrains : List RR.Train -> Html Msg
viewTrains trains =
    table [ class "table" ]
        (tr []
            [ th [ scope "col" ] [ text "Track" ]
            , th [ class "text-right", scope "col" ] [ text "Pos" ]
            , th [ scope "col" ] [ text "Orientation" ]
            , th [ class "text-right", scope "col" ] [ text "Speed" ]
            , th [ class "text-right", scope "col" ] [ text "Length" ]
            , th [ scope "col" ] [ text "State" ]
            ]
            :: List.map
                (\train ->
                    tr []
                        [ td []
                            [ text (String.fromInt (Layout.getTrackId train.loc.track)) ]
                        , td [ class "text-right" ] [ text (Round.round 1 train.loc.pos) ]
                        , td [] [ text (Orientation.toString train.loc.orient) ]
                        , td [ class "text-right" ] [ text (Round.round 1 train.speed) ]
                        , td [ class "text-right" ] [ text (Round.round 1 train.length) ]
                        , td []
                            [ case train.state of
                                RR.Normal ->
                                    text nbsp

                                RR.EmergencyStop ->
                                    text "Emergency stop"

                                RR.Crashed ->
                                    text "Crashed"

                                RR.OffMap ->
                                    text "Off map"
                            ]
                        ]
                )
                trains
        )


scaleTransform : Float -> String
scaleTransform scale =
    let
        scaleStr =
            String.fromFloat scale
    in
    "scale(" ++ scaleStr ++ " " ++ scaleStr ++ ")"


trackToSvg : Layout.Track -> Svg Msg
trackToSvg track =
    let
        conns =
            Layout.getConnectors track

        from =
            Layout.getPosition conns.from

        to =
            Layout.getPosition conns.to
    in
    line
        [ from.x |> String.fromFloat |> x1
        , from.y |> String.fromFloat |> y1
        , to.x |> String.fromFloat |> x2
        , to.y |> String.fromFloat |> y2
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


connectorToSvg : Layout.Connector -> Svg Msg
connectorToSvg conn =
    let
        pos =
            Layout.getPosition conn
    in
    circle
        [ pos.x |> String.fromFloat |> cx
        , pos.y |> String.fromFloat |> cy
        , r "5"
        , fill "none"
        , stroke "grey"
        , strokeWidth "1"
        ]
        []


trainToSvg : RR.Train -> Svg Msg
trainToSvg train =
    g [] (trainToSvgRecursive train.loc train.length [])


trainToSvgRecursive : RR.Location -> Float -> List (Svg Msg) -> List (Svg Msg)
trainToSvgRecursive loc length svgList =
    if length <= 0 then
        svgList

    else
        let
            newPos =
                loc.pos - Orientation.byOrientation loc.orient length
        in
        trackSegment loc.track loc.pos (clamp 0 (Layout.trackLength loc.track) newPos)
            --TODO Recurse!
            :: svgList


trackSegment : Layout.Track -> Float -> Float -> Svg msg
trackSegment track fromPos toPos =
    let
        tl =
            Layout.trackLength track

        conns =
            Layout.getConnectors track

        from =
            Layout.getPosition conns.from

        to =
            Layout.getPosition conns.to

        dx =
            to.x - from.x

        dy =
            to.y - from.y
    in
    --TODO Curves and such.
    line
        [ from.x + dx * fromPos / tl |> String.fromFloat |> x1
        , from.y + dy * fromPos / tl |> String.fromFloat |> y1
        , from.x + dx * toPos / tl |> String.fromFloat |> x2
        , from.y + dy * toPos / tl |> String.fromFloat |> y2
        , stroke "red"
        , strokeLinecap "round"
        , strokeWidth "5"
        ]
        []


buttonGroup content =
    div [ class "btn-group", attribute "role" "group" ] content
