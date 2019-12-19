module Main exposing (Msg(..), main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (Html, button, div, li, p, text, ul)
import Html.Attributes as Att
import Html.Events exposing (onClick)
import Railroad as RR
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes exposing (..)
import Time exposing (posixToMillis)


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : RR.State
    , scale : Float
    , millis : Maybe Int
    , frame : Int
    }


type Msg
    = Tick Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = RR.sample
      , scale = 1
      , millis = Nothing
      , frame = 0
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
            ( { model | millis = Just newMillis, state = RR.moved duration model.state, frame = duration }, Cmd.none )


type alias Document msg =
    { title : String, body : List (Html msg) }


view : Model -> Document Msg
view model =
    { title = "Railroad"
    , body =
        [ Grid.container []
            [ Grid.row []
                [ Grid.col []
                    [ svg
                        [ height "600"
                        , viewBox "0 0 800 600"
                        , Att.style "border" "1px solid black"
                        , model.scale |> scaleTransform |> transform
                        ]
                        [ g [] (List.map trackToSvg model.state.layout.tracks)
                        , g [] (List.map connectorToSvg (RR.connectors model.state.layout))
                        , g [] (List.map trainToSvg model.state.trains)
                        ]
                    ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ trainList model.state.trains
                    ]
                ]
            ]
        ]
    }


scaleTransform : Float -> String
scaleTransform scale =
    let
        scaleStr =
            String.fromFloat scale
    in
    "scale(" ++ scaleStr ++ " " ++ scaleStr ++ ")"


trackToSvg : RR.Track -> Svg Msg
trackToSvg track =
    line
        [ track.from.pos.x |> String.fromFloat |> x1
        , track.from.pos.y |> String.fromFloat |> y1
        , track.to.pos.x |> String.fromFloat |> x2
        , track.to.pos.y |> String.fromFloat |> y2
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


connectorToSvg : RR.Connector -> Svg Msg
connectorToSvg conn =
    circle
        [ conn.pos.x |> String.fromFloat |> cx
        , conn.pos.y |> String.fromFloat |> cy
        , r "5"
        , fill "none"
        , stroke "grey"
        , strokeWidth "1"
        ]
        []


trainToSvg : RR.Train -> Svg Msg
trainToSvg train =
    g [] (List.map trackOccupancyToSvg (RR.tracksForTrain train))


trackOccupancyToSvg : RR.TrackOccupancy -> Svg msg
trackOccupancyToSvg occ =
    let
        tl =
            RR.trackLength occ.track

        dx =
            occ.track.to.pos.x - occ.track.from.pos.x

        dy =
            occ.track.to.pos.y - occ.track.from.pos.y
    in
    line
        [ occ.track.from.pos.x + dx * occ.from / tl |> String.fromFloat |> x1
        , occ.track.from.pos.y + dy * occ.from / tl |> String.fromFloat |> y1
        , occ.track.from.pos.x + dx * occ.to / tl |> String.fromFloat |> x2
        , occ.track.from.pos.y + dy * occ.to / tl |> String.fromFloat |> y2
        , stroke "red"
        , strokeLinecap "round"
        , strokeWidth "5"
        ]
        []


trainList : List RR.Train -> Html Msg
trainList trains =
    ul [] (List.map trainSpec trains)


trainSpec : RR.Train -> Html Msg
trainSpec train =
    li []
        [ "pos: "
            ++ String.fromFloat train.pos
            ++ ", speed: "
            ++ String.fromFloat train.speed
            ++ ", length: "
            ++ String.fromFloat train.length
            |> text
        ]
