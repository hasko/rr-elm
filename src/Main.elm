module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Att
import Html.Events exposing (onClick)
import Length exposing (inMeters)
import Point2d exposing (Point2d)
import Railroad as RR
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    RR.State


type Msg
    = Increment
    | Decrement


init : Model
init =
    RR.sample


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ height "600", viewBox "0 0 800 600", Att.style "border" "1px solid black" ]
            [ g [] (List.map trackToSvg model.layout.tracks)
            , g [] (List.map connectorToSvg (RR.connectors model.layout))
            , g [] (List.map trainToSvg model.trains)
            ]
        ]


trackToSvg : RR.Track -> Svg Msg
trackToSvg track =
    line
        [ track.from.position |> Point2d.xCoordinate |> inMeters |> floor |> String.fromInt |> x1
        , track.from.position |> Point2d.yCoordinate |> inMeters |> floor |> String.fromInt |> y1
        , track.to.position |> Point2d.xCoordinate |> inMeters |> floor |> String.fromInt |> x2
        , track.to.position |> Point2d.yCoordinate |> inMeters |> floor |> String.fromInt |> y2
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


connectorToSvg : RR.Connector -> Svg Msg
connectorToSvg conn =
    circle
        [ conn.position |> Point2d.xCoordinate |> inMeters |> floor |> String.fromInt |> cx
        , conn.position |> Point2d.yCoordinate |> inMeters |> floor |> String.fromInt |> cy
        , r "5"
        , fill "none"
        , stroke "grey"
        , strokeWidth "1"
        ]
        []


trainToSvg : RR.Train -> Svg Msg
trainToSvg train =
    g [] (List.map trainSegmentToSvg (RR.tracksForTrain train))


trainSegmentToSvg : RR.Track -> Svg msg
trainSegmentToSvg track =
    line
        [ track.from.position |> Point2d.xCoordinate |> inMeters |> floor |> String.fromInt |> x1
        , track.from.position |> Point2d.yCoordinate |> inMeters |> floor |> String.fromInt |> y1
        , track.to.position |> Point2d.xCoordinate |> inMeters |> floor |> String.fromInt |> x2
        , track.to.position |> Point2d.yCoordinate |> inMeters |> floor |> String.fromInt |> y2
        , stroke "red"
        , strokeLinecap "round"
        , strokeWidth "5"
        ]
        []
