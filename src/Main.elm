module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Att
import Html.Events exposing (onClick)
import Railroad as RR
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)


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
        [ x1 (String.fromInt track.from.position.x)
        , y1 (String.fromInt track.from.position.y)
        , x2 (String.fromInt track.to.position.x)
        , y2 (String.fromInt track.to.position.y)
        , stroke "black"
        , strokeLinecap "round"
        ]
        []


connectorToSvg : RR.Connector -> Svg Msg
connectorToSvg conn =
    circle
        [ cx (String.fromInt conn.position.x)
        , cy (String.fromInt conn.position.y)
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
        [ x1 (String.fromInt track.from.position.x)
        , y1 (String.fromInt track.from.position.y)
        , x2 (String.fromInt track.to.position.x)
        , y2 (String.fromInt track.to.position.y)
        , stroke "red"
        , strokeLinecap "round"
        , strokeWidth "5"
        ]
        []
