module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Att
import Html.Events exposing (onClick)
import Railroad exposing (Layout)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Railroad.Layout


type Msg
    = Increment
    | Decrement


init : Layout
init =
    Railroad.sample


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
            (List.map trackToLine model.tracks)
        ]


trackToLine : Railroad.Track -> Svg Msg
trackToLine track =
    line
        [ x1 (String.fromInt track.from.position.x)
        , y1 (String.fromInt track.from.position.y)
        , x2 (String.fromInt track.to.position.x)
        , y2 (String.fromInt track.to.position.y)
        , stroke "black"
        , strokeLinecap "round"
        ]
        []
