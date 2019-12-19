module Main exposing (Msg(..), main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes as Att
import Html.Events exposing (onClick)
import Railroad as RR
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    { state : RR.State
    , scale : Float
    }


type Msg
    = Increment
    | Decrement


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = RR.sample
      , scale = 1
      }
    , Cmd.none
    )


subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


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
