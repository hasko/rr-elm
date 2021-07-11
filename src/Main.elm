module Main exposing (main)

import Browser
import Cursor exposing (Cursor)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (g, svg)
import Svg.Attributes as SA
import Track exposing (..)


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    List Track


type Msg
    = TrackMsg Track.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( [ Track
            { x = 10.0, y = 10.0, dir = degrees 0 }
            (Point { length = 90, radius = 300, sweep = degrees 15, state = Straight })
      , Track
            { x = 10.0, y = 50.0, dir = degrees 15 }
            (Point { length = 90, radius = 300, sweep = degrees 15, state = Straight })
      ]
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "justify-content" "center" ]
        [ svg [ SA.viewBox "0 0 600 400", SA.height "400px" ]
            [ g []
                [ Svg.rect [ SA.x1 "0", SA.y1 "0", SA.width "600", SA.height "400", SA.fill "#94e3a9" ] []
                , g [] (List.map (\track -> Track.render track (\trackMsg -> TrackMsg trackMsg)) model)
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
