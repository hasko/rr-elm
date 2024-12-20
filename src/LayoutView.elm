module LayoutView exposing (Model, Msg(..), init, update, view)

import Html exposing (Html)
import Json.Decode as Decode
import Rect exposing (Rect)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as SA
import Svg.Events


type alias Model =
    { zoom : Float
    , offset : { x : Float, y : Float }
    , drag : Maybe DragState
    }


type Msg
    = ZoomWheel Float


type alias DragState =
    { start : { x : Float, y : Float }
    , current : { x : Float, y : Float }
    }


init : Model
init =
    { zoom = 1.0
    , offset = { x = 0, y = 0 }
    , drag = Nothing
    }


view : Rect -> String -> List (Svg Msg) -> Model -> Html Msg
view viewBox bgColor elements model =
    svg
        [ SA.width "100%"
        , SA.viewBox (viewBox |> Rect.rectToString)
        , onWheel ZoomWheel
        ]
        [ rect (Rect.rectToAttrib viewBox ++ [ SA.fill bgColor ]) []
        , Svg.g
            [ SA.id "layout"
            , SA.transform
                ("translate("
                    ++ String.fromFloat model.offset.x
                    ++ ","
                    ++ String.fromFloat model.offset.y
                    ++ ") "
                    ++ "scale("
                    ++ String.fromFloat model.zoom
                    ++ ")"
                )
            ]
            elements
        ]


onWheel : (Float -> msg) -> Svg.Attribute msg
onWheel toMsg =
    Svg.Events.on "wheel" (Decode.map toMsg (Decode.field "deltaY" Decode.float))


update : Msg -> Model -> Model
update msg model =
    case msg of
        ZoomWheel deltaY ->
            { model | zoom = max 0.1 (model.zoom + deltaY * -0.001) }
