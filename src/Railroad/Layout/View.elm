module Railroad.Layout.View exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
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
    | DragStart Float Float
    | DragMove Float Float
    | DragEnd


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
        , wheelHandler
        , mouseHandler
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


wheelHandler : Svg.Attribute Msg
wheelHandler =
    Svg.Events.on "wheel" (Decode.map ZoomWheel (Decode.field "deltaY" Decode.float))


mouseHandler : Svg.Attribute Msg
mouseHandler =
    Svg.Events.on "mousedown" (mouseDecoder DragStart)


mouseDecoder : (Float -> Float -> msg) -> Decode.Decoder msg
mouseDecoder msg =
    Decode.map2 msg
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ZoomWheel deltaY ->
            { model | zoom = max 0.1 (model.zoom + deltaY * -0.001) }

        DragStart x y ->
            { model | drag = Just { start = { x = x, y = y }, current = { x = x, y = y } } }

        DragMove x y ->
            case model.drag of
                Just dragState ->
                    let
                        dx =
                            (x - dragState.current.x) / model.zoom

                        dy =
                            (y - dragState.current.y) / model.zoom

                        newOffset =
                            { x = model.offset.x + dx
                            , y = model.offset.y + dy
                            }
                    in
                    { model
                        | offset = newOffset
                        , drag = Just { dragState | current = { x = x, y = y } }
                    }

                Nothing ->
                    model

        DragEnd ->
            { model | drag = Nothing }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (mouseDecoder DragMove)
                , Browser.Events.onMouseUp (Decode.succeed DragEnd)
                ]

        Nothing ->
            Sub.none
