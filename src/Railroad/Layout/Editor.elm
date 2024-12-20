module Railroad.Layout.Editor exposing (Model, Msg(..), init, update, view)

import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode
import Railroad.Layout as Layout
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { zoom : Float
    , offset : { x : Float, y : Float }
    , drag : Maybe DragState
    }


type alias DragState =
    { start : { x : Float, y : Float }
    , current : { x : Float, y : Float }
    }


type Msg
    = ZoomChanged Float
    | DragStart Float Float
    | DragMove Float Float
    | DragEnd


init : Model
init =
    { zoom = 1.0
    , offset = { x = 0, y = 0 }
    , drag = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ZoomChanged newZoom ->
            { model | zoom = newZoom }

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


mouseDecoder : (Float -> Float -> msg) -> Decode.Decoder msg
mouseDecoder msg =
    Decode.map2 msg
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


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


view : Layout.Layout -> Model -> Html Msg
view layout model =
    Layout.editSvg
        layout
        model.zoom
        { mouseHandler = SE.on "mousedown" (mouseDecoder DragStart)
        , offset = model.offset
        }
