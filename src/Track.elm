module Track exposing (Msg, PointState(..), Track, TrackDetails(..), render)

import Cursor exposing (Cursor, apply, cursorToTransformation, homCoords, homCoordsToXY)
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Track =
    { loc : Cursor, details : TrackDetails }


type TrackDetails
    = Point { length : Float, radius : Float, sweep : Float, state : PointState }


type PointState
    = Straight
    | Curved


type Msg
    = Noop


render : Track -> (Msg -> msg) -> Svg msg
render track teacher =
    case track.details of
        Point d ->
            let
                ( x0, y0 ) =
                    ( track.loc.x, track.loc.y )

                ( x1, y1 ) =
                    Debug.log "c2t" (cursorToTransformation track.loc) |> apply (homCoords (Cursor d.length 0 0)) |> homCoordsToXY

                {-
                   ( x0 + d.length * cos track.loc.dir
                   , y0 + d.length * sin track.loc.dir
                   )
                -}
                ( x2, y2 ) =
                    ( x0 + d.radius * (sin d.sweep - sin track.loc.dir)
                    , y0 + d.radius * (cos track.loc.dir - cos d.sweep)
                    )
            in
            Svg.g []
                [ Svg.line
                    [ SA.x1 (String.fromFloat x0)
                    , SA.y1 (String.fromFloat y0)
                    , SA.x2 (String.fromFloat x1)
                    , SA.y2 (String.fromFloat y1)
                    , SA.stroke "black"
                    , SA.strokeWidth "1.435"
                    ]
                    []
                , Svg.path
                    [ SA.d
                        ("M "
                            ++ point2String ( x0, y0 )
                            ++ " A "
                            ++ point2String ( d.radius, d.radius )
                            ++ ",0 "
                            ++ (if d.sweep > 180.0 then
                                    "1"

                                else
                                    "0"
                               )
                            ++ ",1 "
                            ++ point2String ( x2, y2 )
                        )
                    , SA.fill "none"
                    , SA.stroke "black"
                    , SA.strokeWidth "1.435"
                    ]
                    []
                ]


point2String : ( Float, Float ) -> String
point2String ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y
