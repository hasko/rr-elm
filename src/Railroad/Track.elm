module Railroad.Track exposing
    ( Track(..)
    , decoder
    , encode
    , getPositionOnTrack
    , length
    , moveFrame
    , toSvg
    )

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Direction2d
import Frame2d
import Geometry.Svg as Gsvg
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Length exposing (Length)
import Point2d
import Quantity
import Railroad.Util exposing (Frame)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Vector2d


type Track
    = StraightTrack Length
    | CurvedTrack Length Angle
    | MapExit


length : Track -> Length
length track =
    case track of
        StraightTrack l ->
            l

        CurvedTrack r a ->
            Quantity.multiplyBy (Angle.inRadians a) r |> Quantity.abs

        MapExit ->
            -- Assume infinite length off map
            Quantity.positiveInfinity


connectorFrames : Track -> ( Frame, Frame )
connectorFrames track =
    case track of
        StraightTrack l ->
            ( Frame2d.atOrigin, Frame2d.atPoint (Point2d.xy l Quantity.zero) )

        CurvedTrack r a ->
            ( Frame2d.atOrigin
            , Frame2d.withAngle a
                (Point2d.xy
                    (Quantity.multiplyBy (Angle.cos a) r)
                    (Quantity.multiplyBy (Angle.sin a) r)
                )
            )

        MapExit ->
            -- Entry and exit are the same
            ( Frame2d.atOrigin, Frame2d.atOrigin )


moveFrame : Frame -> Track -> Frame
moveFrame cursor track =
    case track of
        StraightTrack l ->
            getPositionOnTrack l cursor track

        CurvedTrack r a ->
            let
                cursorDir =
                    Frame2d.xDirection cursor |> Direction2d.toAngle |> Angle.inDegrees

                newDir =
                    cursorDir + Angle.inDegrees a

                avgDirRad =
                    degrees ((cursorDir + newDir) / 2.0)

                s =
                    2 * Length.inMeters r * sin (degrees (abs (Angle.inDegrees a)) / 2)
            in
            cursor
                |> Frame2d.translateBy (Vector2d.meters (s * cos avgDirRad) (s * sin avgDirRad))
                |> Frame2d.rotateBy a

        MapExit ->
            -- Nothing to move
            cursor


getPositionOnTrack : Length -> Frame -> Track -> Frame
getPositionOnTrack trackPosition cursor track =
    case track of
        StraightTrack _ ->
            Frame2d.translateBy (Vector2d.xy trackPosition Quantity.zero) cursor

        CurvedTrack r a ->
            let
                -- Calculate amount of angle covered
                a2 =
                    Quantity.multiplyBy (Quantity.ratio trackPosition (length track)) a

                arc =
                    curveToArc r a2
                        |> Arc2d.placeIn cursor

                p =
                    Arc2d.endPoint arc
            in
            cursor |> Frame2d.moveTo p |> Frame2d.rotateBy a2

        MapExit ->
            -- Just assume the unseen track is straight
            Frame2d.translateBy (Vector2d.xy trackPosition Quantity.zero) cursor


curveToArc : Length -> Angle -> Arc2d Length.Meters coords
curveToArc r a =
    Point2d.origin
        |> Arc2d.sweptAround
            (Point2d.meters 0
                (if Quantity.greaterThanOrEqualToZero a then
                    Length.inMeters r

                 else
                    -(Length.inMeters r)
                )
            )
            a



-- SVG


toSvg : Track -> Bool -> List (Svg msg)
toSvg track active =
    let
        cc1 =
            connectorFrames track
                |> Tuple.second
                |> Frame2d.originPoint
                |> Point2d.toRecord Length.inMeters

        strokeColor =
            if active then
                "gray"

            else
                "lightGray"
    in
    case track of
        StraightTrack _ ->
            [ Svg.line
                [ SA.x1 "0"
                , SA.y1 "0"
                , cc1.x |> String.fromFloat |> SA.x2
                , cc1.y |> String.fromFloat |> SA.y2
                , SA.stroke strokeColor
                , SA.strokeWidth "1.435"
                ]
                []
            , trackDelimiter
            ]

        CurvedTrack r a ->
            let
                arc =
                    curveToArc r a
            in
            [ Gsvg.arc2d
                [ SA.fill "none"
                , SA.stroke strokeColor
                , SA.strokeWidth "1.435"
                ]
                arc
            , trackDelimiter
            ]

        MapExit ->
            -- TODO draw a nice icon or something
            []


trackDelimiter : Svg msg
trackDelimiter =
    Svg.line
        [ SA.x1 "0"
        , SA.y1 "-1"
        , SA.x2 "0"
        , SA.y2 "1"
        , SA.stroke "black"
        , SA.strokeWidth "1px"
        , Html.Attributes.attribute "vector-effect" "non-scaling-stroke"
        ]
        []



-- JSON encode


encode : Track -> Value
encode track =
    case track of
        StraightTrack l ->
            Encode.object [ ( "length", Encode.float (Length.inMeters l) ) ]

        CurvedTrack r a ->
            Encode.object
                [ ( "radius", Encode.float (Length.inMeters r) )
                , ( "sweep", Encode.float (Angle.inDegrees a) )
                ]

        MapExit ->
            Encode.object [ ( "type", Encode.string "map-exit" ) ]



-- JSON decode


decoder : Decoder Track
decoder =
    Decode.oneOf [ straightDecoder, curveDecoder ]


straightDecoder : Decoder Track
straightDecoder =
    Decode.field "length" Decode.float
        |> Decode.map Length.meters
        |> Decode.map StraightTrack


curveDecoder : Decoder Track
curveDecoder =
    Decode.map2 CurvedTrack
        (Decode.field "radius" Decode.float |> Decode.map Length.meters)
        (Decode.field "sweep" Decode.float |> Decode.map Angle.degrees)
