module Railroad.TrackSegment exposing (TrackSegment(..), length, pointAlong)

import Basics.Extra
import CubicSpline2d exposing (ArcLengthParameterized, CubicSpline2d, arcLength, arcLengthParameterized, nondegenerate)
import Length exposing (Length)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity


type TrackSegment units coords
    = Straight (LineSegment2d units coords)
    | Bezier (CubicSpline2d units coords) (ArcLengthParameterized units coords)
    | Composite (List (TrackSegment units coords))
    | Null (Point2d units coords)


fromCubicSpline : CubicSpline2d units coords -> TrackSegment units coords
fromCubicSpline spline =
    case nondegenerate spline of
        Err p ->
            Null p

        Ok nd ->
            Bezier spline (arcLengthParameterized (Length.millimeters 1) nd)


length : TrackSegment Length.Meters coords -> Length
length ts =
    case ts of
        Straight ls ->
            LineSegment2d.length ls

        Bezier bs alps ->
            CubicSpline2d.arcLength alps

        Composite cs ->
            List.map length cs |> Quantity.sum


pointAlong : TrackSegment Length.Meters coords -> Length -> Point2d Length.Meters coords
pointAlong ts d =
    if Quantity.lessThanZero d then
        startPoint ts

    else if d |> Quantity.greaterThan (length ts) then
        endPoint ts

    else
        case ts of
            Straight ls ->
                LineSegment2d.interpolate ls (Quantity.ratio d (LineSegment2d.length ls))

            Bezier bs ->
                case CubicSpline2d.nondegenerate bs of
                    Ok nd ->
                        nd
                            |> CubicSpline2d.arcLengthParameterized { maxError = Length.meters 0.1 }
                            |> Basics.Extra.flip CubicSpline2d.pointAlong d

                    Err p2d ->
                        p2d

            Composite cs ->
                case cs of
                    [] ->
                        -- Should never happen
                        Point2d.origin

                    ts0 :: tsr ->
                        if length ts0 |> Quantity.lessThanOrEqualTo d then
                            pointAlong ts0 d

                        else
                            pointAlong (Composite tsr) (d |> Quantity.minus (length ts0))


startPoint : TrackSegment units coords -> Point2d units coords
startPoint ts =
    case ts of
        Straight ls ->
            LineSegment2d.startPoint ls

        Bezier bs ->
            CubicSpline2d.startPoint bs

        Composite cs ->
            case cs of
                [] ->
                    -- Should never happen
                    Point2d.origin

                ts0 :: _ ->
                    startPoint ts0


endPoint : TrackSegment units coords -> Point2d units coords
endPoint ts =
    case ts of
        Straight ls ->
            LineSegment2d.endPoint ls

        Bezier bs ->
            CubicSpline2d.endPoint bs

        Composite cs ->
            case cs of
                [] ->
                    -- Should never happen
                    Point2d.origin

                ts0 :: [] ->
                    endPoint ts0

                _ :: tsr ->
                    endPoint (Composite tsr)
