module Railroad.Util exposing (Frame, WorldCoords)

import Frame2d exposing (Frame2d)
import Length exposing (Meters)


type WorldCoords
    = WorldCoords


type LocalCoords
    = LocalCoords


type alias Frame =
    Frame2d Meters WorldCoords { defines : LocalCoords }
