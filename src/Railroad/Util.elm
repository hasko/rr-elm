module Railroad.Util exposing (Frame, World)

import Frame2d exposing (Frame2d)
import Length exposing (Meters)


type World
    = World


type alias Frame =
    Frame2d Meters World {}
