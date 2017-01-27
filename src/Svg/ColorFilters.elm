module Svg.ColorFilters exposing (..)

import Svg.Operations exposing (colorMatrix)
import Svg.Operations.Internal as Internal exposing (ColorMatrixData(..), FilterOperation)


{-| Takes the red, green and blue components to calculate a luminance value, that is used for the alpha channel.
-}
luminanceToAlpha : FilterOperation msg -> FilterOperation msg
luminanceToAlpha =
    Internal.colorMatrix [] LuminanceToAlpha


{-| Rotate the color in the input around the color wheel
(i.e. rotating by 120 degrees (a third of the circle) turns red into green, green into blue and blue into red).
-}
hueRotate : Float -> FilterOperation msg -> FilterOperation msg
hueRotate angle =
    Internal.colorMatrix [] (HueRotate angle)


{-| Make the colors in the input more intense
-}
saturate : Float -> FilterOperation msg -> FilterOperation msg
saturate factor =
    Internal.colorMatrix [] (Saturate factor)


invert =
    colorMatrix []


{-| -}
greyscale =
    -- taken from https://medium.com/square-corner-blog/welcome-to-the-color-matrix-64d112e3f43d#.splu7ntmr
    let
        matrix =
            [ [ 0.2126, 0.7152, 0.0722, 0, 0 ]
            , [ 0.2126, 0.7152, 0.0722, 0, 0 ]
            , [ 0.2126, 0.7152, 0.0722, 0, 0 ]
            , [ 0, 0, 0, 0, 1 ]
            ]
    in
        colorMatrix matrix


monochrome =
    -- https://css-tricks.com/color-filters-can-turn-your-gray-skies-blue/#monochrome-colorizing
    let
        matrix =
            [ [ 1.0, 0, 0, 0, 0 ]
            , [ 0.8, 0, 0, 0, 0 ]
            , [ 0.65, 0, 0, 0, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


sepia =
    let
        matrix =
            [ [ 0.393, 0.769, 0.189, 0, 0 ]
            , [ 0.349, 0.686, 0.168, 0, 0 ]
            , [ 0.272, 0.534, 0.131, 0, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


nightvision =
    let
        matrix =
            [ [ 0.1, 0.4, 0, 0, 0 ], [ 0.3, 1, 0.3, 0, 0 ], [ 0, 0.4, 0.1, 0, 0 ], [ 0, 0, 0, 1, 0 ] ]
    in
        colorMatrix matrix


warm =
    let
        matrix =
            [ [ 1.06, 0, 0, 0, 0 ]
            , [ 0, 1.01, 0, 0, 0 ]
            , [ 0, 0, 0.93, 0, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


cool =
    let
        matrix =
            [ [ 0.99, 0, 0, 0, 0 ]
            , [ 0, 0.93, 0, 0, 0 ]
            , [ 0, 0, 1.08, 0, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


brightness v_ =
    let
        v =
            -- -100 is black, 0 is normal, 100 is white
            -- these numbers, does that work ? [-1,1] or [0, 1] might be better
            255 * (v_ / 100)

        matrix =
            [ [ 1, 0, 0, 0, v ]
            , [ 0, 1, 0, 0, v ]
            , [ 0, 0, 1, 0, v ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


exposure v_ =
    let
        v =
            Basics.max v_ 0

        matrix =
            [ [ v, 0, 0, 0, 0 ]
            , [ 0, v, 0, 0, 0 ]
            , [ 0, 0, v, 0, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


contrast v =
    let
        n =
            0.5 * (1 - v)

        matrix =
            [ [ v, 0, 0, 0, n ]
            , [ 0, v, 0, 0, n ]
            , [ 0, 0, v, 0, n ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


matrix =
    [ [ 1 + v, 0, 0, 0, 0 ]
    , [ 0, 1, 0, 0, 0 ]
    , [ 0, 0, 1 - v, 0, 0 ]
    , [ 0, 0, 0, 1, 0 ]
    ]
matrix =
    [ [ 1 + v, 0, 0, 0, 0 ]
    , [ 0, 1, 0, 0, 0 ]
    , [ 0, 0, 1 + v, 0, 0 ]
    , [ 0, 0, 0, 1, 0 ]
    ]


threshold =
    let
        lum =
            { r = 0.3086, g = 0.6094, b = 0.082 }

        color =
            { r = 255 * lum.r, g = 255 * lum.g, b = 255 * lum.b }

        matrix =
            [ [ color.r, color.g, color.b, 0, -255 * v ]
            , [ color.r, color.g, color.b, 0, -255 * v ]
            , [ color.r, color.g, color.b, 0, -255 * v ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix


protanomaly =
    let
        matrix =
            [ [ 0.817, 0.183, 0, 0, 0 ]
            , [ 0.333, 0.667, 0, 0, 0 ]
            , [ 0, 0.125, 0.875, 0, 0 ]
            , [ 0, 0, 0, 1, 0 ]
            ]
    in
        colorMatrix matrix
