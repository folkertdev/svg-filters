module NiceApi exposing (..)

{-|

This module includes the most commonly-used SVG filter functionality in a nice wrapper.

If you need more fine-grained control, use the primitives in the [Internal] module.

# Combining Filters
@docs sequential, concurrent

# Blend
@docs normal, multiply, screen, darken, lighten

# Morphology
@docs thicken, shrink

# Composite
@docs arithmetic, over, in_, atop, xor, and, lighter

# Color Matrix
@docs colorMatrix

@docs displacement, gaussianBlur
-}

import Svg.Operations as Internal exposing (..)


{-| Spatially displace the foreground (second argument) with the color values of the background (third argument)
-}
displacement : Float -> FilterOperation msg -> FilterOperation msg
displacement scale =
    Internal.displacementMap [] { defaultDisplacementMap | scale = scale }


{-| Creates a solid layer of a color with an opacity
-}
flood : { color : String, opacity : Float } -> FilterOperation msg -> FilterOperation msg
flood options =
    Internal.flood [] options


{-| Use the input filter as tiles
-}
tile : FilterOperation msg -> FilterOperation msg
tile =
    Internal.tile []


{-| gaussian blur of the given amount (standard deviation)
-}
gaussianBlur : Float -> FilterOperation msg -> FilterOperation msg
gaussianBlur stdDeviation =
    Internal.gaussianBlur [] { defaultGaussianBlur | stdDeviation = stdDeviation }


{-| -}
colorMatrix : List (List Float) -> FilterOperation msg -> FilterOperation msg
colorMatrix matrix =
    Internal.colorMatrix [] (Matrix <| List.concat matrix)



-- tile, image, turbulence, gaussianBlur
--- Blend


{-| Normal blend mode only uses the foreground pixels, discarding the background.
-}
normal : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
normal =
    blend [] { mode = Normal }


{-| Multiply blend mode multiplies the numbers for each pixel of the top layer with the corresponding pixel for the bottom layer. The result is a darker picture.
-}
multiply : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
multiply =
    blend [] { mode = Multiply }


{-| With Screen blend mode the values of the pixels in the two layers are inverted, multiplied, and then inverted again.
This yields the opposite effect to multiply. The result is a brighter picture.
-}
screen : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
screen =
    blend [] { mode = Screen }


{-| Selects the mininum of each component from the foreground and background pixels
-}
darken : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
darken =
    blend [] { mode = Darken }


{-| Selects the maximum of each component from the foreground and background pixels
-}
lighten : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
lighten =
    blend [] { mode = Lighten }



--- Morphology


{-| Thickens or fattens (dilates) the result of a filter operation in the x and y direction
-}
thicken : Float -> Float -> FilterOperation msg -> FilterOperation msg
thicken x y =
    morphology [] { operator = Dilate, radius = ( x, y ) }


{-| Shrinks or thins (erodes) the result of a filter in the x and y direction
-}
shrink : Float -> Float -> FilterOperation msg -> FilterOperation msg
shrink x y =
    morphology [] { operator = Erode, radius = ( x, y ) }



--- Composite


{-| Apply an arithmetic operation to the two input images

    result = k1*i1*i2 + k2*i1 + k3*i2 + k4
-}
arithmetic : { k1 : Float, k2 : Float, k3 : Float, k4 : Float } -> FilterOperation msg -> FilterOperation msg -> FilterOperation msg
arithmetic { k1, k2, k3, k4 } =
    composite [] { defaultComposite | operator = Arithmetic, k1 = k1, k2 = k2, k3 = k3, k4 = k4 }


{-| -}
over : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
over =
    composite [] { defaultComposite | operator = Over }


{-| -}
in_ : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
in_ =
    composite [] { defaultComposite | operator = In }


{-| -}
atop : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
atop =
    composite [] { defaultComposite | operator = Atop }


{-| -}
out : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
out =
    composite [] { defaultComposite | operator = Out }


{-| -}
xor : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
xor =
    composite [] { defaultComposite | operator = Xor }


{-| -}
and : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
and =
    composite [] { defaultComposite | operator = And }


{-| -}
lighter : FilterOperation msg -> FilterOperation msg -> FilterOperation msg
lighter =
    composite [] { defaultComposite | operator = Lighter }



--- Noise
