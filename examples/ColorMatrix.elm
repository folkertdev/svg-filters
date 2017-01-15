module ColorMatrix exposing (..)

{-| Create a dropshadow around the input filter operation
-}


dropShadow : Float -> Float -> Float -> FilterOperation msg -> FilterOperation msg
dropShadow stdDeviation offsetX offsetY input =
    [ gaussianBlur [] { defaultGaussianBlur | stdDeviation = stdDeviation } sourceAlpha
        |> offset [] { defaultOffset | dx = offsetX, dy = offsetY }
    , input
    ]
        |> concurrent


raised =
    sourceAlpha
        |> specularLighting []
            { defaultSpecularLighting | specularConstant = 1.2, specularExponent = 12, color = "#bbbbbb" }
            [ distantLight { aziumth = 45, elevation = 45 } ]
        |> composite [] { defaultComposite | operator = Arithmetic, k2 = 1, k3 = 1 } sourceGraphic


{-| -}
greyscale =
    -- taken from https://medium.com/square-corner-blog/welcome-to-the-color-matrix-64d112e3f43d#.splu7ntmr
    let
        matrix =
            List.concat
                [ [ 0.2126, 0.7152, 0.0722, 0, 0 ]
                , [ 0.2126, 0.7152, 0.0722, 0, 0 ]
                , [ 0.2126, 0.7152, 0.0722, 0, 0 ]
                , [ 0, 0, 0, 0, 1 ]
                ]
    in
        colorMatrix [] (Matrix matrix)


monochrome =
    -- https://css-tricks.com/color-filters-can-turn-your-gray-skies-blue/#monochrome-colorizing
    let
        matrix =
            List.concat
                [ [ 1.0, 0, 0, 0, 0 ]
                , [ 0.8, 0, 0, 0, 0 ]
                , [ 0.65, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 1, 0 ]
                ]
    in
        colorMatrix [] (Matrix matrix)


sepia =
    let
        matrix =
            List.concat
                [ [ 0.393, 0.769, 0.189, 0, 0 ]
                , [ 0.349, 0.686, 0.168, 0, 0 ]
                , [ 0.272, 0.534, 0.131, 0, 0 ]
                , [ 0, 0, 0, 1, 0 ]
                ]
    in
        colorMatrix [] (Matrix matrix)
