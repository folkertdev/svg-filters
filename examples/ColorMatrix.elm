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
