module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (cx, cy, r, fill, x, y)
import Svg.Operations exposing (..)
import Svg.Filter as Filter exposing (..)


goo =
    let
        matrix =
            [ [ 1, 0, 0, 0, 0 ]
            , [ 0, 1, 0, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            , [ 0, 0, 0, 19, -4 ]
              -- adjust the -4 for more/less gooeyness
            ]
    in
        sourceGraphic
            |> gaussianBlur [] { defaultGaussianBlur | stdDeviation = 10 }
            |> colorMatrix [] (Matrix (List.concat matrix))
            |> composite [] { defaultComposite | operator = Atop } sourceGraphic
goo =
    let
        contrastAlpha =
            [ [ 1, 0, 0, 0, 0 ]
            , [ 0, 1, 0, 0, 0 ]
            , [ 0, 0, 1, 0, 0 ]
            , [ 0, 0, 0, 18, -7 ]
            ]
    in
        sourceGraphic
            |> gaussianBlur [] { defaultGaussianBlur | stdDeviation = 10 }
            |> colorMatrix [] (Matrix (List.concat contrastAlpha))
            |> composite [] { defaultComposite | operator = Atop } sourceGraphic


shadowedGoo =
    let
        shadow =
            List.concat
                [ [ 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 1 -0.2 ]
                ]
    in
        goo
            |> gaussianBlur [] { defaultGaussianBlur | stdDeviation = 3 }
            |> colorMatrix [] (Matrix shadow)
            |> offset [] { defaultOffset | dx = 1, dy = 1 }
            |> composite [] { defaultComposite | operator = Atop } goo
            |> composite [] { defaultComposite | operator = Atop } sourceGraphic


main =
    div []
        [ Svg.svg [ Svg.Attributes.width "800", Svg.Attributes.height "300" ]
            [ defs []
                [ goo
                    |> Filter.create [] { id = "goo", filterUnits = UserSpaceOnUse }
                    |> Filter.toElement
                , shadowedGoo
                    |> Filter.create [] { id = "shadowedGoo", filterUnits = UserSpaceOnUse }
                    |> Filter.toElement
                ]
            , circle [ cx "100", cy "100", r "50", fill "#f49393", Filter.useFilter goo ] []
            , rect [ x "140", y "100", Svg.Attributes.width "30", Svg.Attributes.height "30", fill "#f49393", Filter.useFilter goo ] []
            ]
        ]
