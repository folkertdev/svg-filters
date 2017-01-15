module Sketchy exposing (..)

{-| Examples of svg filters shown in the MDN documentation
-}

import Html exposing (div)
import Html.Attributes exposing (id, href, rel, type_, class)
import Svg exposing (..)
import Svg.Attributes exposing (x, y, cx, cy, width, height, viewBox, r, fill, fontSize, preserveAspectRatio, fillOpacity, y1, y2, x1, x2, textAnchor)
import Svg.Filter as Filter exposing (..)


thicken radius =
    morphology [] { defaultMorphology | operator = Dilate, radius = radius }


thin radius =
    morphology [] { defaultMorphology | operator = Erode, radius = radius }


fill operation =
    composite [] { defaultComposite | operator = In } operation


subtract operation =
    composite [] { defaultComposite | operator = Out } operation


identityMatrix n =
    List.range 1 n
        |> List.map
            (\j ->
                List.range 1 n
                    |> List.map
                        (\i ->
                            if i == j then
                                1
                            else
                                0
                        )
            )


main =
    let
        colorB =
            flood [] { color = "#73DCFF", opacity = 0.75 }

        colorR =
            flood [] { color = "#9673FF", opacity = 0.75 }

        tex01 =
            turbulence [] { defaultTurbulence | baseFrequency = twice 0.05, type_ = FractalNoise, numOctaves = 3, seed = 0 }

        tex02 =
            let
                matrix =
                    List.concat
                        [ [ 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, -2.1, 1.1 ]
                        ]
            in
                colorMatrix [] (Matrix matrix) tex01

        tex03 =
            let
                matrix =
                    List.concat
                        [ [ 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, 0, 0 ]
                        , [ 0, 0, 0, -1.7, 1.8 ]
                        ]
            in
                colorMatrix [] (Matrix matrix) tex01

        fill_ =
            concat (primitive SourceAlpha)
                [ offset [] { dx = -3, dy = 4 }
                , flip (displacementMap [] { defaultDisplacementMap | scale = 17 }) tex01
                , fill tex03
                , fill colorB
                ]

        thickened =
            thicken (twice 2) (primitive SourceAlpha)

        outline =
            concat thickened
                [ flip subtract (primitive SourceAlpha)
                , flip (displacementMap [] { defaultDisplacementMap | scale = 7 }) tex01
                , composite [] { defaultComposite | operator = Arithmetic, k2 = -1, k3 = 1 } tex02
                ]

        extruded =
            convolveMatrix [] { defaultConvolveMatrix | kernelMatrix = identityMatrix 8, divisor = Just 1 } (primitive SourceAlpha)

        bevelOutline =
            concat extruded
                [ thicken (twice 2)
                , flip subtract extruded
                , flip (displacementMap [] { defaultDisplacementMap | scale = 7 }) tex01
                , (composite [] { defaultComposite | operator = Arithmetic, k2 = -1, k3 = 1 }) tex02
                , offset [] { dx = -7, dy = -7 }
                , flip (composite [] { defaultComposite | operator = Out }) thickened
                ]

        bevelFill =
            concat extruded
                [ offset [] { dx = -9, dy = -9 }
                , flip subtract thickened
                , flip (displacementMap [] { defaultDisplacementMap | scale = 17 }) tex01
                , fill colorR
                ]

        sketchy =
            [ bevelFill
            , bevelOutline
            , fill_
            , outline
            ]
                |> concurrent
                |> Filter.create [] { id = "filter", filterUnits = UserSpaceOnUse }
    in
        div
            []
            [ Html.node "style"
                []
                [ text """
body{
        height: 100%;
        background: radial-gradient(ellipse at center, rgba(0,0,0,0) 40%,rgba(140,114,93,0.7) 100%),#FFECDF url('https://www.smashingmagazine.com/wp-content/uploads/2015/05/paper.jpg');
        background-size: cover;
        background-attachment: fixed;
        padding: 0;
        margin: 0;
      }
::selection {
        background: #73DCFF;
      }

      ::-moz-selection {
        background: #73DCFF;
      }

      svg{
        display: block;
        position: relative;
        width: 550px;
        height: 200px;
        top: 50%;
        margin: 0 auto;
        overflow: hidden;
        background-size: cover;
      }

      .filtered, #filtered{
        filter: url(#filter);
        -webkit-filter: url(#filter);
        fill: #9673FF;
        color: #9673FF;
        font-family: 'Alfa Slab One', cursive;
        text-transform: uppercase;
        font-size: 90px;
      }
                """
                ]
            , Html.node "link" [ href "http://fonts.googleapis.com/css?family=Alfa+Slab+One", rel "stylesheet", type_ "text/css" ] []
            , Svg.svg [ width "550", height "200px" ]
                [ defs []
                    [ Filter.toElement sketchy ]
                , text_
                    [ x "20"
                    , y "140"
                    , id "filtered"
                    ]
                    [ text "Scratch!" ]
                ]
            ]
