module MdnExamples exposing (..)

{-| Examples of svg filters shown in the MDN documentation
-}

import Html exposing (div)
import Html.Attributes exposing (id)
import Svg exposing (..)
import Svg.Attributes exposing (x, y, cx, cy, width, height, viewBox, r, fill, fontSize, preserveAspectRatio, fillOpacity, y1, y2, x1, x2, textAnchor)
import Svg.Filter as Filter exposing (..)


main =
    div []
        [ text "blend"
        , blendExample
        , text "colorMatrix"
        , colorMatrixExample
        , text "componentTransfer"
        , componentTransferExample
          -- composite
        , text "convolveMatrix"
        , convolveMatrixExample
        , text "diffuseLighting"
        , diffuseLightingExample
        , text "displacementMap"
        , displacementMapExample
        , text "flood"
        , floodExample
          -- funcA, funcB, funcG, funcR
        , text "gaussianBlur"
        , gaussianBlurExample
        , text "image"
          --, imageExample
          -- merge, mergeNode
        , text "morphology"
        , morphologyExample
        , text "offset"
          --, offsetExample
        , text "specularLighting"
        , specularLightingExample
        , text "tile"
        , text "turbulence"
        , turbulenceExample
        ]


turbulenceExample =
    displacementMapExample


gaussianBlurExample =
    Svg.svg
        [ width "512", height "120", viewBox "0 0 120 120" ]
        [ Svg.filter [ id "blurMe" ] (interpret simple)
        , circle [ cx "60", cy "60", r "50", fill "green" ] []
        , circle [ cx "170", cy "60", r "50", fill "green", Svg.Attributes.filter "url(#blurMe)" ] []
        ]


morphologyExample =
    Svg.svg [ width "300", height "180" ]
        [ Svg.filter [ id "erode" ] (interpret <| morphology [] { operator = Erode, radius = ( 1, 1 ) } (primitive SourceGraphic))
        , Svg.filter [ id "dilate" ] (interpret <| morphology [] { operator = Dilate, radius = ( 2, 2 ) } (primitive SourceGraphic))
        , text_ [ fontSize "3em", y "1em" ] [ text "Normal Text" ]
        , text_ [ fontSize "3em", y "2em", Svg.Attributes.filter "url(#erode)" ] [ text "Thinned Text" ]
        , text_ [ fontSize "3em", y "3em", Svg.Attributes.filter "url(#dilate)" ] [ text "Fattened Text" ]
        ]


displacementMapExample =
    let
        turb =
            turbulence [] { defaultTurbulence | type_ = TurbulenceNoise, baseFrequency = twice 0.05, numOctaves = 2 }
    in
        Svg.svg [ width "200", height "200", viewBox "0 0 220 220" ]
            [ displacementMap [] { scale = 50, xChannelSelector = R, yChannelSelector = G } (primitive SourceGraphic) turb
                |> interpret
                |> Svg.filter [ id "displacementFilter" ]
            , circle [ cx "100", cy "100", r "100", Svg.Attributes.filter "url(#displacementFilter)" ] []
            ]


{-|
    This only works in chrome with href, not with xlink:href like in the MDN example

-}
blendExample =
    let
        floodFill =
            Flood [ x "0", y "0", width "100%", height "100%" ] { color = "green", opacity = 1 }

        spotLight =
            blend [] { mode = Multiply } (primitive SourceGraphic) floodFill
    in
        Svg.svg [ width "200", height "200", viewBox "0 0 220 220" ]
            [ Svg.filter [ id "spotlight" ] (interpret spotLight)
            , image
                [ Html.Attributes.attribute "href" "https://developer.mozilla.org/files/6457/mdn_logo_only_color.png"
                , x "10%"
                , y "10%"
                , width "80%"
                , height "80%"
                , Html.Attributes.style [ ( "filter", "url(#spotlight)" ) ]
                ]
                []
            ]


floodExample =
    let
        floodFilter =
            Flood [ x "50", y "50", width "100", height "100" ] { color = "green", opacity = 0.5 }
                |> Filter.create [] { id = "floodFilter", filterUnits = UserSpaceOnUse }
    in
        Svg.svg [ width "200", height "200" ]
            [ Filter.toElement floodFilter
            , Svg.use [ Filter.useFilter floodFilter ] []
            ]


colorMatrixExample =
    let
        matrix1 =
            List.concat
                [ [ 0, 0, 0, 0, 0 ]
                , [ 1, 1, 1, 1, 0 ]
                , [ 0, 0, 0, 0, 0 ]
                , [ 0, 0, 0, 1, 0 ]
                ]

        colorMeMatrix =
            ColorMatrix [] (Matrix matrix1) (primitive SourceGraphic)
                |> Filter.create [] { id = "colorMeMatrix", filterUnits = ObjectBoundingBox }
    in
        Svg.svg [ width "100%", height "100%", preserveAspectRatio "xMidYMid meet" ]
            [ Filter.toElement colorMeMatrix
            , g [ Filter.useFilter colorMeMatrix ]
                [ circle [ cx "30", cy "30", r "20", fill "blue", fillOpacity "0.5" ] []
                , circle [ cx "20", cy "50", r "20", fill "green", fillOpacity "0.5" ] []
                , circle [ cx "40", cy "50", r "20", fill "red", fillOpacity "0.5" ] []
                ]
            ]


type alias Stop =
    { offset : Float, stopColor : String }


rainbow =
    linearGradient [ x1 "0", y1 "0", x2 "100%", y2 "0" ] { id = "rainbow", units = UserSpaceOnUse } <|
        [ Stop 0.0 "#ff0000"
        , Stop 0.2 "#ffff00"
        , Stop 0.4 "#00ff00"
        , Stop 0.6 "#00ffff"
        , Stop 0.8 "#0000ff"
        , Stop 1.0 "#800080"
        ]


linearGradient attributes { id, units } stops =
    let
        units_ =
            case units of
                UserSpaceOnUse ->
                    "userSpaceOnUse"

                ObjectBoundingBox ->
                    "objectBoundingBox"

        stopToElement { offset, stopColor } =
            Svg.stop [ Svg.Attributes.offset (toString offset), Svg.Attributes.stopColor stopColor ] []
    in
        Svg.linearGradient (attributes ++ [ Svg.Attributes.id id, Svg.Attributes.gradientUnits units_ ]) (List.map stopToElement stops)


componentTransferExample =
    let
        transfers =
            [ funcR (Table { tableValues = [ 0, 0, 1, 1 ] })
            , funcG (Table { tableValues = [ 1, 1, 0, 0 ] })
            , funcB (Table { tableValues = [ 0, 1, 1, 0 ] })
            ]

        tableTransfer =
            ComponentTransfer [] transfers (primitive SourceGraphic)
                |> Filter.create [] { id = "table", filterUnits = ObjectBoundingBox }
    in
        Svg.svg [ viewBox "0 0 600 300" ]
            [ rainbow
            , Filter.toElement tableTransfer
            , rect [ x "0", y "8%", width "100%", height "20", Html.Attributes.style [ ( "fill", "url(#rainbow)" ) ] ] []
            , rect [ x "0", y "29%", width "100%", height "20", Html.Attributes.style [ ( "fill", "url(#rainbow)" ) ], Filter.useFilter tableTransfer ] []
            ]


convolveMatrixExample =
    let
        emboss =
            ConvolveMatrix [] { defaultConvolveMatrix | kernelMatrix = [ [ 3, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, -3 ] ] } (primitive SourceGraphic)
                |> Filter.create [] { id = "emboss", filterUnits = ObjectBoundingBox }
    in
        Svg.svg [ width "200", height "200", viewBox "0 0 200 200" ]
            [ Filter.toElement emboss
            , image
                [ Html.Attributes.attribute "href" "https://developer.mozilla.org/files/12668/MDN.svg"
                , width "200"
                , height "200"
                , x "0"
                , y "0"
                , Filter.useFilter emboss
                ]
                []
            ]


diffuseLightingExample =
    let
        lightMe1 =
            DiffuseLighting []
                defaultDiffuseLighting
                [ PointLight { x = 150, y = 60, z = 20 }
                ]
                (primitive SourceGraphic)
                |> Filter.create [] { id = "lightMe1", filterUnits = ObjectBoundingBox }
    in
        Svg.svg [ width "440", height "140" ]
            [ text_ [ textAnchor "middle", x "60", y "22" ] [ text "No Light" ]
            , circle [ cx "60", cy "80", r "50", fill "green" ] []
            , text_ [ textAnchor "middle", x "170", y "22" ] [ text "fePointLight" ]
            , Filter.toElement lightMe1
            , circle [ cx "170", cy "80", r "50", fill "green", Filter.useFilter lightMe1 ] []
            ]


specularLightingExample =
    let
        filter =
            Filter.concat (primitive SourceGraphic)
                [ SpecularLighting [] { defaultSpecularLighting | specularExponent = 20, lightingColor = "#bbbbbb" } [ PointLight { x = 50, y = 75, z = 200 } ]
                , Composite [] { defaultComposite | operator = Arithmetic, k1 = 0, k2 = 1, k3 = 1, k4 = 0 } (primitive SourceGraphic)
                ]
                |> Filter.create [] { id = "filter", filterUnits = ObjectBoundingBox }
    in
        Svg.svg [ width "200", height "200", viewBox "0 0 220 220" ]
            [ Filter.toElement filter
            , circle [ cx "110", cy "110", r "100", Filter.useFilter filter ] []
            ]
