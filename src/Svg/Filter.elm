module Filter exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes
import Html
import FNV
import String


empty =
    Html.Attributes.attribute "_" ""


twice x =
    ( x, x )


hash : a -> Int
hash =
    FNV.hashString << toString


offset =
    Svg.filter [ id "offset" ]
        [ Svg.feOffset [ Html.Attributes.attribute "in" "SourceGraphic", dx "60", dy "60" ] []
        ]


type InputImage
    = SourceGraphic
    | SourceAlpha
    | BackgroundImage
    | BackgroundAlpha
    | FillPaint
    | StrokePaint



-- | FilterPrimitiveReference String
-- special case of FilterPrimitiveReference
-- | FilterAsImage Filter


example =
    """
<feMorphology operator="dilate" radius="3" in="SourceAlpha" result="OUTLINE_10" />
<feComposite operator="out" in="OUTLINE_10" in2 = "SourceAlpha" result="OUTLINE_20" />
<feDisplacementMap scale="7" in="OUTLINE_20" in2="Texture_10" result="OUTLINE_30" />
<feComposite operator="arithmetic" k2="-1" k3="1" in="Texture_20" in2="OUTLINE_30" result="OUTLINE_40" />
"""


primitive =
    Primitive


type Filter msg
    = Blend (List (Svg.Attribute msg)) { mode : BlendMode } (Filter msg) (Filter msg)
    | ColorMatrix (List (Svg.Attribute msg)) ColorMatrixData (Filter msg)
    | ComponentTransfer (List (Svg.Attribute msg)) (List TransferFunction) (Filter msg)
    | Composite (List (Svg.Attribute msg)) { operator : CompositeOperator, k1 : Float, k2 : Float, k3 : Float, k4 : Float } (Filter msg) (Filter msg)
    | ConvolveMatrix
        (List (Svg.Attribute msg))
        { kernelMatrix : List (List Float)
        , divisor : Maybe Float
        , bias : Float
        , targetX : Maybe Float
        , targetY : Maybe Float
        , edgeMode : EdgeMode
        , kernelUnitLength : ( Float, Float )
        , preserveAlpha : Bool
        }
        (Filter msg)
    | DisplacementMap (List (Svg.Attribute msg)) { scale : Float, xChannelSelector : ChannelSelector, yChannelSelector : ChannelSelector } (Filter msg) (Filter msg)
    | Flood (List (Svg.Attribute msg)) { color : String, opacity : Float }
    | Morphology (List (Svg.Attribute msg)) { operator : MorphologyOperator, radius : ( Float, Float ) } (Filter msg)
    | GaussianBlur (List (Svg.Attribute msg)) { stdDeviation : Float, edgeMode : String } (Filter msg)
    | Turbulence (List (Svg.Attribute msg)) { baseFrequency : ( Float, Float ), numOctaves : Int, seed : Float, stitchTiles : Bool, type_ : NoiseType }
    | Primitive InputImage


defaultConvolveMatrix =
    { kernelMatrix = []
    , divisor = Nothing
    , bias = 0
    , targetX = Nothing
    , targetY = Nothing
    , edgeMode = Duplicate
    , kernelUnitLength = twice 1
    , preserveAlpha = False
    }


type EdgeMode
    = Duplicate
    | Wrap
    | Zeros


blend =
    Blend


defaultBlend =
    { blend = Normal }


type BlendMode
    = Normal
    | Multiply
    | Screen
    | Darken
    | Lighten


{-| Combination of the type and value fields of the feColorMatrix element.
-}
type ColorMatrixData
    = Matrix (List Float)
    | Saturate Float
    | HueRotate Float
    | LuminanceToAlpha


{-| Helpers for feComponentTransfer
-}
type TransferFunctionType
    = Identity
    | Table { tableValues : List Float }
    | Discrete { tableValues : List Float }
    | Linear { slope : Float, intercept : Float }
    | Gamma { amplitude : Float, exponent : Float, offset : Float }


type TransferFunction
    = TransferFunction ChannelSelector TransferFunctionType


funcR =
    TransferFunction R


funcG =
    TransferFunction G


funcB =
    TransferFunction B


funcA =
    TransferFunction A


{-| The <feTurbulence> SVG filter primitive creates an image using the Perlin turbulence function.
It allows the synthesis of artificial textures like clouds or marble.
The resulting image will fill the entire filter primitive subregion.

-}
turbulence =
    Turbulence


type NoiseType
    = FractalNoise
    | TurbulenceNoise


defaultTurbulence =
    { baseFrequency = ( 0, 0 ), numOctaves = 1, seed = 0, stitchTiles = False, type_ = TurbulenceNoise }


handlePrimitive : Filter msg -> ( String, List (Svg msg) )
handlePrimitive filter =
    case filter of
        Primitive inputImage ->
            ( toString inputImage, [] )

        _ ->
            ( "elm-svg-filter_" ++ toString (hash filter), interpret filter )


resultAttribute filter =
    "elm-svg-filter_"
        ++ toString (hash filter)
        |> Svg.Attributes.result


combinator subfilters function extraAttributes specificAttributes =
    subfilters ++ [ function (extraAttributes ++ specificAttributes) [] ]


interpret : Filter msg -> List (Svg msg)
interpret filter =
    case filter of
        ColorMatrix attributes typevalue in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1

                typevalueAttrs =
                    case typevalue of
                        Matrix values ->
                            [ Svg.Attributes.type_ "matrix"
                            , Svg.Attributes.values <| String.join ", " (List.map toString values)
                            ]

                        Saturate factor ->
                            [ Svg.Attributes.type_ "saturate"
                            , Svg.Attributes.values <| toString factor
                            ]

                        HueRotate angle ->
                            [ Svg.Attributes.type_ "hueRotate"
                            , Svg.Attributes.values <| toString angle
                            ]

                        LuminanceToAlpha ->
                            [ Svg.Attributes.type_ "luminanceToAlpha" ]
            in
                combinator subfilters1 feColorMatrix (attributes ++ typevalueAttrs) <|
                    [ Svg.Attributes.in_ reference1
                    , resultAttribute filter
                    ]

        ComponentTransfer attributes transfers in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1

                transferFunctionToElement transferfunc =
                    case transferfunc of
                        TransferFunction R type_ ->
                            feFuncR (transferFunctionTypeToAttributes type_) []

                        TransferFunction G type_ ->
                            feFuncG (transferFunctionTypeToAttributes type_) []

                        TransferFunction B type_ ->
                            feFuncB (transferFunctionTypeToAttributes type_) []

                        TransferFunction A type_ ->
                            feFuncA (transferFunctionTypeToAttributes type_) []

                transferFunctionTypeToAttributes transferfunc =
                    case transferfunc of
                        Identity ->
                            [ Svg.Attributes.type_ "identity" ]

                        Table { tableValues } ->
                            [ Svg.Attributes.type_ "table"
                            , Html.Attributes.attribute "tableValues" <| String.join ", " <| List.map toString tableValues
                            ]

                        Discrete { tableValues } ->
                            [ Svg.Attributes.type_ "discrete"
                            , Html.Attributes.attribute "tableValues" <| String.join ", " <| List.map toString tableValues
                            ]

                        Linear { slope, intercept } ->
                            [ Svg.Attributes.type_ "linear"
                            , Svg.Attributes.slope (toString slope)
                            , Svg.Attributes.intercept (toString intercept)
                            ]

                        Gamma { amplitude, exponent, offset } ->
                            [ Svg.Attributes.type_ "gamma"
                            , Svg.Attributes.amplitude (toString amplitude)
                            , Svg.Attributes.exponent (toString exponent)
                            , Svg.Attributes.offset (toString offset)
                            ]
            in
                subfilters1
                    ++ [ feComponentTransfer
                            (attributes ++ [ Svg.Attributes.in_ reference1, resultAttribute filter ])
                            (List.map transferFunctionToElement transfers)
                       ]

        Composite attributes { operator, k1, k2, k3, k4 } in1 in2 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1

                ( reference2, subfilters2 ) =
                    handlePrimitive in2
            in
                combinator (subfilters1 ++ subfilters2) feComposite attributes <|
                    [ Svg.Attributes.in_ reference1
                    , Svg.Attributes.in2 reference2
                    , Svg.Attributes.operator (String.toLower <| toString operator)
                    , Svg.Attributes.k1 (toString k1)
                    , Svg.Attributes.k2 (toString k2)
                    , Svg.Attributes.k3 (toString k3)
                    , Svg.Attributes.k4 (toString k4)
                    , resultAttribute filter
                    ]

        ConvolveMatrix attributes settings in1 ->
            let
                order =
                    let
                        ( orderX, orderY ) =
                            ( List.length settings.kernelMatrix, Maybe.withDefault 3 (Maybe.map (List.length) (List.head settings.kernelMatrix)) )
                    in
                        Svg.Attributes.order <| toString orderX ++ " " ++ toString orderY

                kernelMatrix =
                    settings.kernelMatrix
                        |> List.concat
                        |> List.map toString
                        |> String.join " "
                        |> Svg.Attributes.kernelMatrix

                divisor =
                    Maybe.map (\v -> Svg.Attributes.divisor (toString v)) settings.divisor
                        |> Maybe.withDefault empty

                bias =
                    toString settings.bias
                        |> Svg.Attributes.bias

                targetX =
                    Maybe.map (\v -> Svg.Attributes.targetX (toString v)) settings.targetX
                        |> Maybe.withDefault empty

                targetY =
                    Maybe.map (\v -> Svg.Attributes.targetY (toString v)) settings.targetY
                        |> Maybe.withDefault empty

                edgeMode =
                    Svg.Attributes.edgeMode <|
                        case settings.edgeMode of
                            Duplicate ->
                                "duplicate"

                            Wrap ->
                                "wrap"

                            Zeros ->
                                "none"

                kernelUnitLength =
                    (toString (Tuple.first settings.kernelUnitLength) ++ " " ++ toString (Tuple.second settings.kernelUnitLength))
                        |> Svg.Attributes.kernelUnitLength

                preserveAlpha =
                    toString settings.preserveAlpha
                        |> String.toLower
                        |> Svg.Attributes.preserveAlpha

                ( reference1, subfilters1 ) =
                    handlePrimitive in1
            in
                combinator subfilters1 feConvolveMatrix attributes <|
                    [ order
                    , kernelMatrix
                    , divisor
                    , bias
                    , targetX
                    , targetY
                    , edgeMode
                    , kernelUnitLength
                    , preserveAlpha
                    , Svg.Attributes.in_ reference1
                    , resultAttribute filter
                    ]

        DisplacementMap attributes { scale, xChannelSelector, yChannelSelector } in1 in2 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1

                ( reference2, subfilters2 ) =
                    handlePrimitive in2
            in
                combinator (subfilters1 ++ subfilters2) feDisplacementMap attributes <|
                    [ Svg.Attributes.in_ reference1
                    , Svg.Attributes.in2 reference2
                    , Svg.Attributes.scale (toString scale)
                    , Svg.Attributes.xChannelSelector (toString xChannelSelector)
                    , Svg.Attributes.yChannelSelector (toString yChannelSelector)
                    , resultAttribute filter
                    ]

        Morphology attributes { operator, radius } in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1

                ( rx, ry ) =
                    radius
            in
                combinator subfilters1 feMorphology attributes <|
                    [ Svg.Attributes.in_ reference1
                    , Svg.Attributes.operator (String.toLower <| toString operator)
                    , Svg.Attributes.radius (toString rx ++ " " ++ toString ry)
                    , resultAttribute filter
                    ]

        GaussianBlur attributes { stdDeviation, edgeMode } in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1
            in
                combinator subfilters1 feGaussianBlur attributes <|
                    [ Svg.Attributes.in_ reference1
                    , Svg.Attributes.stdDeviation (toString stdDeviation)
                    , Svg.Attributes.edgeMode edgeMode
                    , resultAttribute filter
                    ]

        Turbulence attributes { baseFrequency, numOctaves, seed, stitchTiles, type_ } ->
            let
                ( fx, fy ) =
                    baseFrequency
            in
                combinator [] feTurbulence attributes <|
                    [ Svg.Attributes.baseFrequency (toString fx ++ " " ++ toString fy)
                    , Svg.Attributes.numOctaves (toString numOctaves)
                    , Svg.Attributes.seed (toString seed)
                    , Svg.Attributes.stitchTiles
                        (if stitchTiles then
                            "stitch"
                         else
                            "noStitch"
                        )
                    , Svg.Attributes.type_
                        (case type_ of
                            FractalNoise ->
                                "fractalNoise"

                            TurbulenceNoise ->
                                "turbulence"
                        )
                    , resultAttribute filter
                    ]

        Flood attributes { color, opacity } ->
            combinator [] feFlood attributes <|
                [ Svg.Attributes.floodColor color
                , Svg.Attributes.floodOpacity <| toString opacity
                , resultAttribute filter
                ]

        Blend attributes { mode } in1 in2 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1

                ( reference2, subfilters2 ) =
                    handlePrimitive in2
            in
                combinator (subfilters1 ++ subfilters2) feBlend attributes <|
                    [ Svg.Attributes.in_ reference1
                    , Svg.Attributes.in2 reference2
                    , Svg.Attributes.mode (String.toLower <| toString mode)
                    , resultAttribute filter
                    ]

        Primitive image ->
            []


singleton x =
    [ x ]


type ChannelSelector
    = R
    | G
    | B
    | A


type MorphologyOperator
    = Erode
    | Dilate


type CompositeOperator
    = Over
    | In
    | Out
    | Atop
    | Xor
    | Arithmetic


morphology =
    Morphology


composite =
    Composite


displacementMap =
    DisplacementMap


{-| The [<feGaussianBlur>](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feGaussianBlur)
SVG filter primitive blurs the input image by the amount specified in stdDeviation, which defines the bell-curve.
-}
gaussianBlur =
    GaussianBlur


defaultComposite =
    { operator = Over, k1 = 0, k2 = 0, k3 = 0, k4 = 0 }


defaultDisplacementMap =
    { scale = 0, xChannelSelector = A, yChannelSelector = A }


dsl =
    let
        morphed =
            morphology [] { operator = Dilate, radius = ( 3, 3 ) } (primitive SourceAlpha)

        composited =
            composite [] { defaultComposite | operator = Out } morphed (primitive SourceAlpha)

        displaced =
            displacementMap [] { defaultDisplacementMap | scale = 7 } morphed composited
    in
        composite [] { defaultComposite | operator = Arithmetic, k2 = -1, k3 = 1 } composited displaced


simple =
    GaussianBlur [] { stdDeviation = 5, edgeMode = "duplicate" } (primitive SourceGraphic)


tooSimple =
    feGaussianBlur [ in_ "SourceGraphic", stdDeviation "3" ] []


main =
    -- morphologyExample
    -- blendExample
    -- floodExample
    -- colorMatrixExample
    -- componentTransferExample
    convolveMatrixExample


gausianBlurExample =
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


type FilterUnit
    = UserSpaceOnUse
    | ObjectBoundingBox


declareFilter : List (Attribute msg) -> { id : String, filterUnits : FilterUnit } -> Filter msg -> Svg msg
declareFilter attributes { id, filterUnits } filter =
    let
        unit =
            case filterUnits of
                UserSpaceOnUse ->
                    "userSpaceOnUse"

                ObjectBoundingBox ->
                    "objectBoundingBox"
    in
        Svg.filter (attributes ++ [ Svg.Attributes.id id, Svg.Attributes.filterUnits unit ]) (interpret filter)


floodExample =
    let
        floodFilter =
            Flood [ x "50", y "50", width "100", height "100" ] { color = "green", opacity = 0.5 }
    in
        Svg.svg [ width "200", height "200" ]
            [ declareFilter [] { id = "floodFilter", filterUnits = UserSpaceOnUse } floodFilter
            , Svg.use [ Svg.Attributes.filter "url(#floodFilter)" ] []
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
    in
        Svg.svg [ width "100%", height "100%", preserveAspectRatio "xMidYMid meet" ]
            [ declareFilter [] { id = "colorMeMatrix", filterUnits = ObjectBoundingBox } colorMeMatrix
            , g [ Svg.Attributes.filter "url(#colorMeMatrix)" ]
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
    in
        Svg.svg [ viewBox "0 0 600 300" ]
            [ rainbow
            , declareFilter [] { id = "table", filterUnits = ObjectBoundingBox } tableTransfer
            , rect [ x "0", y "8%", width "100%", height "20", Html.Attributes.style [ ( "fill", "url(#rainbow)" ) ] ] []
            , rect [ x "0", y "29%", width "100%", height "20", Html.Attributes.style [ ( "fill", "url(#rainbow)" ), ( "filter", "url(#table)" ) ] ] []
            ]


convolveMatrixExample =
    let
        emboss =
            ConvolveMatrix [] { defaultConvolveMatrix | kernelMatrix = [ [ 3, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, -3 ] ] } (primitive SourceGraphic)
    in
        Svg.svg [ viewBox "0 0 200 200" ]
            [ declareFilter [] { id = "emboss", filterUnits = ObjectBoundingBox } emboss
            , image
                [ Html.Attributes.attribute "href" "https://developer.mozilla.org/files/12668/MDN.svg"
                , width "200"
                , height "200"
                , x "0"
                , y "0"
                , Html.Attributes.style [ ( "filter", "url(#emboss)" ) ]
                ]
                []
            ]
