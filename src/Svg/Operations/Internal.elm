module Svg.Operations.Internal
    exposing
        ( FilterOperation
        , blend
        , defaultBlend
        , BlendMode(..)
        , colorMatrix
        , ColorMatrixData(..)
        , composite
        , CompositeOperator(..)
        , defaultComposite
        , convolveMatrix
        , defaultConvolveMatrix
        , displacementMap
        , defaultDisplacementMap
        , flood
        , defaultFlood
        , gaussianBlur
        , defaultGaussianBlur
        , image
        , defaultImage
        , morphology
        , MorphologyOperator(..)
        , defaultMorphology
        , offset
        , defaultOffset
        , tile
        , turbulence
        , defaultTurbulence
        , componentTransfer
        , ComponentTransferData
        , defaultComponentTransfer
        , diffuseLighting
        , defaultDiffuseLighting
        , specularLighting
        , defaultSpecularLighting
        , pointLight
        , spotLight
        , distantLight
        , PrimitiveImage
        , sourceGraphic
        , sourceAlpha
        , backgroundImage
        , backgroundAlpha
        , fillPaint
        , strokePaint
        , twice
        , TransferFunction(..)
        , concat
        , sequential
        , concurrent
        , render
        )

{-| A wrapper around SVG's primitive filter operations


# Types

@docs FilterOperation

# Helpers

@docs twice

@docs render

# Image primitives

@docs PrimitiveImage
@docs sourceGraphic , sourceAlpha , backgroundImage , backgroundAlpha , fillPaint , strokePaint


# Operations

## Blend
@docs blend, BlendMode, defaultBlend
## Color Matrix
@docs colorMatrix, ColorMatrixData
@docs composite,defaultComposite, CompositeOperator
@docs convolveMatrix,defaultConvolveMatrix
@docs displacementMap,defaultDisplacementMap
@docs flood,defaultFlood
@docs gaussianBlur,defaultGaussianBlur
@docs image, defaultImage
@docs morphology, defaultMorphology, MorphologyOperator
@docs offset,defaultOffset
@docs tile
@docs turbulence,defaultTurbulence

# Combining Operations

@docs concat, sequential, concurrent

# Transfer
@docs componentTransfer,defaultComponentTransfer
@docs TransferFunction,ComponentTransferData

# Lighting

@docs diffuseLighting,defaultDiffuseLighting
@docs specularLighting,defaultSpecularLighting

@docs pointLight,spotLight, distantLight

-}

import Svg exposing (..)
import Svg.Attributes
import Html.Attributes
import FNV


empty =
    Html.Attributes.attribute "_" ""


hash : a -> Int
hash =
    FNV.hashString << toString


type alias ConvolveMatrixData =
    { kernelMatrix : List (List Float)
    , divisor : Maybe Float
    , bias : Float
    , targetX : Maybe Float
    , targetY : Maybe Float
    , edgeMode : EdgeMode
    , kernelUnitLength : ( Float, Float )
    , preserveAlpha : Bool
    }


type alias DiffuseLightingData =
    { surfaceScale : Float
    , diffuseConstant : Float
    , kernelUnitLength : ( Float, Float )
    , lightingColor : String
    }


type alias SpecularLightingData =
    { surfaceScale : Float
    , specularConstant : Float
    , specularExponent : Float
    , kernelUnitLength : ( Float, Float )
    , lightingColor : String
    }


{-| Type representing a primitive filter operation
-}
type FilterOperation msg
    = Blend (List (Svg.Attribute msg)) { mode : BlendMode } (FilterOperation msg) (FilterOperation msg)
    | ColorMatrix (List (Svg.Attribute msg)) ColorMatrixData (FilterOperation msg)
    | ComponentTransfer (List (Svg.Attribute msg)) ComponentTransferData (FilterOperation msg)
    | Composite (List (Svg.Attribute msg)) { operator : CompositeOperator, k1 : Float, k2 : Float, k3 : Float, k4 : Float } (FilterOperation msg) (FilterOperation msg)
    | ConvolveMatrix (List (Svg.Attribute msg)) ConvolveMatrixData (FilterOperation msg)
    | DiffuseLighting (List (Svg.Attribute msg)) DiffuseLightingData (List LightingType) (FilterOperation msg)
    | DisplacementMap (List (Svg.Attribute msg)) { scale : Float, xChannelSelector : ChannelSelector, yChannelSelector : ChannelSelector } (FilterOperation msg) (FilterOperation msg)
    | Flood (List (Svg.Attribute msg)) { color : String, opacity : Float }
    | GaussianBlur (List (Svg.Attribute msg)) { stdDeviation : Float, edgeMode : EdgeMode } (FilterOperation msg)
    | Morphology (List (Svg.Attribute msg)) { operator : MorphologyOperator, radius : ( Float, Float ) } (FilterOperation msg)
    | Image (List (Svg.Attribute msg)) { link : String, preserveAspectRatio : String }
    | Offset (List (Svg.Attribute msg)) { dx : Float, dy : Float } (FilterOperation msg)
    | SpecularLighting (List (Svg.Attribute msg)) SpecularLightingData (List LightingType) (FilterOperation msg)
    | Tile (FilterOperation msg)
    | Turbulence (List (Svg.Attribute msg)) TurbulenceData
    | Primitive PrimitiveImage
    | Concat (FilterOperation msg) (List (FilterOperation msg -> FilterOperation msg))
    | Sequential (List (FilterOperation msg))
    | Concurrent (List (FilterOperation msg))


{-| -}
type EdgeMode
    = Duplicate
    | Wrap
    | Zeros


{-| -}
type PrimitiveImage
    = SourceGraphic
    | SourceAlpha
    | BackgroundImage
    | BackgroundAlpha
    | FillPaint
    | StrokePaint


primitive : PrimitiveImage -> FilterOperation msg
primitive =
    Primitive


{-| -}
sourceGraphic : FilterOperation msg
sourceGraphic =
    primitive SourceGraphic


{-| -}
sourceAlpha : FilterOperation msg
sourceAlpha =
    primitive SourceAlpha


{-| -}
backgroundImage : FilterOperation msg
backgroundImage =
    primitive BackgroundImage


{-| -}
backgroundAlpha : FilterOperation msg
backgroundAlpha =
    primitive BackgroundAlpha


{-| -}
fillPaint : FilterOperation msg
fillPaint =
    primitive FillPaint


{-| -}
strokePaint : FilterOperation msg
strokePaint =
    primitive StrokePaint


type ChannelSelector
    = R
    | G
    | B
    | A


{-| Operators used by the morphology filter
-}
type MorphologyOperator
    = Erode
    | Dilate


{-| Operators used by the composite filter
-}
type CompositeOperator
    = Over
    | In
    | Out
    | Atop
    | Xor
    | Arithmetic


{-| The kind of noise
-}
type NoiseType
    = FractalNoise
    | TurbulenceNoise


type LightingType
    = PointLight { x : Float, y : Float, z : Float }
    | DistantLight { azimuth : Float, elevation : Float }
    | SpotLight { pointsAtX : Float, pointsAtY : Float, pointsAtZ : Float, specularExponent : Float, limitingConeAngle : Maybe Float }


{-| -}
pointLight : { x : Float, y : Float, z : Float } -> LightingType
pointLight =
    PointLight


{-| -}
distantLight : { azimuth : Float, elevation : Float } -> LightingType
distantLight =
    DistantLight


{-| -}
spotLight : { pointsAtX : Float, pointsAtY : Float, pointsAtZ : Float, specularExponent : Float, limitingConeAngle : Maybe Float } -> LightingType
spotLight =
    SpotLight



--- Blend


{-| -}
blend : List (Attribute msg) -> { mode : BlendMode } -> FilterOperation msg -> FilterOperation msg -> FilterOperation msg
blend =
    Blend


{-| -}
defaultBlend : { blend : BlendMode }
defaultBlend =
    { blend = Normal }


{-| Type of blending
-}
type BlendMode
    = Normal
    | Multiply
    | Screen
    | Darken
    | Lighten



--- ColorMatrix


{-| Color Matrix
-}
colorMatrix : List (Attribute msg) -> ColorMatrixData -> FilterOperation msg -> FilterOperation msg
colorMatrix =
    ColorMatrix


{-| Combination of the type and value fields of the feColorMatrix element.
-}
type ColorMatrixData
    = Matrix (List Float)
    | Saturate Float
    | HueRotate Float
    | LuminanceToAlpha



-- ComponentTransfer


{-| -}
componentTransfer : List (Attribute msg) -> ComponentTransferData -> FilterOperation msg -> FilterOperation msg
componentTransfer =
    ComponentTransfer


{-| Helpers for feComponentTransfer
-}
type TransferFunction
    = Identity
    | Table { tableValues : List Float }
    | Discrete { tableValues : List Float }
    | Linear { slope : Float, intercept : Float }
    | Gamma { amplitude : Float, exponent : Float, offset : Float }


{-| -}
type alias ComponentTransferData =
    { r : TransferFunction
    , g : TransferFunction
    , b : TransferFunction
    , a : TransferFunction
    }


{-| -}
defaultComponentTransfer : ComponentTransferData
defaultComponentTransfer =
    { r = Identity
    , g = Identity
    , b = Identity
    , a = Identity
    }



--- Composite


{-| -}
defaultComposite :
    { k1 : Float
    , k2 : Float
    , k3 : Float
    , k4 : Float
    , operator : CompositeOperator
    }
defaultComposite =
    { operator = Over, k1 = 0, k2 = 0, k3 = 0, k4 = 0 }


{-| -}
defaultDisplacementMap :
    { scale : Float
    , xChannelSelector : ChannelSelector
    , yChannelSelector : ChannelSelector
    }
defaultDisplacementMap =
    { scale = 0, xChannelSelector = A, yChannelSelector = A }


{-| -}
convolveMatrix :
    List (Attribute msg)
    -> ConvolveMatrixData
    -> FilterOperation msg
    -> FilterOperation msg
convolveMatrix =
    ConvolveMatrix


{-| -}
morphology :
    List (Attribute msg)
    -> { operator : MorphologyOperator, radius : ( Float, Float ) }
    -> FilterOperation msg
    -> FilterOperation msg
morphology =
    Morphology


{-| -}
defaultMorphology : { operator : MorphologyOperator, radius : ( Float, Float ) }
defaultMorphology =
    { operator = Erode, radius = twice 0 }


{-| -}
composite :
    List (Attribute msg)
    -> { k1 : Float
       , k2 : Float
       , k3 : Float
       , k4 : Float
       , operator : CompositeOperator
       }
    -> FilterOperation msg
    -> FilterOperation msg
    -> FilterOperation msg
composite =
    Composite


{-| -}
displacementMap :
    List (Attribute msg)
    -> DisplacementMapData
    -> FilterOperation msg
    -> FilterOperation msg
    -> FilterOperation msg
displacementMap =
    DisplacementMap


{-| -}
type alias DisplacementMapData =
    { scale : Float
    , xChannelSelector : ChannelSelector
    , yChannelSelector : ChannelSelector
    }


{-| The [<feGaussianBlur>](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feGaussianBlur)
SVG filter primitive blurs the input image by the amount specified in stdDeviation, which defines the bell-curve.
-}
gaussianBlur :
    List (Attribute msg)
    -> { edgeMode : EdgeMode, stdDeviation : Float }
    -> FilterOperation msg
    -> FilterOperation msg
gaussianBlur =
    GaussianBlur


{-| -}
defaultGaussianBlur : { edgeMode : EdgeMode, stdDeviation : number }
defaultGaussianBlur =
    { stdDeviation = 0, edgeMode = Duplicate }


{-| -}
flood : List (Attribute msg) -> { color : String, opacity : Float } -> FilterOperation msg
flood =
    Flood


{-| -}
defaultFlood : { color : String, opacity : Float }
defaultFlood =
    { color = "#FFF", opacity = 1 }


{-| The <feTurbulence> SVG filter primitive creates an image using the Perlin turbulence function.
It allows the synthesis of artificial textures like clouds or marble.
The resulting image will fill the entire filter primitive subregion.

-}
turbulence : List (Attribute msg) -> TurbulenceData -> FilterOperation msg
turbulence =
    Turbulence


{-| -}
type alias TurbulenceData =
    { baseFrequency : ( Float, Float ), numOctaves : Int, seed : Float, stitchTiles : Bool, type_ : NoiseType }


{-| -}
defaultTurbulence : TurbulenceData
defaultTurbulence =
    { baseFrequency = ( 0, 0 ), numOctaves = 1, seed = 0, stitchTiles = False, type_ = TurbulenceNoise }


{-| -}
offset : List (Attribute msg) -> { dx : Float, dy : Float } -> FilterOperation msg -> FilterOperation msg
offset =
    Offset


{-| -}
defaultOffset : { dx : Float, dy : Float }
defaultOffset =
    { dx = 0, dy = 0 }


{-| helper for creating a tuple from one value

this is used where svg expects a `numberoptionalnumber`, for instance to
define the radius of a circle: you can set both the x and y radius, or set only the
x, in which case the y-radius will default to the x-radius
-}
twice : a -> ( a, a )
twice x =
    ( x, x )


{-| -}
defaultDiffuseLighting : DiffuseLightingData
defaultDiffuseLighting =
    { surfaceScale = 1, diffuseConstant = 1, kernelUnitLength = twice 1, lightingColor = "FFF" }


{-| -}
specularLighting :
    List (Attribute msg)
    -> SpecularLightingData
    -> List LightingType
    -> FilterOperation msg
    -> FilterOperation msg
specularLighting =
    SpecularLighting


{-| -}
defaultSpecularLighting : SpecularLightingData
defaultSpecularLighting =
    { surfaceScale = 1, specularConstant = 1, specularExponent = 1, kernelUnitLength = twice 1, lightingColor = "FFF" }


{-| -}
defaultConvolveMatrix : ConvolveMatrixData
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


{-| -}
diffuseLighting :
    List (Attribute msg)
    -> DiffuseLightingData
    -> List LightingType
    -> FilterOperation msg
    -> FilterOperation msg
diffuseLighting =
    DiffuseLighting


{-| -}
tile : FilterOperation msg -> FilterOperation msg
tile =
    Tile


{-| -}
defaultImage : { link : String, preserveAspectRatio : String }
defaultImage =
    { link = "", preserveAspectRatio = "xMidYMid" }


{-| -}
image :
    List (Attribute msg)
    -> { link : String, preserveAspectRatio : String }
    -> FilterOperation msg
image =
    Image


{-| Concatenate multiple filter operations. The output of one filter will be the (final) input to the next
-}
concat : FilterOperation msg -> List (FilterOperation msg -> FilterOperation msg) -> FilterOperation msg
concat =
    Concat


{-| Applay multpile styles at the same time
-}
concurrent : List (FilterOperation msg) -> FilterOperation msg
concurrent =
    Concurrent


{-| define multiple filter operations sequentially (only the final one will be visible)
-}
sequential : List (FilterOperation msg) -> FilterOperation msg
sequential =
    Sequential



---- COMPILER ----


lightingTypeToElement : LightingType -> Svg msg
lightingTypeToElement lt =
    case lt of
        PointLight { x, y, z } ->
            Svg.fePointLight
                [ Svg.Attributes.x (toString x)
                , Svg.Attributes.y (toString y)
                , Svg.Attributes.z (toString z)
                ]
                []

        DistantLight { azimuth, elevation } ->
            Svg.feDistantLight [ Svg.Attributes.azimuth (toString azimuth), Svg.Attributes.elevation (toString elevation) ] []

        SpotLight { pointsAtX, pointsAtY, pointsAtZ, specularExponent, limitingConeAngle } ->
            Svg.feSpotLight
                [ Svg.Attributes.pointsAtX (toString pointsAtX)
                , Svg.Attributes.pointsAtY (toString pointsAtY)
                , Svg.Attributes.pointsAtZ (toString pointsAtZ)
                , Svg.Attributes.specularExponent (toString specularExponent)
                , Maybe.map (\lca -> Svg.Attributes.limitingConeAngle (toString lca)) limitingConeAngle
                    |> Maybe.withDefault empty
                ]
                []


handlePrimitive : FilterOperation msg -> ( String, List (Svg msg) )
handlePrimitive filter =
    case filter of
        Primitive inputImage ->
            ( toString inputImage, [] )

        Concat base operations ->
            handlePrimitive (List.foldl (<|) base operations)

        _ ->
            ( "elm-svg-filter_" ++ toString (hash filter), render filter )


resultAttribute filter =
    "elm-svg-filter_"
        ++ toString (hash filter)
        |> Svg.Attributes.result


combinator subfilters function extraAttributes specificAttributes =
    subfilters ++ [ function (extraAttributes ++ specificAttributes) [] ]


{-| Turns a filter definition into Svg
-}
render : FilterOperation msg -> List (Svg msg)
render filter =
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

                transferFunctionToAttributes transferfunc =
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
                            [ feFuncR (transferFunctionToAttributes transfers.r) []
                            , feFuncR (transferFunctionToAttributes transfers.g) []
                            , feFuncR (transferFunctionToAttributes transfers.b) []
                            , feFuncR (transferFunctionToAttributes transfers.a) []
                            ]
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

        DiffuseLighting attributes { surfaceScale, diffuseConstant, kernelUnitLength, lightingColor } lamps in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1
            in
                subfilters1
                    ++ [ feDiffuseLighting
                            (attributes
                                ++ [ Svg.Attributes.in_ reference1
                                   , Svg.Attributes.surfaceScale (toString surfaceScale)
                                   , Svg.Attributes.diffuseConstant (toString diffuseConstant)
                                   , Svg.Attributes.kernelUnitLength (toString (Tuple.first kernelUnitLength) ++ " " ++ toString (Tuple.second kernelUnitLength))
                                   , Svg.Attributes.lightingColor lightingColor
                                   , resultAttribute filter
                                   ]
                            )
                            (List.map lightingTypeToElement lamps)
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

                edgeMode_ =
                    Svg.Attributes.edgeMode <|
                        case edgeMode of
                            Duplicate ->
                                "duplicate"

                            Wrap ->
                                "wrap"

                            Zeros ->
                                "none"
            in
                combinator subfilters1 feGaussianBlur attributes <|
                    [ Svg.Attributes.in_ reference1
                    , Svg.Attributes.stdDeviation (toString stdDeviation)
                    , edgeMode_
                    , resultAttribute filter
                    ]

        Image attributes { link, preserveAspectRatio } ->
            [ feImage (attributes ++ [ Html.Attributes.attribute "xlink:href" link, Svg.Attributes.preserveAspectRatio preserveAspectRatio ]) []
            ]

        Offset attributes { dx, dy } in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1
            in
                combinator subfilters1 feOffset attributes <|
                    [ Svg.Attributes.in_ reference1
                    , resultAttribute filter
                    , Svg.Attributes.dx (toString dx)
                    , Svg.Attributes.dy (toString dy)
                    ]

        SpecularLighting attributes { surfaceScale, specularConstant, specularExponent, kernelUnitLength, lightingColor } lamps in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1
            in
                subfilters1
                    ++ [ feSpecularLighting
                            (attributes
                                ++ [ Svg.Attributes.in_ reference1
                                   , Svg.Attributes.surfaceScale (toString surfaceScale)
                                   , Svg.Attributes.specularConstant (toString specularConstant)
                                   , Svg.Attributes.specularExponent (toString specularExponent)
                                   , Svg.Attributes.kernelUnitLength (toString (Tuple.first kernelUnitLength) ++ " " ++ toString (Tuple.second kernelUnitLength))
                                   , Svg.Attributes.lightingColor lightingColor
                                   , resultAttribute filter
                                   ]
                            )
                            (List.map lightingTypeToElement lamps)
                       ]

        Tile in1 ->
            let
                ( reference1, subfilters1 ) =
                    handlePrimitive in1
            in
                subfilters1 ++ [ feTile [ Svg.Attributes.in_ reference1, resultAttribute filter ] [] ]

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

        Concat base filters ->
            render (List.foldl (<|) base filters)

        Sequential filters ->
            List.concatMap render filters

        Concurrent operations ->
            let
                evaluate operation =
                    let
                        ( reference1, subfilters1 ) =
                            operation
                                |> Debug.log "to be handled"
                                |> handlePrimitive
                                |> Debug.log "evaluated concurrently"
                    in
                        ( subfilters1, feMergeNode [ Svg.Attributes.in_ reference1 ] [] )

                ( subs, nodes ) =
                    List.map evaluate operations
                        |> List.unzip
            in
                (List.concat subs) ++ [ feMerge [] nodes ]

        Primitive image ->
            []
