module ColorMatrix exposing (..)

import Svg
import Svg.Attributes exposing (..)
import Html exposing (img, div, text)
import Html.Attributes exposing (src)
import Svg.ColorFilters as ColorFilters
import Svg.Operations exposing (gaussianBlur, concurrent)
import Svg.Operations.Internal exposing (defaultGaussianBlur, offset, defaultOffset, sourceAlpha, FilterOperation, sourceGraphic)
import Svg.Filter as Filter exposing (Unit(..))


{-| Create a dropshadow around the input filter operation
-}
dropShadow : Float -> Float -> Float -> FilterOperation msg -> FilterOperation msg
dropShadow stdDeviation offsetX offsetY input =
    [ gaussianBlur stdDeviation sourceAlpha
        |> Svg.Operations.Internal.offset [] { defaultOffset | dx = offsetX, dy = offsetY }
    , input
    ]
        |> concurrent



{-
   raised =
       sourceAlpha
           |> specularLighting []
               { defaultSpecularLighting | specularConstant = 1.2, specularExponent = 12, color = "#bbbbbb" }
               [ distantLight { aziumth = 45, elevation = 45 } ]
           |> composite [] { defaultComposite | operator = Arithmetic, k2 = 1, k3 = 1 } sourceGraphic


-}


marten attributes =
    img
        ([ Html.Attributes.width 600
         , src "https://i.guim.co.uk/img/media/a1661827fc303dbff4c4f4c269a1c1256288a736/133_597_4693_2815/master/4693.jpg?w=1920&q=55&auto=format&usm=12&fit=max&s=535e2a3cd529d488a0ae7e59bac65dee"
         ]
            ++ attributes
        )
        []


rutkowski attributes =
    img
        ([ Html.Attributes.width 600
         , src "http://www.iamag.co/features/itsart/wp-content/uploads/2015/11/Grzegorz-Rutkowski-21.jpg"
         ]
            ++ attributes
        )
        []


creator filter =
    let
        id =
            Svg.Operations.Internal.hash (toString filter)
                |> toString
                |> (\s -> "filter_" ++ s)
    in
        Filter.create [ x "0", y "0", width "100%", height "100%" ] { id = id, filterUnits = ObjectBoundingBox } filter


colorFilters =
    [ ( "greyscale", ColorFilters.greyscale sourceGraphic )
    , ( "monochrome", ColorFilters.monochrome sourceGraphic )
    , ( "sepia", ColorFilters.sepia sourceGraphic )
    , ( "nightvision", ColorFilters.nightvision sourceGraphic )
    , ( "warm", ColorFilters.warm sourceGraphic )
    ]
        |> List.map (Tuple.mapSecond creator)


main =
    div []
        [ Svg.svg []
            [ Svg.defs [] (List.map (\( _, filter ) -> Filter.toElement filter) colorFilters)
            ]
        , div [] (List.concatMap (\( name, filter ) -> [ text name, rutkowski [ Filter.useFilter filter ] ]) colorFilters)
        ]
