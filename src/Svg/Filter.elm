module Svg.Filter exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes
import Html
import String
import Svg.Operations.Internal exposing (..)


type Filter msg
    = Filter (List (Svg.Attribute msg)) { id : String, filterUnits : Unit } (FilterOperation msg)


create : List (Svg.Attribute msg) -> { id : String, filterUnits : Unit } -> FilterOperation msg -> Filter msg
create =
    Filter


type Unit
    = UserSpaceOnUse
    | ObjectBoundingBox


toElement : Filter msg -> Svg msg
toElement (Filter attributes { id, filterUnits } operation) =
    let
        unit =
            case filterUnits of
                UserSpaceOnUse ->
                    "userSpaceOnUse"

                ObjectBoundingBox ->
                    "objectBoundingBox"
    in
        Svg.filter (attributes ++ [ Svg.Attributes.id id, Svg.Attributes.filterUnits unit ]) (Svg.Operations.Internal.render operation)


useFilter : Filter msg -> Attribute msg
useFilter (Filter _ { id } _) =
    Html.Attributes.style [ ( "filter", "url(#" ++ id ++ ")" ) ]


singleton x =
    [ x ]
