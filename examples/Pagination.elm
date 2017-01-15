module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (defs, svg)
import Animation exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { current : Int
    , items : List String
    , styles : List Animation.State
    }


init =
    let
        images =
            [ "https://tympanus.net/Development/CreativeGooeyEffects/img/1.jpg"
            , "https://tympanus.net/Development/CreativeGooeyEffects/img/2.jpg"
            , "https://tympanus.net/Development/CreativeGooeyEffects/img/3.jpg"
            , "https://tympanus.net/Development/CreativeGooeyEffects/img/4.jpg"
            , "https://tympanus.net/Development/CreativeGooeyEffects/img/5.jpg"
            ]

        styles =
            Animation.style [ Animation.opacity 1.0 ] :: List.repeat (List.length images - 1) (Animation.style [ Animation.opacity 0.0 ])
    in
        ( { current = 1, items = images, styles = styles }
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate model.styles


type Msg
    = Clicked Int
    | Animate Animation.Msg


update msg model =
    case msg of
        Clicked n ->
            ( model, Cmd.none )

        Animate animMsg ->
            ( { model | styles = List.map (Animation.update animMsg) model.styles }, Cmd.none )


viewImage current imageSrc =
    div [ class "pagination-item" ]
        [ img [ alt "image", src imageSrc ]
            []
        ]


dots current n =
    button [ class "pagination-dot", onClick (Clicked n) ]
        [ span [ class "pagination-number" ]
            [ text (toString n) ]
        ]


view model =
    div []
        [ Html.node "style" [] [ text css ]
        , div [ class "demo-3 content" ]
            [ div [ class "pagination" ]
                [ div [ class "pagination-content" ] (List.map (viewImage model.current) model.items)
                , div [ class "pagination-navigation" ]
                    [ div [ class "pagination-current" ] []
                    , div [ class "pagination-dots" ] (List.map (dots model.current) (List.range 1 5))
                    ]
                , p [ class "info" ]
                    [ text "Based on the Dribbble shot "
                    , a [ href "https://dribbble.com/shots/1676635-Page-scroll-concept" ]
                        [ text "Page scroll concept" ]
                    , text "by Kreativa Studio"
                    ]
                ]
            ]
        , svg [ attribute "version" "1.1", attribute "width" "800", attribute "xmlns" "http://www.w3.org/2000/svg" ]
            [ defs []
                [ node "filter"
                    [ id "goo" ]
                    [ node "feGaussianBlur"
                        [ attribute "in" "SourceGraphic", attribute "result" "blur", attribute "stdDeviation" "10" ]
                        []
                    , node "feColorMatrix"
                        [ attribute "in" "blur", attribute "mode" "matrix", attribute "result" "goo", attribute "values" "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 19 -9" ]
                        []
                    , node "feComposite"
                        [ attribute "in" "SourceGraphic", attribute "in2" "goo", attribute "operator" "atop" ]
                        []
                    ]
                ]
            ]
        ]


css =
    """
@import url(//fonts.googleapis.com/css?family=Raleway:100,400,600,800);
@font-face {
font-weight: normal;
font-style: normal;
font-family: 'codropsicons';
src: url("../fonts/codropsicons/codropsicons.eot");
src: url("../fonts/codropsicons/codropsicons.eot?#iefix") format("embedded-opentype"), url("../fonts/codropsicons/codropsicons.woff") format("woff"), url("../fonts/codropsicons/codropsicons.ttf") format("truetype"), url("../fonts/codropsicons/codropsicons.svg#codropsicons") format("svg");
}

*,
*:after,
*:before {
-webkit-box-sizing: border-box;
box-sizing: border-box;
}

.cf:before,
.cf:after {
content: '';
display: table;
}

.cf:after {
clear: both;
}

body {
background: #46484a;
color: #a8a8a8;
font-weight: 400;
font-size: 1em;
font-family: 'Raleway', 'Avenir Next', 'Helvetica Neue', Helvetica, Arial, sans-serif;
}

a {
color: #fff;
text-decoration: none;
outline: none;
}

a:hover,
a:focus,
.codrops-demos a.current-demo {
color: #eab1c6;
outline: none;
}

button:focus {
outline: none;
}

.container {
margin: 0 auto;
text-align: center;
overflow: hidden;
}

.content {
text-align: left;
}

.info {
text-align: center;
padding-top: 2.5em;
font-size: 0.85em;
font-weight: 600;
}

/* Header */
.codrops-header {
padding: 2em 0 4em;
letter-spacing: -1px;
}

.codrops-header h1 {
font-weight: 100;
font-size: 2.9em;
line-height: 1;
margin: 0;
color: #6fafdd;
}

.codrops-header h1 span {
display: block;
font-size: 40%;
letter-spacing: 0;
padding: 10px 0 30px;
color: #a8a8a8;
}

.codrops-links {
position: relative;
display: inline-block;
white-space: nowrap;
text-align: center;
font-size: 0.9em;
margin-bottom: 15px;
min-height: 32px;
}

.codrops-links::after {
position: absolute;
top: 0;
left: 50%;
width: 1px;
height: 100%;
background: rgba(0,0,0,0.2);
content: '';
-webkit-transform: rotate3d(0, 0, 1, 22.5deg);
transform: rotate3d(0, 0, 1, 22.5deg);
}

.codrops-icon {
display: inline-block;
margin: 0.5em;
padding: 0em 0;
width: 1.5em;
text-decoration: none;
}

.codrops-icon:before {
margin: 0 5px;
text-transform: none;
font-weight: normal;
font-style: normal;
font-variant: normal;
font-family: 'codropsicons';
line-height: 1;
speak: none;
-webkit-font-smoothing: antialiased;
}

.codrops-icon span {
display: none;
}

.codrops-icon--drop:before {
content: "\\e001";
}

.codrops-icon--prev:before {
content: "\\e004";
}

/* Demo links */
.codrops-demos a {
display: inline-block;
text-transform: uppercase;
letter-spacing: 1px;
font-size: 0.65em;
background: #36383a;
padding: 0.75em 1.5em;
margin: 0.25em;
border-radius: 2px;
}

/* Related demos */
.content--related {
text-align: center;
font-weight: 400;
}

.media-item {
display: inline-block;
padding: 1em;
margin: 1em 0 0 0;
vertical-align: top;
-webkit-transition: color 0.3s;
transition: color 0.3s;
}

.media-item__img {
opacity: 0.3;
max-width: 100%;
-webkit-transition: opacity 0.3s;
transition: opacity 0.3s;
}

.media-item:hover .media-item__img,
.media-item:focus .media-item__img {
opacity: 1;
}

.media-item__title {
font-size: 0.85em;
margin: 0;
padding: 0.5em;
font-weight: 400;
}

/* Demo color */
.demo-2 { background: #4D4D52; }
.demo-2 a:hover,
.demo-2 a:focus,
.demo-2 .codrops-demos a.current-demo { color: #f59393; }

.demo-2 .codrops-demos a {
background: #7e7878;
}

.demo-2 .codrops-header h1 { color: #f59393; }

.demo-3 { background: #eeeeef; }
.demo-3 a { color: #322930; }
.demo-3 .codrops-demos a { background: #fff; }

.demo-3 a:hover,
.demo-3 a:focus,
.demo-3 .codrops-demos a.current-demo {
color: #c59e6f;
}

.demo-3 .codrops-header h1 { color: #c59e6f; }

.demo-4 { background: #4a4646; }
.demo-4 .codrops-demos a { background: #36383a; }

.demo-4 a:hover,
.demo-4 a:focus,
.demo-4 .codrops-demos a.current-demo {
color: #C87070;
}

.demo-4 .codrops-header h1 { color: #C87070; }

.demo-5 { background: #f9f9f9; }
.demo-5 a { color: #f78e42; }
.demo-5 .codrops-demos a { background: #d8d8d8; color: #fff;}

.demo-5 a:hover,
.demo-5 a:focus,
.demo-5 .codrops-demos a.current-demo {
color: #398B44;
}

.demo-5 .codrops-header h1 { color: #62ed74; }

.demo-6 { background: #3D4444; }
.demo-6 a { color: #DA7071; }
.demo-6 .codrops-demos a { background: #353838; color: #fff;}

.demo-6 a:hover,
.demo-6 a:focus,
.demo-6 .codrops-demos a.current-demo {
color: #a8a8a8;
}

.demo-6 .codrops-header h1 { color: #fff; }

.demo-7 { background: #2d2d2d; }
.demo-7 .codrops-demos a { background: #3A3A3A; color: #E2E2E2;}

.demo-7 a:hover,
.demo-7 a:focus,
.demo-7 .codrops-demos a.current-demo {
color: #FBA407;
}

.demo-7 .codrops-header h1 { color: #808080; }

.demo-1 #cdawrap,
.demo-2 #cdawrap,
.demo-4 #cdawrap,
.demo-6 #cdawrap,
.demo-7 #cdawrap {
background: none;
border: 1px solid rgba(0,0,0,0.1);
}

.demo-3 #cdawrap,
.demo-5 #cdawrap {
border: none;
}

.demo-1 #cdawrap .carbon-text,
.demo-2 #cdawrap .carbon-text,
.demo-4 #cdawrap .carbon-text,
.demo-6 #cdawrap .carbon-text,
.demo-7 #cdawrap .carbon-text {
color: #f0f0f0;
}

@media screen and (max-width:50em) {
.codrops-header {
    padding: 3em 10% 4em;
}
}

@media screen and (max-width:40em) {
.codrops-header h1 {
    font-size: 2.8em;
}
}
.pagination {
position: relative;
width: 100%;
max-width: 800px;
margin: 0 auto;
}

.pagination-navigation {
position: relative;
text-align: center;
font-size: 0;
-webkit-filter: url("#goo");
filter: url("#goo");
-webkit-transform: translateZ(0);
transform: translateZ(0);
}

.pagination-dot,
.pagination-current {
position: relative;
display: inline-block;
width: 32px;
height: 32px;
padding: 0;
line-height: 32px;
background: #d5d3d2;
border-radius: 50%;
margin: 0 13px;
border: none;
outline: none;
font-size: 16px;
font-weight: bold;
color: #fff;
font-family: 'Avenir Next', 'Helvetica Neue', Helvetica, Arial, sans-serif;
}

.pagination-number {
display: block;
width: 100%;
height: 100%;
border-radius: 50%;
-webkit-transform: scale(0.01, 0.01);
transform: scale(0.01, 0.01);
}

.pagination-current {
position: absolute;
left: 0;
top: 0;
}

.pagination-content {
height: 300px;
}

.pagination-item {
position: absolute;
left: 50%;
margin-left: -200px;
font-size: 0;
border-radius: 5px;
border: 7px solid #fff;
}

.pagination-item img {
position: relative;
opacity: 0.95;
}
"""
