module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html msg
main =
    div [] [ header, content ]


header : Html msg
header =
    h1 [] [ text "Useful links" ]


content : Html msg
content =
    ul []
        [ linkItem "" "Homepage"
        , linkItem "" "Packages"
        , linkItem "" "Playground"
        ]

linkItem : String -> String -> Html msg
linkItem url text_ =
    li [] [a [href url] [text text_]]