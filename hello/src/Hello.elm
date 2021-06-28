module Hello exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html msg
main =
    div []
        [ h1 [] [ text "Useful links" ]
        , ul []
            [ li [] [ a [ href "" ] [ text "Homepage" ] ]
            , li [] [ a [ href "" ] [ text "Packages" ] ]
            , li [] [ a [ href "" ] [ text "Playground" ] ]
            ]
        ]
