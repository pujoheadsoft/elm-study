module Skelton exposing
    ( Details
    , Segment
    , Warning(..)
    , authorSegment
    , projectSegment
    , versionSegment
    , view
    )

import Browser
import Elm.Version as V
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Utils.Logo as Logo


type alias Details msg =
    { title : String
    , header : List Segment
    , warning : Warning
    , attrs : List (Attribute msg)
    , kids : List (Html msg)
    }


type Warning
    = NoProblems
    | WarnOld
    | WarnMoved String String
    | WarnNewerVersion String V.Version


type Segment
    = Text String
    | Link String String


authorSegment : String -> Segment
authorSegment author =
    Text author


projectSegment : String -> String -> Segment
projectSegment author project =
    Link (Href.toProject author project) project


versionSegment : String -> String -> Maybe V.Version -> Segment
versionSegment author project version =
    Link (Href.toVersion author project version) project


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
    case maybeVersion of
        Nothing ->
            "latest"

        Just version ->
            V.toString version


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    let _ = Debug.log "Skelton view" "" in
    { title = details.title
    , body =
        [ viewHeader details.header
        , lazy viewWarning details.warning
        , Html.map toMsg <|
            div (class "center" :: style "flex" "1" :: details.attrs) details.kids
        , viewFooter
        ]
    }


viewHeader : List Segment -> Html msg
viewHeader segments =
    div [ class "header" ]
        [ div [ class "nav" ]
            [ viewLogo
            , case segments of
                [] ->
                    text ""

                _ ->
                    h1 [] (List.intersperse slash (List.map viewSegment segments))
            ]
        ]


slash : Html msg
slash =
    span [ class "spacey-char" ] [ text "/" ]


viewSegment : Segment -> Html msg
viewSegment segment =
    case segment of
        Text string ->
            text string

        Link address string ->
            a [ href address ] [ text string ]


viewWarning : Warning -> Html msg
viewWarning warning =
    div [ class "header-underbar" ] <|
        case warning of
            NoProblems ->
                []

            WarnOld ->
                [ p [ class "version-warning" ]
                    [ text "NOTE - this package is compatible with Elm 0.19.1" ]
                ]

            WarnMoved author project ->
                [ p [ class "version-warning" ]
                    [ text "NOTE - this package move to "
                    , a [ href (Href.toVersion author project Nothing) ]
                        [ text (author ++ "/" ++ project) ]
                    ]
                ]

            WarnNewerVersion url version ->
                [ p [ class "version-warning" ]
                    [ text "NOTE - the latest version is "
                    , a [ href url ] [ text (V.toString version) ]
                    ]
                ]


viewFooter : Html msg
viewFooter =
    div [ class "footer" ]
        [ a [ class "grey-link", href "https://github.com/elm/package.elm-lang.org/" ]
            [ text "Site Source" ]
        , text " - 2012-2020 Evan Czaplicki"
        ]


viewLogo : Html msg
viewLogo =
    a
        [ href "/"
        , style "text-decoration" "none"
        , style "margin-right" "32px"
        , style "display" "flex"
        , style "align-items" "center"
        ]
        [ Logo.logo 32
        , div
            [ style "padding-left" "8px" ]
            [ div
                [ style "line-height" "24px"
                , style "font-size" "30px"
                ]
                [ text "elm" ]
            , div
                [ style "font-size" "12px" ]
                [ text "packages" ]
            ]
        ]
