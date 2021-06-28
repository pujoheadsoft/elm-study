module Page.Docs exposing (..)

import Browser.Dom as Dom
import Dict
import Elm.Constraint as C
import Elm.Docs as Docs
import Elm.License as License
import Elm.Package as Pkg
import Elm.Project as Outline
import Elm.Version as V
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Html.Lazy exposing (..)
import Page.Docs.Block as Block
import Page.Problem as Problem
import Release
import Session
import Skelton
import Task
import Time
import Url.Builder as Url
import Url.Parser exposing (query)
import Utils.Markdown as Markdown
import Utils.OneOrMore exposing (OneOrMore)


type alias Model =
    { session : Session.Data
    , author : String
    , project : String
    , version : Maybe V.Version
    , focus : Focus
    , query : String
    , releases : Status (OneOrMore Release.Release)
    , readme : Status String
    , docs : Status (List Docs.Module)
    , outline : Status Outline.PackageInfo
    }


type Focus
    = Readme
    | About
    | Module String (Maybe String)


type Status a
    = Failure
    | Loading
    | Success a


type DocsError
    = NotFound
    | FoundButMissingModule


init : Session.Data -> String -> String -> Maybe V.Version -> Focus -> ( Model, Cmd Msg )
init session author project version focus =
    case Session.getReleases session author project of
        Just releases ->
            getInfo (Release.getLatestVersion releases) <|
                Model session author project version focus "" (Success releases) Loading Loading Loading

        Nothing ->
            ( Model session author project version focus "" Loading Loading Loading Loading
            , Session.fetchReleases author project GotReleases
            )


getInfo : V.Version -> Model -> ( Model, Cmd Msg )
getInfo latest model =
    let
        author =
            model.author

        project =
            model.project

        version =
            Maybe.withDefault latest model.version

        maybeInfo =
            Maybe.map3 (\a b c -> ( a, b, c ))
                (Session.getReadme model.session author project version)
                (Session.getDocs model.session author project version)
                (Session.getOutline model.session author project version)
    in
    case maybeInfo of
        Nothing ->
            ( model
            , Cmd.batch
                [ Session.fetchReadme author project version (GotReadme version)
                , Session.fetchDocs author project version (GotDocs version)
                , Session.fetchOutline author project version (GotOutline version)
                ]
            )

        Just ( readme, docs, outline ) ->
            ( { model
                | readme = Success readme
                , docs = Success docs
                , outline = Success outline
              }
            , scrollIfNeeded model.focus
            )


scrollIfNeeded : Focus -> Cmd Msg
scrollIfNeeded focus =
    case focus of
        Module _ (Just tag) ->
            Task.attempt ScrollAttempted
                (Dom.getElement tag
                    |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
                )

        _ ->
            Cmd.none


type Msg
    = QueryChanged String
    | ScrollAttempted (Result Dom.Error ())
    | GotReleases Session.ReleaseResult
    | GotReadme V.Version Session.StringResult
    | GotDocs V.Version Session.DocsResult
    | GotOutline V.Version Session.OutlineResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChanged query ->
            ( { model | query = query }, Cmd.none )

        ScrollAttempted _ ->
            ( model, Cmd.none )

        GotReleases result ->
            case result of
                Err _ ->
                    ( { model
                        | releases = Failure
                        , readme = Failure
                        , docs = Failure
                        , outline = Failure
                      }
                    , Cmd.none
                    )

                Ok releases ->
                    getInfo (Release.getLatestVersion releases)
                        { model
                            | releases = Success releases
                            , session = Session.addReleases model.author model.project releases model.session
                        }

        GotReadme version result ->
            case result of
                Err _ ->
                    ( { model | readme = Failure }, Cmd.none )

                Ok readme ->
                    ( { model
                        | readme = Success readme
                        , session = Session.addReadme model.author model.project version readme model.session
                      }
                    , Cmd.none
                    )

        GotDocs version result ->
            case result of
                Err _ ->
                    ( { model | docs = Failure }
                    , Cmd.none
                    )

                Ok docs ->
                    ( { model
                        | docs = Success docs
                        , session = Session.addDocs model.author model.project version docs model.session
                      }
                    , Cmd.none
                    )

        GotOutline version result ->
            case result of
                Err _ ->
                    ( { model | outline = Failure }, Cmd.none )

                Ok outline ->
                    ( { model
                        | outline = Success outline
                        , session = Session.addOutline model.author model.project version outline model.session
                      }
                    , Cmd.none
                    )


view : Model -> Skelton.Details Msg
view model =
    { title = toTitle model
    , header = toHeader model
    , warning = toWarning model
    , attrs = []
    , kids =
        [ viewContent model
        , viewSidebar model
        ]
    }


toTitle : Model -> String
toTitle model =
    case model.focus of
        Readme ->
            toGenericTitle model

        About ->
            toGenericTitle model

        Module name _ ->
            name ++ " - " ++ toGenericTitle model


toGenericTitle : Model -> String
toGenericTitle model =
    case getVersion model of
        Just version ->
            model.project ++ " " ++ V.toString version

        Nothing ->
            model.project


getVersion : Model -> Maybe V.Version
getVersion model =
    case model.version of
        Just version ->
            model.version

        Nothing ->
            case model.releases of
                Success releases ->
                    Just (Release.getLatestVersion releases)

                Loading ->
                    Nothing

                Failure ->
                    Nothing


toHeader : Model -> List Skelton.Segment
toHeader model =
    [ Skelton.authorSegment model.author
    , Skelton.projectSegment model.author model.project
    , Skelton.versionSegment model.author model.project (getVersion model)
    ]


toWarning : Model -> Skelton.Warning
toWarning model =
    case Dict.get (model.author ++ "/" ++ model.project) renames of
        Just ( author, project ) ->
            Skelton.WarnMoved author project

        Nothing ->
            case model.outline of
                Failure ->
                    warnIfNewer model

                Loading ->
                    warnIfNewer model

                Success outline ->
                    if isOld outline.elm then
                        Skelton.WarnOld

                    else
                        warnIfNewer model


warnIfNewer : Model -> Skelton.Warning
warnIfNewer model =
    case model.version of
        Nothing ->
            Skelton.NoProblems

        Just version ->
            case model.releases of
                Failure ->
                    Skelton.NoProblems

                Loading ->
                    Skelton.NoProblems

                Success releases ->
                    let
                        latest =
                            Release.getLatestVersion releases
                    in
                    if version == latest then
                        Skelton.NoProblems

                    else
                        Skelton.WarnNewerVersion (toNewerUrl model) latest


toNewerUrl : Model -> String
toNewerUrl model =
    case model.focus of
        Readme ->
            Href.toVersion model.author model.project Nothing

        About ->
            Href.toAbout model.author model.project Nothing

        Module m v ->
            Href.toModule model.author model.project Nothing m v


renames : Dict.Dict String ( String, String )
renames =
    Dict.fromList
        [ ( "evancz/elm-effects", ( "elm", "core" ) )
        , ( "evancz/elm-html", ( "elm", "html" ) )
        , ( "evancz/elm-http", ( "elm", "http" ) )
        , ( "evancz/elm-svg", ( "elm", "svg" ) )
        , ( "evancz/start-app", ( "elm", "html" ) )
        , ( "evancz/virtual-dom", ( "elm", "virtual-dom" ) )
        , ( "elm-lang/animation-frame", ( "elm", "browser" ) )
        , ( "elm-lang/core", ( "elm", "core" ) )
        , ( "elm-lang/html", ( "elm", "html" ) )
        , ( "elm-lang/http", ( "elm", "http" ) )
        , ( "elm-lang/svg", ( "elm", "svg" ) )
        , ( "elm-lang/virtual-dom", ( "elm", "virtual-dom" ) )
        , ( "elm-community/elm-list-extra", ( "elm-community", "list-extra" ) )
        , ( "elm-community/elm-linear-algebra", ( "elm-community", "linear-algebra" ) )
        , ( "elm-community/elm-lazy-list", ( "elm-community", "lazy-list" ) )
        , ( "elm-community/elm-json-extra", ( "elm-community", "json-extra" ) )
        ]


isOld : C.Constraint -> Bool
isOld c =
    case String.split " " (C.toString c) of
        [ mini, minop, _, maxop, maxi ] ->
            Maybe.withDefault False <|
                Maybe.map4 (\low lop hop high -> not (lop low ( 0, 19, 1 ) && hop ( 0, 19, 1 ) high))
                    (getVsn mini)
                    (getOp minop)
                    (getOp maxop)
                    (getVsn maxi)

        _ ->
            False


getVsn : String -> Maybe ( Int, Int, Int )
getVsn vsn =
    case List.filterMap String.toInt (String.split "." vsn) of
        [ x, y, z ] ->
            Just ( x, y, z )

        _ ->
            Nothing


getOp : String -> Maybe (comparable -> comparable -> Bool)
getOp op =
    case op of
        "<" ->
            Just (<)

        "<=" ->
            Just (<=)

        _ ->
            Nothing


viewContent : Model -> Html msg
viewContent model =
    case model.focus of
        Readme ->
            lazy viewReadme model.readme

        About ->
            lazy2 viewAbout model.outline model.releases

        Module name tag ->
            lazy5 viewModule model.author model.project model.version name model.docs


viewReadme : Status String -> Html msg
viewReadme status =
    case status of
        Success readme ->
            div [ class "block-list" ] [ Markdown.block readme ]

        Loading ->
            div [ class "block-list" ] [ text "" ]

        Failure ->
            div
                (class "block-list" :: Problem.styles)
                (Problem.offline "README.md")


viewModule : String -> String -> Maybe V.Version -> String -> Status (List Docs.Module) -> Html msg
viewModule author project version name status =
    case status of
        Success allDocs ->
            case findModule name allDocs of
                Just docs ->
                    let
                        header =
                            h1 [ class "block-list-title" ] [ text name ]

                        info =
                            Block.makeInfo author project version name allDocs

                        blocks =
                            List.map (Block.view info) (Docs.toBlocks docs)
                    in
                    div [ class "block-list" ] (header :: blocks)

                Nothing ->
                    div
                        (class "block-list" :: Problem.styles)
                        (Problem.missingModule author project version name)

        Loading ->
            div [ class "block-list" ] [ h1 [ class "block-list-title" ] [ text name ] ]

        Failure ->
            div
                (class "block-list" :: Problem.styles)
                (Problem.offline "docs.json")


findModule : String -> List Docs.Module -> Maybe Docs.Module
findModule name docList =
    case docList of
        [] ->
            Nothing

        docs :: otherDocs ->
            if docs.name == name then
                Just docs

            else
                findModule name otherDocs


viewSidebar : Model -> Html Msg
viewSidebar model =
    div
        [ class "pkg-nav" ]
        [ ul []
            [ li [] [ lazy4 viewReadmeLink model.author model.project model.version model.focus ]
            , li [] [ lazy4 viewAboutLink model.author model.project model.version model.focus ]
            , li [] [ lazy4 viewBrowserSourceLink model.author model.project model.version model.releases ]
            ]
        , h2 [] [ text "Module" ]
        , input
            [ placeholder "Search"
            , value model.query
            , onInput QueryChanged
            ]
            []
        , viewSidebarModules model
        ]


viewSidebarModules : Model -> Html msg
viewSidebarModules model =
    case model.docs of
        Failure ->
            text ""

        Loading ->
            text ""

        Success modules ->
            if String.isEmpty model.query then
                let
                    viewEntry docs =
                        li [] [ viewModuleLink model docs.name ]
                in
                ul [] (List.map viewEntry modules)

            else
                let
                    query =
                        String.toLower model.query
                in
                ul [] (List.filterMap (viewSearchItem model query) modules)


viewSearchItem : Model -> String -> Docs.Module -> Maybe (Html msg)
viewSearchItem model query docs =
    let
        toItem ownerName valueName =
            viewValueItem model docs.name ownerName valueName

        matches =
            List.filterMap (isMatch query toItem) docs.binops
                ++ List.concatMap (isUnionMatch query toItem) docs.unions
                ++ List.filterMap (isMatch query toItem) docs.aliases
                ++ List.filterMap (isMatch query toItem) docs.values
    in
    if List.isEmpty matches && not (String.contains query docs.name) then
        Nothing

    else
        Just <|
            li
                [ class "pkg-nav-search-chunk" ]
                [ viewModuleLink model docs.name
                , ul [] matches
                ]


isMatch : String -> (String -> String -> b) -> { r | name : String } -> Maybe b
isMatch query toResult { name } =
    if String.contains query (String.toLower name) then
        Just (toResult name name)

    else
        Nothing


isUnionMatch : String -> (String -> String -> a) -> Docs.Union -> List a
isUnionMatch query toResult { name, tags } =
    let
        tagMatches =
            List.filterMap (isTagMatch query toResult name) tags
    in
    if String.contains query (String.toLower name) then
        toResult name name :: tagMatches

    else
        tagMatches


isTagMatch : String -> (String -> String -> a) -> String -> ( String, details ) -> Maybe a
isTagMatch query toResult tipeName ( tagName, _ ) =
    if String.contains query (String.toLower tagName) then
        Just (toResult tipeName tagName)

    else
        Nothing


viewReadmeLink : String -> String -> Maybe V.Version -> Focus -> Html msg
viewReadmeLink author project version focus =
    navLink "README" (Href.toVersion author project version) <|
        case focus of
            Readme ->
                True

            About ->
                False

            Module _ _ ->
                False


viewAboutLink : String -> String -> Maybe V.Version -> Focus -> Html msg
viewAboutLink author project version focus =
    navLink "About" (Href.toAbout author project version) <|
        case focus of
            Readme ->
                False

            About ->
                False

            Module _ _ ->
                False


viewBrowserSourceLink : String -> String -> Maybe V.Version -> Status (OneOrMore Release.Release) -> Html msg
viewBrowserSourceLink author project maybeVersion releasesStatus =
    case maybeVersion of
        Just version ->
            viewBrowserSourceLinkHelp author project version

        Nothing ->
            case releasesStatus of
                Success releases ->
                    viewBrowserSourceLinkHelp author project (Release.getLatestVersion releases)

                Loading ->
                    text "Source"

                Failure ->
                    text "Source"


viewBrowserSourceLinkHelp : String -> String -> V.Version -> Html msg
viewBrowserSourceLinkHelp author project version =
    let
        url =
            Url.crossOrigin
                "https://github.com"
                [ author, project, "tree", V.toString version ]
                []
    in
    a [ class "pkg-nav-module", href url ] [ text "Source" ]


viewModuleLink : Model -> String -> Html msg
viewModuleLink model name =
    let
        url =
            Href.toModule model.author model.project model.version name Nothing
    in
    navLink name url <|
        case model.focus of
            Readme ->
                False

            About ->
                False

            Module selectedName _ ->
                selectedName == name


viewValueItem : Model -> String -> String -> String -> Html msg
viewValueItem { author, project, version } moduleName ownerName valueName =
    let
        url =
            Href.toModule author project version moduleName (Just ownerName)
    in
    li [ class "pkg-nav-value" ] [ navLink valueName url False ]


viewAbout : Status Outline.PackageInfo -> Status (OneOrMore Release.Release) -> Html msg
viewAbout outlineStatus releases =
    case outlineStatus of
        Success outline ->
            div [ class "block-list pkg-about" ]
                [ h1 [ class "block-list-title" ] [ text "About" ]
                , p [] [ text outline.summary ]
                , pre [] [ code [] [ text ("elm install " ++ Pkg.toString outline.name) ] ]
                , p []
                    [ text "Published "
                    , viewReleaseTime outline releases
                    , text " under the "
                    , a [ href (toLicenseUrl outline) ] [ code [] [ text (License.toString outline.license) ] ]
                    , text " license."
                    ]
                , p []
                    [ text "Elm version "
                    , code [] [ text (C.toString outline.elm) ]
                    ]
                , case outline.deps of
                    [] ->
                        text ""

                    _ :: _ ->
                        div []
                            [ h1 [ style "margin-top" "2em", style "mergin-bottom" "0.5em" ] [ text "Dependancies" ]
                            , table [] (List.map viewDependency outline.deps)
                            ]
                ]

        Loading ->
            div [ class "block-list pkg-about" ] [ text "" ]

        Failure ->
            div
                (class "block-list pkg-bout" :: Problem.styles)
                (Problem.offline "elm.json")


viewReleaseTime : Outline.PackageInfo -> Status (OneOrMore Release.Release) -> Html msg
viewReleaseTime outline releasesStatus =
    case releasesStatus of
        Failure ->
            text ""

        Loading ->
            text ""

        Success releases ->
            case Release.getTime outline.version releases of
                Nothing ->
                    text ""

                Just time ->
                    span [] [ text "on", code [] [ text (timeToString time) ] ]


timeToString : Time.Posix -> String
timeToString time =
    String.fromInt (Time.toDay Time.utc time)
        ++ " "
        ++ monthToString (Time.toMonth Time.utc time)
        ++ " "
        ++ String.fromInt (Time.toYear Time.utc time)


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


toLicenseUrl : Outline.PackageInfo -> String
toLicenseUrl outline =
    Url.crossOrigin
        "https://github.com"
        [ Pkg.toString outline.name, "blob", V.toString outline.version, "LICENSE" ]
        []


viewDependency : ( Pkg.Name, C.Constraint ) -> Html msg
viewDependency ( pkg, constraint ) =
    tr []
        [ td []
            [ case String.split "/" (Pkg.toString pkg) of
                [ author, project ] ->
                    a [ href (Href.toVersion author project Nothing) ]
                        [ span [ class "light" ] [ text (author ++ "/") ]
                        , text project
                        ]

                _ ->
                    text (Pkg.toString pkg)
            ]
        ]


navLink : String -> String -> Bool -> Html msg
navLink name url isBold =
    let
        attributes =
            if isBold then
                [ class "pkg-nav-module"
                , style "font-weight" "bold"
                , style "text-decoration" "underline"
                ]

            else
                [ class "pkg-nav-module" ]
    in
    a (href url :: attributes) [ text name ]
