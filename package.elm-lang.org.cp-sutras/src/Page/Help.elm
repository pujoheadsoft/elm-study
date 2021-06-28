module Page.Help exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Markdown
import Session
import Skelton


type alias Model =
    { session : Session.Data
    , title : String
    , content : Content
    }


type Content
    = Failure
    | Loading
    | Success String


init : Session.Data -> String -> String -> ( Model, Cmd Msg )
init session title url =
    ( Model session title Loading
    , Http.get
        { url = url
        , expect = Http.expectString GotContent
        }
    )


type Msg
    = GotContent (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotContent result ->
            case result of
                Err _ ->
                    ( { model | content = Failure }, Cmd.none )

                Ok content ->
                    ( { model | content = Success content }, Cmd.none )


view : Model -> Skelton.Details msg
view model =
    { title = model.title
    , header = []
    , warning = Skelton.NoProblems
    , attrs = []
    , kids = [ viewContent model.title model.content ]
    }


viewContent : String -> Content -> Html msg
viewContent title content =
    case content of
        Failure ->
            text ""

        Loading ->
            h1 [ style "max-width" "600px" ] [ text title ]

        Success help ->
            Markdown.toHtml [ style "max-width" "600px" ] help
