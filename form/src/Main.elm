module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { input : String
    , memos : List String
    }


type Msg
    = Input String
    | Submit

{--
  カスタム型の定義
  InputやSubmitはMsg型がとりえる「値」(型そのものではない)
  Submitはこれ自身Msgとみなせる
  が、InputはStringと一緒に使われることではじめてMsgとなる
  つまりInput単体ではMsgにはならない。
  何になるかというとStringを受け取るとMsgになる「関数」である。
  (Inputは String -> Msg)
  これをカスタム型のコンストラクタという。
  そしてカスタム型のコンストラクタは上記のとおり「関数」である。
--}

init : Model
init =
    { input = ""
    , memos = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Submit ->
            { model
                | input = ""
                , memos = model.input :: model.memos
            }


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button
                [ disabled (String.length model.input < 1) ]
                [ text "Submit" ]
            ]
        , ul [] (List.map viewMemo model.memos)
        ]


viewMemo : String -> Html Msg
viewMemo memo =
    li [] [ text memo ]
