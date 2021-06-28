module Page.Search.Entry exposing (..)

import Elm.Version as V
import Json.Decode as D


type alias Entry =
    { name : String
    , author : String
    , project : String
    , summary : String
    , license : String
    , version : V.Version
    }


search : String -> List Entry -> List Entry
search query entries =
    let
        queryTerms =
            String.words (String.toLower query)

        -- entryを引数としbooleanを返す関数
        matchesAllTerms entry =
            let
                lowerName =
                    String.toLower entry.name

                lowerSummary =
                    String.toLower entry.summary

                -- termを引数としbooleanを返す関数
                -- entryのnameかsummaryのどちらかに含まれたらtrue
                matchesTerm term =
                    String.contains term lowerName
                        || String.contains term lowerSummary
            in
            -- クエリ文字列のリストがすべてentryのnameかsummaryに含まれるか
            List.all matchesTerm queryTerms
    in
    -- クエリ文字列にマッチするentryだけを返す
    List.filter matchesAllTerms entries

-- EntryのJson Decoder
decoder : D.Decoder Entry
decoder =
  D.map4 (\f a b c -> f a b c)
    (D.field "name" (D.andThen splitName D.string))
    (D.field "summary" D.string)
    (D.field "license" D.string)
    (D.field "version" V.decoder)

splitName : String -> D.Decoder (String -> String -> V.Version -> Entry)
splitName name =
  case String.split "/" name of
    [author, project] ->
      D.succeed (Entry name author project)

    _ ->
      D.fail ("Ran into an invalid package name: " ++ name)