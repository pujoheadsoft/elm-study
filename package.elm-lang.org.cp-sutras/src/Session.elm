module Session exposing (..)

import Dict
import Elm.Docs as Docs
import Elm.Project as Outline
import Elm.Version as V
import Http
import Json.Decode as Decode
import Page.Search.Entry as Entry
import Release
import Url.Builder as Url
import Utils.OneOrMore exposing (OneOrMore(..))


type alias Data =
    { entries : Maybe (List Entry.Entry)
    , releases : Dict.Dict String (OneOrMore Release.Release)
    , readmes : Dict.Dict String String
    , docs : Dict.Dict String (List Docs.Module)
    , outlines : Dict.Dict String Outline.PackageInfo
    }


empty : Data
empty =
    Data Nothing Dict.empty Dict.empty Dict.empty Dict.empty


getEntries : Data -> Maybe (List Entry.Entry)
getEntries data =
    data.entries


addEntries : List Entry.Entry -> Data -> Data
addEntries entries data =
    { data | entries = Just entries }


toPkgKey : String -> String -> String
toPkgKey author project =
    author ++ "/" ++ project


getReleases : Data -> String -> String -> Maybe (OneOrMore Release.Release)
getReleases data author project =
    Dict.get (toPkgKey author project) data.releases


addReleases : String -> String -> OneOrMore Release.Release -> Data -> Data
addReleases author project releases data =
    let
        newReleases =
            Dict.insert (toPkgKey author project) releases data.releases
    in
    { data | releases = newReleases }



{-
   fetchReleases : String -> String -> Cmd GotReleasesMsg
   fetchReleases author project =
       Http.get
           {url = Url.absolute ["packages", author, project, "releases.json"] []
           , expect = Http.expectJson GotReleases Release.decoder}

   type GotReleasesMsg
       = GotReleases (Result Http.Error (OneOrMore Release.Release))
-}


type alias ReleaseResult =
    Result Http.Error (OneOrMore Release.Release)


fetchReleases : String -> String -> (ReleaseResult -> msg) -> Cmd msg
fetchReleases author project toMessage =
    Http.get
        { url = Url.relative [ "https://package.elm-lang.org/packages", author, project, "releases.json" ] []
        , expect = Http.expectJson toMessage Release.decoder
        }



{-
   expectJson : (Result Error a -> msg) -> Decode.Decoder a -> Expect msg
   引数1 (Result Error a -> msg), 引数2  Decode.Decoder, 戻り値 Expect msg
   Resultの引数1はエラーのとき返る型、引数2はOKのとき返る型。この場合は任意の型aを表す。
   msgはExpectの任意の型。つまりここのResultのmsgとExpectのmsgは同じ型でなければならない
   またOKのときに返る型aと、Decoderの型aは同じ型である必要がある

   type GotReleasesMsg
       = GotReleases (Result Http.Error (OneOrMore Release.Release))

   GotReleasesは関数。(Result Http.Error (OneOrMore Release.Release))を引数にとってGotReleasesMsgを生成する関数。
   引数は (Result.Ok Releaseのインスタンス) か (Result.Err Http.Errorのいずれか例えばHttp.Timeoutとか)という形でのみ受け取れる

-}


toVsnKey : String -> String -> V.Version -> String
toVsnKey author project version =
    author ++ "/" ++ project ++ "@" ++ V.toString version


getReadme : Data -> String -> String -> V.Version -> Maybe String
getReadme data author project version =
    Dict.get (toVsnKey author project version) data.readmes


addReadme : String -> String -> V.Version -> String -> Data -> Data
addReadme author project version readme data =
    let
        newReadmes =
            Dict.insert (toVsnKey author project version) readme data.readmes
    in
    { data | readmes = newReadmes }


type alias StringResult =
    Result Http.Error String


fetchReadme : String -> String -> V.Version -> (StringResult -> msg) -> Cmd msg
fetchReadme author project version toMessage =
    Http.get
        { url = Url.relative [ "https://package.elm-lang.org/packages", author, project, V.toString version, "README.md" ] []
        , expect = Http.expectString toMessage
        }


getDocs : Data -> String -> String -> V.Version -> Maybe (List Docs.Module)
getDocs data author project version =
    Dict.get (toVsnKey author project version) data.docs


addDocs : String -> String -> V.Version -> List Docs.Module -> Data -> Data
addDocs author project version readme data =
    let
        newDocs =
            Dict.insert (toVsnKey author project version) readme data.docs
    in
    { data | docs = newDocs }


type alias DocsResult =
    Result Http.Error (List Docs.Module)


fetchDocs : String -> String -> V.Version -> (DocsResult -> msg) -> Cmd msg
fetchDocs author project version toMessage =
    Http.get
        { url = Url.relative [ "https://package.elm-lang.org/packages", author, project, V.toString version, "docs.json" ] []
        , expect = Http.expectJson toMessage (Decode.list Docs.decoder)
        }


getOutline : Data -> String -> String -> V.Version -> Maybe Outline.PackageInfo
getOutline data author project version =
    Dict.get (toVsnKey author project version) data.outlines


addOutline : String -> String -> V.Version -> Outline.PackageInfo -> Data -> Data
addOutline author project version readme data =
    let
        newOutlines =
            Dict.insert (toVsnKey author project version) readme data.outlines
    in
    { data | outlines = newOutlines }


type alias OutlineResult =
    Result Http.Error Outline.PackageInfo


fetchOutline : String -> String -> V.Version -> (OutlineResult -> msg) -> Cmd msg
fetchOutline author project version toMessage =
    Http.get
        { url = Url.relative [ "https://package.elm-lang.org/packages", author, project, V.toString version, "elm.json" ] []
        , expect = Http.expectJson toMessage outlineDecoder
        }


outlineDecoder : Decode.Decoder Outline.PackageInfo
outlineDecoder =
    Outline.decoder |> Decode.andThen getPkgOutline


getPkgOutline : Outline.Project -> Decode.Decoder Outline.PackageInfo
getPkgOutline outline =
    case outline of
        Outline.Application _ ->
            Decode.fail "Unexpected application"

        Outline.Package info ->
            Decode.succeed info
