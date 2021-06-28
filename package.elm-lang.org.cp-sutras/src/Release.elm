module Release exposing (..)

import Elm.Version as V
import Json.Decode as D
import Time
import Utils.OneOrMore exposing (OneOrMore(..))


type alias Release =
    { version : V.Version
    , time : Int
    }


getLatestVersion : OneOrMore Release -> V.Version
getLatestVersion (OneOrMore r rs) =
    getLatestVersionHelp rs r


getLatestVersionHelp : List Release -> Release -> V.Version
getLatestVersionHelp releases maxRelease =
    case releases of
        [] ->
            -- 再帰の終了
            maxRelease.version

        release :: otherReleases ->
            -- 再帰 <|は逆のパイプ(つまりifの結果がotherReleasesの次の引数となる)
            getLatestVersionHelp otherReleases <|
                if release.time > maxRelease.time then
                    release

                else
                    maxRelease


getTime : V.Version -> OneOrMore Release -> Maybe Time.Posix
getTime version (OneOrMore r rs) =
    getTimeHelp version (r :: rs)


getTimeHelp : V.Version -> List Release -> Maybe Time.Posix
getTimeHelp version releases =
    case releases of
        r :: rs ->
            if r.version == version then
                Just (Time.millisToPosix (r.time * 1000))

            else
                getTimeHelp version rs

        [] ->
            Nothing


decoder : D.Decoder (OneOrMore Release)
decoder =
    D.keyValuePairs D.int
        |> D.andThen (decoderHelp [])


decoderHelp : List Release -> List ( String, Int ) -> D.Decoder (OneOrMore Release)
decoderHelp revReleases pairs =
    case pairs of
        [] ->
            case List.reverse revReleases of
                [] ->
                    D.fail "Expecting at least one release!"

                r :: rs ->
                    D.succeed (OneOrMore r rs)

        ( str, time ) :: otherPairs ->
            case V.fromString str of
                Nothing ->
                    D.fail <| "Failed `" ++ str ++ "` must be a valid version, like 3.1.4"

                Just vsn ->
                    decoderHelp (Release vsn time :: revReleases) otherPairs
