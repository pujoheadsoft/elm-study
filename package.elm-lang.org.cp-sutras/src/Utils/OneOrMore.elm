module Utils.OneOrMore exposing (..)

import Json.Decode as Decode

type OneOrMore a =
    OneOrMore a (List a)

head : OneOrMore a -> a
head (OneOrMore x _) = x

tail : OneOrMore a -> List a
tail (OneOrMore _ xs) = xs

append : OneOrMore a -> OneOrMore a -> OneOrMore a
append (OneOrMore x xs) (OneOrMore y ys) =
    OneOrMore x (xs ++ y :: ys)
-- 1つ目の頭と 1つ目の残り + 2つ目の頭 と 2つ目の残りを結合

toList : OneOrMore a -> List a
toList (OneOrMore x xs) = x :: xs

-- MAPPING

map : (a -> b) -> OneOrMore a -> OneOrMore b
map func (OneOrMore x xs) =
    OneOrMore (func x) (List.map func xs)

-- DECODER

decoder : Decode.Decoder a -> Decode.Decoder (OneOrMore a)
decoder entryDecoder =
    Decode.list entryDecoder
        |> Decode.andThen checkList

checkList : List a -> Decode.Decoder (OneOrMore a)
checkList list =
    case list of
        [] ->
            Decode.fail "An array with one or more elements."
        
        x :: xs ->
            Decode.succeed (OneOrMore x xs)