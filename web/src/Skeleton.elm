module Skeleton exposing (bareNameOf, docText, pathFor)

{-| スキーマから「新しいファイルの骨格」を組む(純ロジック)。

方針は 1 つ: **旗は明示する**。省略時既定に頼って空の {} を置くと、開いた人は
「何を書けるのか」をスキーマを読みに行かないと分からない。宣言された欄は
default があれば default、無ければ型ごとの空値で、全部書いた形で作る
(trigger なら on / once が最初から並んでいる状態)。

一覧(catalog / list)は空のまま作る — 中身は 1 件目を「+ 追加」で足す時に、
同じ既定値の規則(EntryOps.newEntry)で入る。ここで 1 件目を勝手に作らないのは、
「空の一覧」と「1 件だけある一覧」を取り違えさせないため。

-}

import EntryOps
import Json.Encode as E
import Schema exposing (Schema, Section, SectionKind(..))


{-| 骨格の本文(2 スペース字下げ・末尾改行つき)。スキーマが無ければ空の
オブジェクトだけ — 何も宣言されていない物に旗は立てられない。
-}
docText : Maybe Schema -> String
docText schema =
    let
        body =
            case schema of
                Just s ->
                    E.object (versionPair s ++ List.map sectionPair s.sections)

                Nothing ->
                    E.object []
    in
    E.encode 2 body ++ "\n"


{-| version はスキーマが版を宣言している時だけ、同じ数字で先頭に書く。 -}
versionPair : Schema -> List ( String, E.Value )
versionPair schema =
    case schema.version of
        Just v ->
            [ ( "version", E.int v ) ]

        Nothing ->
            []


sectionPair : ( String, Section ) -> ( String, E.Value )
sectionPair ( key, section ) =
    ( key, sectionValue section )


sectionValue : Section -> E.Value
sectionValue section =
    case section.kind of
        RecordKind ->
            EntryOps.newEntry section

        -- セクション自体が 1 個の値(light / shader 等)。default があればそれ
        ValueKind field ->
            EntryOps.fieldValue field

        Catalog ->
            E.object []

        ListKind ->
            E.list identity []

        -- フォームにできない kind は空の入れ物も決められない(形を知らない)
        Unsupported _ ->
            E.null


{-| 宣言の pattern と入力名から、新しいファイルのパスを決める。
"*" は名前で埋める(assets/\*.map.json + "cave" → assets/cave.map.json)。
"*" の無い pattern はその 1 本しか置き場が無いので、名前は使わずそのまま。

入力に置き場所や拡張子が混ざっていても(assets/cave.map.json と打っても)
同じ 1 本に落ちる — 置き場と拡張子を決めるのは宣言の側で、名前は名前だけ。

-}
pathFor : String -> String -> String
pathFor pattern name =
    case String.split "*" pattern of
        [ prefix, suffix ] ->
            prefix ++ bareName suffix name ++ suffix

        -- "*" が無い(1 本しか置けない)/ 2 つ以上ある(埋め方を決められない)
        _ ->
            pattern


{-| 入力から名前だけを取り出す。ディレクトリは落とし、宣言側の拡張子を
打ってあれば剥がす。
-}
bareName : String -> String -> String
bareName suffix raw =
    let
        base =
            raw
                |> String.trim
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
    in
    if suffix /= "" && String.endsWith suffix base then
        String.dropRight (String.length suffix) base

    else
        base


{-| 既にあるパスから、pattern の飾り(置き場と拡張子)を外した名前だけ。
複製・改名の入力欄の初期値に使う(全部打ち直させない)。
-}
bareNameOf : String -> String -> String
bareNameOf pattern path =
    case String.split "*" pattern of
        [ prefix, suffix ] ->
            path
                |> dropPrefix prefix
                |> dropSuffix suffix

        _ ->
            path


dropPrefix : String -> String -> String
dropPrefix prefix text =
    if prefix /= "" && String.startsWith prefix text then
        String.dropLeft (String.length prefix) text

    else
        text


dropSuffix : String -> String -> String
dropSuffix suffix text =
    if suffix /= "" && String.endsWith suffix text then
        String.dropRight (String.length suffix) text

    else
        text
