module Table exposing
    ( Cell
    , Column
    , RowId(..)
    , SortDir(..)
    , SortState
    , TableRow
    , cell
    , columns
    , filterRows
    , idColumn
    , rows
    , sortRows
    )

{-| catalog / list セクションの表ビュー用の純ロジック(Html を作らない)。

行は並べ替え・絞り込みの前に「論理の指し先」(catalog の id / list の元配列の添字)を
rowId として抱き込む。表示順をどう変えても書き戻し先(docEdit のパス)がずれないのは
この一点による — 表示位置から添字を逆算する方式は取らない。

-}

import Dict
import Doc
import Json.Decode as D
import Json.Encode as E
import Schema
import Summary


type alias Column =
    { name : String
    , label : String

    -- 型が数値(int / float)の列。表では右寄せ+等幅数字で出す
    , numeric : Bool
    }


{-| 先頭の id / 行番号列の内部名。"//" 始まりはスキーマ側でアノテーションとして捨てられる
「予約」領域なので、実在フィールド名と衝突しない。
-}
idColumn : String
idColumn =
    "//id"


type RowId
    = ById String
    | ByIdx Int


type alias Cell =
    { column : String
    , text : String

    -- JSON の数値だった時だけ Just(数値としての並べ替えに使う)。
    -- "60" のような数字めいた文字列は Nothing のまま — 型を勝手に読み替えない
    , numeric : Maybe Float
    }


type alias TableRow =
    { rowId : RowId
    , idText : String

    -- 中身の見当をつけるための 1 行見出し(盤面の行一覧と同じ規則)。
    -- id を持たない list の行で、番号だけの行に何が書いてあるかを言う
    , summary : String
    , cells : List Cell
    }


{-| 列の並びはフォームと同じ order 順(未指定は末尾・同順位は書いた順)。 -}
columns : Schema.Section -> List Column
columns section =
    section.fields
        |> List.sortBy (\( _, field ) -> Maybe.withDefault orderLast field.order)
        |> List.map
            (\( name, field ) ->
                { name = name
                , label = Maybe.withDefault name field.label
                , numeric = field.type_ == Schema.TInt || field.type_ == Schema.TFloat
                }
            )


orderLast : Int
orderLast =
    2 ^ 30


rows : String -> Schema.Section -> D.Value -> List TableRow
rows key section doc =
    let
        cols =
            columns section
    in
    case section.kind of
        Schema.Catalog ->
            let
                dict =
                    Doc.catalog key doc
            in
            Doc.catalogKeys key doc
                |> List.map
                    (\name ->
                        buildRow cols (ById name) name (Dict.get name dict |> Maybe.withDefault E.null)
                    )

        Schema.ListKind ->
            Doc.list key doc
                |> List.indexedMap (\i entry -> buildRow cols (ByIdx i) ("#" ++ String.fromInt i) entry)

        Schema.RecordKind ->
            []

        -- 単一値セクションに表は無い(右のフォーム 1 行が担当)
        Schema.ValueKind _ ->
            []

        -- フォーム未対応 kind に表は作らない(テキスト編集の担当)
        Schema.Unsupported _ ->
            []


buildRow : List Column -> RowId -> String -> D.Value -> TableRow
buildRow cols rowId idText entry =
    { rowId = rowId
    , idText = idText
    , summary = Summary.line entry
    , cells = cols |> List.map (\col -> cell col.name entry)
    }


{-| 値 1 個の 1 行テキスト化。表の外(ダッシュボードの要約)も同じ見せ方を使う。 -}
cell : String -> D.Value -> Cell
cell name entry =
    case D.decodeValue (D.field name D.value) entry of
        Err _ ->
            { column = name, text = "", numeric = Nothing }

        Ok value ->
            case D.decodeValue D.string value of
                Ok s ->
                    { column = name, text = s, numeric = Nothing }

                Err _ ->
                    case D.decodeValue D.float value of
                        Ok n ->
                            { column = name, text = numberText n, numeric = Just n }

                        Err _ ->
                            case D.decodeValue D.bool value of
                                Ok b ->
                                    { column = name
                                    , text =
                                        if b then
                                            "true"

                                        else
                                            "false"
                                    , numeric = Nothing
                                    }

                                Err _ ->
                                    case D.decodeValue (D.null ()) value of
                                        Ok _ ->
                                            { column = name, text = "null", numeric = Nothing }

                                        Err _ ->
                                            -- record / list 値は表では中身を広げない(セルが暴れる)
                                            { column = name, text = "…", numeric = Nothing }


numberText : Float -> String
numberText n =
    if n == toFloat (round n) then
        String.fromInt (round n)

    else
        String.fromFloat n



-- 並べ替え


type SortDir
    = Asc
    | Desc


type alias SortState =
    { column : String
    , dir : SortDir
    }


{-| 空セル以外が全部数値の列だけ数値比較、それ以外は文字列比較。
セルごとに比較方法を変えると順序の全体が壊れる(推移律が崩れる)ので列単位で決める。
空セルは数値列では常に端へ寄る。
-}
sortRows : Maybe SortState -> List TableRow -> List TableRow
sortRows state list_ =
    case state of
        Nothing ->
            list_

        Just st ->
            let
                isNumericColumn =
                    list_
                        |> List.all
                            (\r ->
                                let
                                    ( text_, num ) =
                                        keyOf st.column r
                                in
                                text_ == "" || num /= Nothing
                            )

                compareRows a b =
                    let
                        ( ta, na ) =
                            keyOf st.column a

                        ( tb, nb ) =
                            keyOf st.column b
                    in
                    if isNumericColumn then
                        compare (Maybe.withDefault (1 / 0) na) (Maybe.withDefault (1 / 0) nb)

                    else
                        compare ta tb

                directed a b =
                    case st.dir of
                        Asc ->
                            compareRows a b

                        Desc ->
                            compareRows b a
            in
            List.sortWith directed list_


keyOf : String -> TableRow -> ( String, Maybe Float )
keyOf column row =
    if column == idColumn then
        case row.rowId of
            ById name ->
                ( name, Nothing )

            ByIdx i ->
                ( row.idText, Just (toFloat i) )

    else
        row.cells
            |> List.filter (\c -> c.column == column)
            |> List.head
            |> Maybe.map (\c -> ( c.text, c.numeric ))
            |> Maybe.withDefault ( "", Nothing )



-- 絞り込み


{-| id と全セル値への部分一致(大文字小文字は区別しない)。空欄は全件。 -}
filterRows : String -> List TableRow -> List TableRow
filterRows query list_ =
    let
        q =
            String.toLower (String.trim query)
    in
    if q == "" then
        list_

    else
        list_
            |> List.filter
                (\r ->
                    String.contains q (String.toLower r.idText)
                        || List.any (\c -> String.contains q (String.toLower c.text)) r.cells
                )
