module OneOfTable exposing (Row, filterRows, rows)

{-| oneOf セクション(カットの表 等)専用の 3 列(#/カット/内容)の行モデル。

キーごとに 1 列 + 見出しがスキーマの長文、では 1 カット = 1 キーの表が横スクロール
だらけになる。ここでは「勝っているキー」(SchemaForm.oneOf と同じ規則: 宣言順
[order 昇順・未指定はフィールド定義順] で最初に行に存在するキー)を「カット」
1 列にまとめ、他の値は「内容」列へ値の型から汎用に組んだ要約として畳む。

Html を作らない(EntryTable が描画を持つ)。

-}

import Doc
import Json.Decode as D
import Json.Encode as E
import Schema
import Table


type alias Row =
    { rowId : Table.RowId
    , idText : String

    -- 勝っているキーの名前(候補が 1 つも無ければ "?")
    , kind : String

    -- スキーマの label(セルの title = hover 用)。無ければ kind と同じ
    , kindTitle : String

    -- 内容の要約(勝っているキーの値 + 補助キーのバッジ)
    , summary : String
    }


{-| スキーマに「アクションキーか補助キーか」の構造化された宣言は無いので、
この一覧で見分ける(条件行 only/unless・待たせない noWait・量だけ添える
pan/secs/run/back の類)。ここに無いキーは誤って除くよりアクション候補に
残す方が実害が小さい(勝ちキーが消えて見えなくなる方がまずい)。
-}
addedKeyNames : List String
addedKeyNames =
    [ "only", "unless", "noWait", "pan", "secs", "run", "back" ]


rows : String -> Schema.Section -> D.Value -> List Row
rows key section doc =
    let
        ordered =
            section.fields
                |> List.sortBy (\( _, field ) -> Maybe.withDefault orderLast field.order)

        actionCandidates =
            ordered |> List.filter (\( name, _ ) -> not (List.member name addedKeyNames))
    in
    Doc.list key doc
        |> List.indexedMap
            (\i entry -> buildRow ordered actionCandidates (Table.ByIdx i) ("#" ++ String.fromInt i) entry)


orderLast : Int
orderLast =
    2 ^ 30


buildRow : List ( String, Schema.Field ) -> List ( String, Schema.Field ) -> Table.RowId -> String -> D.Value -> Row
buildRow ordered actionCandidates rowId idText entry =
    let
        present name =
            case D.decodeValue (D.field name D.value) entry of
                Ok _ ->
                    True

                Err _ ->
                    False

        chosen =
            actionCandidates |> List.filter (\( name, _ ) -> present name) |> List.head

        mainPart =
            case chosen |> Maybe.andThen (\( name, _ ) -> valueOf name entry) of
                Just v ->
                    mainSummary v

                Nothing ->
                    ""

        chosenName =
            chosen |> Maybe.map Tuple.first

        badges =
            ordered
                |> List.filter (\( name, _ ) -> Just name /= chosenName && present name)
                |> List.filterMap
                    (\( name, _ ) ->
                        valueOf name entry |> Maybe.andThen (extraBadge name)
                    )
    in
    { rowId = rowId
    , idText = idText
    , kind = chosenName |> Maybe.withDefault "?"
    , kindTitle =
        chosen
            |> Maybe.andThen (\( name, field ) -> field.label |> Maybe.map (\l -> name ++ ": " ++ l))
            |> Maybe.withDefault (chosenName |> Maybe.withDefault "この行はアクションキーを持ちません")
    , summary = mainPart :: badges |> List.filter ((/=) "") |> String.join " "
    }


valueOf : String -> D.Value -> Maybe D.Value
valueOf name entry =
    D.decodeValue (D.field name D.value) entry |> Result.toMaybe


{-| 勝っているキーの値の要約。型で汎用に組む:
{x,y} は "(x, y)"、他のキーも持つ物({room,x,y,facing} 等)は元の並びのまま
"room (x, y) facing" のように連ねる。文字列・文字列列は先頭を短く、
数値はそのまま、bool はカット名(呼び名)が既に言っているので空。
-}
mainSummary : D.Value -> String
mainSummary v =
    case D.decodeValue (D.keyValuePairs D.value) v of
        Ok pairs ->
            objectSummary pairs

        Err _ ->
            case D.decodeValue (D.list D.string) v of
                Ok items ->
                    truncated (String.join " / " items)

                Err _ ->
                    case D.decodeValue D.string v of
                        Ok s ->
                            truncated s

                        Err _ ->
                            case D.decodeValue D.float v of
                                Ok n ->
                                    numberText n

                                Err _ ->
                                    case D.decodeValue D.bool v of
                                        Ok _ ->
                                            ""

                                        Err _ ->
                                            "…"


{-| {x,y} を 1 つの "(x, y)" にまとめ、他のキーは元の並びの位置のまま
素朴な値のテキストで挟む("room (x, y) facing" のように)。
-}
objectSummary : List ( String, D.Value ) -> String
objectSummary pairs =
    let
        xy =
            case ( lookup "x" pairs, lookup "y" pairs ) of
                ( Just xv, Just yv ) ->
                    Just ("(" ++ scalarText xv ++ ", " ++ scalarText yv ++ ")")

                _ ->
                    Nothing
    in
    pairs
        |> List.filter (\( k, _ ) -> k /= "y" && not (String.startsWith "//" k))
        |> List.filterMap
            (\( k, v ) ->
                if k == "x" then
                    xy

                else
                    Just (scalarText v)
            )
        |> String.join " "


lookup : String -> List ( String, D.Value ) -> Maybe D.Value
lookup key pairs =
    pairs |> List.filter (\( k, _ ) -> k == key) |> List.head |> Maybe.map Tuple.second


{-| 補助キー 1 つを「内容」列のバッジにする。only/unless/noWait は読みやすい定型文、
それ以外は bool を [名前]、数値・文字列は "名前 値" の汎用形。
-}
extraBadge : String -> D.Value -> Maybe String
extraBadge name v =
    case name of
        "only" ->
            D.decodeValue D.string v |> Result.toMaybe |> Maybe.map (\s -> "[" ++ s ++ " のみ]")

        "unless" ->
            D.decodeValue D.string v |> Result.toMaybe |> Maybe.map (\s -> "[" ++ s ++ " なら黙る]")

        "noWait" ->
            case D.decodeValue D.bool v of
                Ok True ->
                    Just "[待たない]"

                _ ->
                    Nothing

        _ ->
            case D.decodeValue D.bool v of
                Ok True ->
                    Just ("[" ++ name ++ "]")

                Ok False ->
                    Nothing

                Err _ ->
                    case D.decodeValue D.float v of
                        Ok n ->
                            Just (name ++ " " ++ numberText n)

                        Err _ ->
                            case D.decodeValue D.string v of
                                Ok s ->
                                    Just (name ++ " " ++ truncated s)

                                Err _ ->
                                    Nothing


scalarText : D.Value -> String
scalarText v =
    case D.decodeValue D.string v of
        Ok s ->
            s

        Err _ ->
            case D.decodeValue D.float v of
                Ok n ->
                    numberText n

                Err _ ->
                    case D.decodeValue D.bool v of
                        Ok b ->
                            if b then
                                "true"

                            else
                                "false"

                        Err _ ->
                            E.encode 0 v


numberText : Float -> String
numberText n =
    if n == toFloat (round n) then
        String.fromInt (round n)

    else
        String.fromFloat n


{-| 判読が目的の要約なので厳密な省略記号でなくてよい — 約 20 文字で切る。 -}
truncated : String -> String
truncated s =
    if String.length s > 20 then
        String.left 20 s ++ "…"

    else
        s


{-| id・カット名・内容の要約への部分一致(大文字小文字は区別しない)。 -}
filterRows : String -> List Row -> List Row
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
                        || String.contains q (String.toLower r.kind)
                        || String.contains q (String.toLower r.summary)
                )
