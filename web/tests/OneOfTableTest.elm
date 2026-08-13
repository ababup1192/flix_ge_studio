module OneOfTableTest exposing (suite)

{-| oneOf セクション(カットの表 等)専用の #/カット/内容 の行モデルの純ロジック。
「勝っているキー」の優先順(宣言順で最初に存在する物・補助キーは候補から外す)と、
値の型から汎用に組む要約(object の {x,y}・文字列の省略・補助キーのバッジ)を固定する。
-}

import Expect
import Json.Decode as D
import Json.Encode as E
import OneOfTable
import Schema
import Table exposing (RowId(..))
import Test exposing (Test, describe, test)


schemaFixture : Schema.Schema
schemaFixture =
    """
{
  "sections": {
    "cuts": {
      "kind": "list",
      "oneOf": true,
      "fields": {
        "wait":   { "type": "float", "label": "待つ(秒。長い説明がここに入っても見出しには出ない)", "order": 1 },
        "walkTo": { "type": "object", "label": "歩く先のマス", "order": 2 },
        "jump":   { "type": "object", "label": "別の部屋へ移す", "order": 3 },
        "say":    { "type": {"list": "text"}, "label": "言葉", "order": 4 },
        "lookBack": { "type": "bool", "label": "カメラを戻す", "order": 5 },
        "run":    { "type": "bool", "label": "走る(補助キー)", "order": 6 },
        "noWait": { "type": "bool", "label": "待たない(補助キー)", "order": 7 },
        "pan":    { "type": "float", "label": "見回す速さ(補助キー)", "order": 8 },
        "only":   { "type": "text", "label": "持っている時だけ", "order": 9 },
        "unless": { "type": "text", "label": "持っている時は黙る", "order": 10 }
      }
    }
  }
}
"""
        |> Schema.decodeString
        |> Result.withDefault { version = Nothing, sections = [] }


docFixture : D.Value
docFixture =
    """
{
  "cuts": [
    { "wait": 1.5 },
    { "walkTo": { "x": 35, "y": 4 }, "run": true, "noWait": true },
    { "say": ["……だれか いるの?"], "only": "torch" },
    { "jump": { "room": "hall2", "x": 3, "y": 5, "facing": "down" } },
    { "unless": "torch" },
    { "lookBack": true, "pan": 1.6 }
  ]
}
"""
        |> D.decodeString D.value
        |> Result.withDefault E.null


sectionOf : String -> Schema.Section
sectionOf key =
    schemaFixture.sections
        |> List.filter (\( k, _ ) -> k == key)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault { kind = Schema.RecordKind, label = Nothing, help = Nothing, group = Nothing, widget = Nothing, oneOf = False, fields = [] }


cutRows : List OneOfTable.Row
cutRows =
    OneOfTable.rows "cuts" (sectionOf "cuts") docFixture


suite : Test
suite =
    describe "OneOfTable — oneOf セクションの #/カット/内容 の純ロジック"
        [ test "rows — 勝っているキーは宣言順で最初に存在する物(補助キーは候補から外す)" <|
            \_ ->
                cutRows
                    |> List.map .kind
                    |> Expect.equal [ "wait", "walkTo", "say", "jump", "?", "lookBack" ]
        , test "rows — rowId は元配列の添字(書き戻し先がずれない)" <|
            \_ ->
                cutRows
                    |> List.map .rowId
                    |> Expect.equal [ ByIdx 0, ByIdx 1, ByIdx 2, ByIdx 3, ByIdx 4, ByIdx 5 ]
        , test "rows — 数値はそのまま" <|
            \_ ->
                cutRows
                    |> List.map .summary
                    |> List.head
                    |> Expect.equal (Just "1.5")
        , test "rows — {x,y} は (x, y)。補助キー(run/noWait)はバッジになって続く" <|
            \_ ->
                cutRows
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map .summary
                    |> Expect.equal (Just "(35, 4) [run] [待たない]")
        , test "rows — 文字列の列は先頭を短く。only は「◯◯のみ」の定型バッジ" <|
            \_ ->
                cutRows
                    |> List.drop 2
                    |> List.head
                    |> Maybe.map .summary
                    |> Expect.equal (Just "……だれか いるの? [torch のみ]")
        , test "rows — {room,x,y,facing} は room (x, y) facing の並びのまま" <|
            \_ ->
                cutRows
                    |> List.drop 3
                    |> List.head
                    |> Maybe.map .summary
                    |> Expect.equal (Just "hall2 (3, 5) down")
        , test "rows — アクションキーが無い行はカット '?'。unless は「◯◯なら黙る」の定型バッジ" <|
            \_ ->
                cutRows
                    |> List.drop 4
                    |> List.head
                    |> Expect.equal
                        (Just
                            { rowId = ByIdx 4
                            , idText = "#4"
                            , kind = "?"
                            , kindTitle = "この行はアクションキーを持ちません"
                            , summary = "[torch なら黙る]"
                            }
                        )
        , test "rows — bool の勝ちキーは内容が空(カット名が既に言っている)。補助キー pan は「pan 値」" <|
            \_ ->
                cutRows
                    |> List.drop 5
                    |> List.head
                    |> Maybe.map .summary
                    |> Expect.equal (Just "pan 1.6")
        , test "rows — カット列の title はスキーマの label(長文は見出しでなくここへ)" <|
            \_ ->
                cutRows
                    |> List.head
                    |> Maybe.map .kindTitle
                    |> Expect.equal (Just "wait: 待つ(秒。長い説明がここに入っても見出しには出ない)")
        , test "filterRows — カット名にも内容の要約にも部分一致する" <|
            \_ ->
                ( OneOfTable.filterRows "walkto" cutRows |> List.map .idText
                , OneOfTable.filterRows "torch" cutRows |> List.map .idText
                , OneOfTable.filterRows "" cutRows |> List.length
                )
                    |> Expect.equal ( [ "#1" ], [ "#2", "#4" ], 6 )
        ]
