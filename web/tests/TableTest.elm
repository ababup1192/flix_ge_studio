module TableTest exposing (suite)

{-| テーブルビューの純ロジック。
並べ替え(数値/文字列)・絞り込み・「表示順をどう変えても rowId が元の指し先
(catalog id / list の元添字)を保つ」ことを固定する。
-}

import Expect
import Json.Decode as D
import Json.Encode as E
import Schema
import Table exposing (RowId(..), SortDir(..))
import Test exposing (Test, describe, test)


schemaFixture : Schema.Schema
schemaFixture =
    """
{
  "sections": {
    "routes": {
      "kind": "catalog",
      "fields": {
        "type":  { "type": {"enum": ["straight", "sine"]}, "label": "動き", "order": 1 },
        "speed": { "type": "float", "order": 2 }
      }
    },
    "spawns": {
      "kind": "list",
      "fields": {
        "atX":  { "type": "int", "label": "出現位置", "order": 1 },
        "kind": { "type": "text", "order": 2 },
        "pos":  { "type": "vec2", "order": 3 }
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
  "routes": {
    "wavy": { "type": "sine", "speed": 85 },
    "slow": { "type": "straight", "speed": 60 }
  },
  "spawns": [
    { "atX": 400, "kind": "turret", "pos": [1, 2] },
    { "atX": 200, "kind": "popcorn" },
    { "atX": 300, "kind": "dome" }
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
        |> Maybe.withDefault { kind = Schema.RecordKind, label = Nothing, help = Nothing, group = Nothing, widget = Nothing, fields = [] }


spawnRows : List Table.TableRow
spawnRows =
    Table.rows "spawns" (sectionOf "spawns") docFixture


idTexts : List Table.TableRow -> List String
idTexts =
    List.map .idText


suite : Test
suite =
    describe "Table — 表ビューの純ロジック"
        [ test "columns — order 順・label 無しはフィールド名" <|
            \_ ->
                Table.columns (sectionOf "spawns")
                    |> Expect.equal
                        [ { name = "atX", label = "出現位置", numeric = True }
                        , { name = "kind", label = "kind", numeric = False }
                        , { name = "pos", label = "pos", numeric = False }
                        ]
        , test "rows — 行に中身の 1 行見出し(最初の空でないスカラ)が付く" <|
            \_ ->
                spawnRows
                    |> List.map (\r -> ( r.idText, r.summary ))
                    |> Expect.equal
                        [ ( "#0", "400" )
                        , ( "#1", "200" )
                        , ( "#2", "300" )
                        ]
        , test "rows — セル値は文字列化・入れ子は「…」・無いキーは空" <|
            \_ ->
                spawnRows
                    |> List.map (\r -> ( r.idText, List.map .text r.cells ))
                    |> Expect.equal
                        [ ( "#0", [ "400", "turret", "…" ] )
                        , ( "#1", [ "200", "popcorn", "" ] )
                        , ( "#2", [ "300", "dome", "" ] )
                        ]
        , test "rows — catalog は id(文書順)が先頭に付く" <|
            \_ ->
                Table.rows "routes" (sectionOf "routes") docFixture
                    |> List.map (\r -> ( r.rowId, r.idText ))
                    |> Expect.equal [ ( ById "wavy", "wavy" ), ( ById "slow", "slow" ) ]
        , test "sortRows — 数値列(atX)は数値で比較(文字列比較なら 200 < 300 < 400 にならない)" <|
            \_ ->
                ( Table.sortRows (Just { column = "atX", dir = Asc }) spawnRows |> idTexts
                , Table.sortRows (Just { column = "atX", dir = Desc }) spawnRows |> idTexts
                )
                    |> Expect.equal ( [ "#1", "#2", "#0" ], [ "#0", "#2", "#1" ] )
        , test "sortRows — 文字列列(kind)は辞書順" <|
            \_ ->
                Table.sortRows (Just { column = "kind", dir = Asc }) spawnRows
                    |> idTexts
                    |> Expect.equal [ "#2", "#1", "#0" ]
        , test "sortRows — id 列は list なら元の並び(数値)として並ぶ" <|
            \_ ->
                Table.sortRows (Just { column = Table.idColumn, dir = Desc }) spawnRows
                    |> idTexts
                    |> Expect.equal [ "#2", "#1", "#0" ]
        , test "filterRows — 全セル値への部分一致・空欄は全件" <|
            \_ ->
                ( Table.filterRows "tur" spawnRows |> idTexts
                , Table.filterRows "" spawnRows |> idTexts
                )
                    |> Expect.equal ( [ "#0" ], [ "#0", "#1", "#2" ] )
        , test "filterRows — catalog は id にも一致する" <|
            \_ ->
                Table.rows "routes" (sectionOf "routes") docFixture
                    |> Table.filterRows "wav"
                    |> idTexts
                    |> Expect.equal [ "wavy" ]
        , test "写像 — 並べ替え+絞り込み後も rowId は元配列の添字を指す(書き戻し先がずれない)" <|
            \_ ->
                spawnRows
                    |> Table.sortRows (Just { column = "atX", dir = Asc })
                    |> Table.filterRows "o"
                    |> List.map .rowId
                    |> Expect.equal [ ByIdx 1, ByIdx 2 ]
        ]
