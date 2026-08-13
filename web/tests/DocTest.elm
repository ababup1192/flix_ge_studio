module DocTest exposing (suite)

{-| 文書(パース済み JSON)からのセクション取り出し。
無い/形違いのセクションが空に倒れること(フォームを即死させない)も固定する。
-}

import Dict
import Doc
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)


docFixture : D.Value
docFixture =
    """
{
  "meta": { "scrollSpeed": 60 },
  "routes": {
    "//": "アノテーションは一覧に出さない",
    "slow": { "type": "straight", "speed": 60 },
    "wavy": { "type": "sine", "speed": 85 }
  },
  "spawns": [
    { "atX": 200, "kind": "popcorn" },
    { "atX": 400, "kind": "turret" }
  ]
}
"""
        |> D.decodeString D.value
        |> Result.withDefault E.null


suite : Test
suite =
    describe "Doc — セクション値の取り出し"
        [ test "catalog — エントリ名で引ける Dict になる" <|
            \_ ->
                Doc.catalog "routes" docFixture
                    |> Dict.get "wavy"
                    |> Maybe.andThen (\v -> D.decodeValue (D.field "speed" D.float) v |> Result.toMaybe)
                    |> Expect.equal (Just 85)
        , test "catalogKeys — 文書に書いた順(辞書順にしない)。アノテーションキーは出さない" <|
            \_ ->
                Doc.catalogKeys "routes" docFixture
                    |> Expect.equal [ "slow", "wavy" ]
        , test "list — 並び順のまま取り出せる" <|
            \_ ->
                Doc.list "spawns" docFixture
                    |> List.map (\v -> D.decodeValue (D.field "atX" D.int) v |> Result.toMaybe)
                    |> Expect.equal [ Just 200, Just 400 ]
        , test "record — セクションの値がそのまま取れる" <|
            \_ ->
                Doc.record "meta" docFixture
                    |> D.decodeValue (D.field "scrollSpeed" D.float)
                    |> Expect.equal (Ok 60)
        , test "無いセクションは空に倒れる(catalog=空 Dict・list=空・record=null)" <|
            \_ ->
                ( Dict.isEmpty (Doc.catalog "nope" docFixture)
                , Doc.list "nope" docFixture
                , E.encode 0 (Doc.record "nope" docFixture)
                )
                    |> Expect.equal ( True, [], "null" )
        ]
