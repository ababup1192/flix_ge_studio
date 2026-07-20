module RefsTest exposing (suite)

{-| 逆参照インデックス(usages)と id 改名の編集列導出(renameEdits)・拒否理由
(renameProblem)。fixture は level.json 方言の縮小版 — routes catalog を
spawns(list)の ref・decor(list)の list-ref・meta(record)の ref から使う。
-}

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Refs exposing (DocEdit(..), Entry(..), PathSeg(..))
import Schema
import Test exposing (Test, describe, test)


schemaText : String
schemaText =
    """
{
  "sections": {
    "meta": {
      "kind": "record",
      "fields": {
        "bossRoute": { "type": {"ref": "routes"} }
      }
    },
    "routes": {
      "kind": "catalog",
      "fields": {
        "speed": { "type": "float", "required": true }
      }
    },
    "spawns": {
      "kind": "list",
      "fields": {
        "atX":   { "type": "int" },
        "route": { "type": {"ref": "routes"} }
      }
    },
    "decor": {
      "kind": "list",
      "fields": {
        "cycle": { "type": {"list": {"ref": "routes"}} }
      }
    }
  }
}
"""


docText : String
docText =
    """
{
  "meta": { "bossRoute": "wavy" },
  "routes": {
    "slow":   { "speed": 60 },
    "wavy":   { "speed": 85 },
    "lonely": { "speed": 10 }
  },
  "spawns": [
    { "atX": 200, "route": "slow" },
    { "atX": 400, "route": "wavy" },
    { "atX": 650, "route": "wavy" }
  ],
  "decor": [
    { "cycle": ["slow", "wavy"] }
  ]
}
"""


schemaFixture : Schema.Schema
schemaFixture =
    Schema.decodeString schemaText
        |> Result.withDefault { version = Nothing, sections = [] }


docFixture : D.Value
docFixture =
    D.decodeString D.value docText
        |> Result.withDefault E.null


usagesOf : String -> Maybe (List Refs.UsageSite)
usagesOf key =
    Refs.usages schemaFixture docFixture
        |> Dict.get key


renameReq : String -> String -> { sectionKey : String, oldId : String, newId : String }
renameReq oldId newId =
    { sectionKey = "routes", oldId = oldId, newId = newId }


suite : Test
suite =
    describe "Refs — 逆参照と id 改名の導出"
        [ describe "usages(逆参照インデックス)"
            [ test "fixture 前提 — スキーマ 4 セクションが読めている(空スキーマの 0 件と混同しない)" <|
                \_ ->
                    List.map Tuple.first schemaFixture.sections
                        |> Expect.equal [ "meta", "routes", "spawns", "decor" ]
            , test "単値の ref — wavy は meta・spawns#1・spawns#2・decor の 4 箇所(文書の順)" <|
                \_ ->
                    usagesOf "routes/wavy"
                        |> Expect.equal
                            (Just
                                [ { sectionKey = "meta", entry = Nothing, fieldName = "bossRoute", elem = Nothing }
                                , { sectionKey = "spawns", entry = Just (AtIndex 1), fieldName = "route", elem = Nothing }
                                , { sectionKey = "spawns", entry = Just (AtIndex 2), fieldName = "route", elem = Nothing }
                                , { sectionKey = "decor", entry = Just (AtIndex 0), fieldName = "cycle", elem = Just 1 }
                                ]
                            )
            , test "list の ref — cycle の要素は elem(何番目か)付きで数える" <|
                \_ ->
                    usagesOf "routes/slow"
                        |> Expect.equal
                            (Just
                                [ { sectionKey = "spawns", entry = Just (AtIndex 0), fieldName = "route", elem = Nothing }
                                , { sectionKey = "decor", entry = Just (AtIndex 0), fieldName = "cycle", elem = Just 0 }
                                ]
                            )
            , test "0 件 — 使われていない id はキーごと現れない" <|
                \_ ->
                    usagesOf "routes/lonely"
                        |> Expect.equal Nothing
            ]
        , describe "renameEdits(改名 1 バッチの編集列)"
            [ test "先頭がキー改名、続いて全参照の値書き換え(パスは具体値で pin)" <|
                \_ ->
                    Refs.renameEdits schemaFixture docFixture (renameReq "wavy" "wave")
                        |> Expect.equal
                            [ RenameKey [ Key "routes", Key "wavy" ] "wave"
                            , SetString [ Key "meta", Key "bossRoute" ] "wave"
                            , SetString [ Key "spawns", Idx 1, Key "route" ] "wave"
                            , SetString [ Key "spawns", Idx 2, Key "route" ] "wave"
                            , SetString [ Key "decor", Idx 0, Key "cycle", Idx 1 ] "wave"
                            ]
            , test "参照 0 件の id はキー改名 1 件だけ" <|
                \_ ->
                    Refs.renameEdits schemaFixture docFixture (renameReq "lonely" "alone")
                        |> Expect.equal [ RenameKey [ Key "routes", Key "lonely" ] "alone" ]
            ]
        , describe "renameProblem(確定を拒む理由)"
            [ test "空(空白だけも)は拒む" <|
                \_ ->
                    Refs.renameProblem docFixture (renameReq "wavy" "  ")
                        |> Expect.equal (Just "id が空です")
            , test "変更なしは拒む" <|
                \_ ->
                    Refs.renameProblem docFixture (renameReq "wavy" "wavy")
                        |> Expect.equal (Just "元の id と同じです")
            , test "既にある id への重複は拒む" <|
                \_ ->
                    Refs.renameProblem docFixture (renameReq "wavy" "slow")
                        |> Expect.equal (Just "\"slow\" は既にあります")
            , test "新しい id なら通る" <|
                \_ ->
                    Refs.renameProblem docFixture (renameReq "wavy" "wave")
                        |> Expect.equal Nothing
            ]
        ]
