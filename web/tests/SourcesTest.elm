module SourcesTest exposing (suite)

{-| 文書横断の参照解決: 選択肢の 2 段解決(同一文書 → 無ければ横断辞書)と、
他文書からの逆参照(使用箇所)の索引。fixture は flix_ge_shooting の
enemies(weapon が weapons.json を指す)の縮小版。
-}

import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Refs
import Schema
import Sources
import Test exposing (Test, describe, test)


enemiesSchema : Schema.Schema
enemiesSchema =
    """
{
  "sections": {
    "enemies": {
      "kind": "catalog",
      "fields": {
        "name":   { "type": "text", "required": true },
        "weapon": { "type": {"ref": "weapons"}, "required": true }
      }
    }
  }
}
"""
        |> Schema.decodeString
        |> Result.withDefault { version = Nothing, sections = [] }


enemiesDoc : D.Value
enemiesDoc =
    parse
        """
{
  "enemies": {
    "slime": { "name": "スライム", "weapon": "claw" },
    "bat":   { "name": "コウモリ", "weapon": "claw" },
    "golem": { "name": "ゴーレム", "weapon": "cannon" }
  }
}
"""


weaponsDoc : D.Value
weaponsDoc =
    parse
        """
{
  "weapons": {
    "claw":   { "name": "爪" },
    "cannon": { "name": "大砲" }
  }
}
"""


parse : String -> D.Value
parse text =
    D.decodeString D.value text |> Result.withDefault E.null


others : List Sources.SourceDoc
others =
    [ { resource = "weapons", path = "assets/weapons.json", doc = weaponsDoc, schema = Nothing } ]


enemiesAsOther : List Sources.SourceDoc
enemiesAsOther =
    [ { resource = "enemies", path = "assets/enemies.json", doc = enemiesDoc, schema = Just enemiesSchema } ]


suite : Test
suite =
    describe "Sources — 文書横断の参照解決"
        [ describe "refChoices(選択肢の 2 段解決)"
            [ test "同一文書に参照先セクションが無ければ横断辞書から引く(claw / cannon が並ぶ)" <|
                \() ->
                    Sources.refChoices "weapons" enemiesDoc others
                        |> Expect.equal [ "claw", "cannon" ]
            , test "同一文書にエントリがあればそちらが正(横断辞書は見ない)" <|
                \() ->
                    Sources.refChoices "weapons" weaponsDoc enemiesAsOther
                        |> Expect.equal [ "claw", "cannon" ]
            , test "どちらにも無ければ空(候補ゼロはぶら下がりの印)" <|
                \() ->
                    Sources.refChoices "items" enemiesDoc others
                        |> Expect.equal []
            ]
        , describe "externalUsages(他文書からの逆参照)"
            [ test "weapons.json を開いた側から見ると、enemies.json の claw 参照 2 箇所が文書順で数えられる" <|
                \() ->
                    Sources.externalUsages enemiesAsOther
                        |> Dict.get (Refs.usageKey "weapons" "claw")
                        |> Maybe.map (List.map (\u -> ( u.path, u.site.entry )))
                        |> Expect.equal
                            (Just
                                [ ( "assets/enemies.json", Just (Refs.AtKey "slime") )
                                , ( "assets/enemies.json", Just (Refs.AtKey "bat") )
                                ]
                            )
            , test "スキーマの無い他文書は数えない(どの欄が ref か分からない)" <|
                \() ->
                    Sources.externalUsages others
                        |> Dict.isEmpty
                        |> Expect.equal True
            ]
        , describe "refRewriteEdits(他文書へ当てる参照書き換えの編集列)"
            [ test "キー改名は含まず、参照している欄の SetString だけが並ぶ" <|
                \() ->
                    Refs.refRewriteEdits enemiesSchema enemiesDoc { sectionKey = "weapons", oldId = "claw", newId = "talon" }
                        |> Expect.equal
                            [ Refs.SetString [ Refs.Key "enemies", Refs.Key "slime", Refs.Key "weapon" ] "talon"
                            , Refs.SetString [ Refs.Key "enemies", Refs.Key "bat", Refs.Key "weapon" ] "talon"
                            ]
            , test "参照 0 件なら空(そのファイルは触らない)" <|
                \() ->
                    Refs.refRewriteEdits enemiesSchema enemiesDoc { sectionKey = "weapons", oldId = "unused", newId = "x" }
                        |> Expect.equal []
            ]
        ]
