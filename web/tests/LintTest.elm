module LintTest exposing (suite)

{-| 問題検出(Lint.check)。flix_ge_shooting の level.json / level.schema.json の実物を
土台に、正常 0 件と 4 種の検出(ぶら下がり参照・未知キー・required 欠落・範囲外)を固定する。
壊した fixture は実物テキストの一意な断片の置換で作る(全体を書き写して二重管理にしない)。
-}

import Expect
import Json.Decode as D
import Json.Encode as E
import Lint exposing (Target(..))
import Schema
import Test exposing (Test, describe, test)


schemaText : String
schemaText =
    """
{
  "//": "level.json の形の宣言(新方言)。",
  "version": 1,
  "sections": {
    "meta": {
      "kind": "record",
      "fields": {
        "scrollSpeed": { "type": "float", "label": "スクロール速度", "order": 1,
                         "min": 0, "max": 200, "widget": "slider", "default": 60 }
      }
    },
    "routes": {
      "kind": "catalog",
      "fields": {
        "type":  { "type": {"enum": ["straight", "sine", "hover"]},
                   "label": "動き", "order": 1, "widget": "segmented", "required": true },
        "speed": { "type": "float", "label": "速さ", "order": 2,
                   "min": 0, "max": 150, "widget": "slider", "required": true },
        "amp":   { "type": "float", "label": "振れ幅", "order": 3,
                   "min": 0, "max": 120, "widget": "slider" },
        "freq":  { "type": "float", "label": "揺れの速さ", "order": 4,
                   "min": 0, "max": 10, "widget": "slider" }
      }
    },
    "spawns": {
      "kind": "list",
      "fields": {
        "atX":   { "type": "int", "label": "出現位置", "order": 1, "required": true,
                   "min": 0, "widget": {"pickOnPreview": {"axis": "x"}} },
        "kind":  { "type": {"enum": ["popcorn", "turret", "dome"]},
                   "label": "敵種", "order": 2, "widget": "segmented", "required": true },
        "y":     { "type": "int", "label": "高さ", "order": 3, "required": true,
                   "min": 0, "max": 240, "widget": "slider" },
        "route": { "type": {"ref": "routes"}, "label": "軌道", "order": 4, "required": true },
        "count": { "type": "int", "label": "連続数", "order": 5,
                   "min": 1, "max": 9, "widget": "spinner", "default": 1 },
        "intervalSec": { "type": "float", "label": "間隔(秒)", "order": 6,
                         "min": 0.1, "max": 2, "default": 0.35 }
      }
    }
  }
}
"""


docText : String
docText =
    """
{
  "meta": { "scrollSpeed": 60 },
  "routes": {
    "slow":  { "type": "straight", "speed": 60 },
    "mid":   { "type": "straight", "speed": 70 },
    "brisk": { "type": "straight", "speed": 75 },
    "fast":  { "type": "straight", "speed": 100 },
    "fixed": { "type": "straight", "speed": 0 },
    "wavy":  { "type": "sine", "speed": 85, "amp": 42, "freq": 3.2 },
    "hover": { "type": "hover", "speed": 60, "amp": 12, "freq": 2.0 }
  },
  "spawns": [
    { "atX": 200,  "kind": "popcorn", "y": 120, "route": "mid" },
    { "atX": 400,  "kind": "popcorn", "y": 60,  "route": "slow" },
    { "atX": 400,  "kind": "popcorn", "y": 180, "route": "slow" },
    { "atX": 650,  "kind": "popcorn", "y": 120, "route": "wavy", "count": 5, "intervalSec": 0.35 },
    { "atX": 800,  "kind": "turret",  "y": 212, "route": "fixed" },
    { "atX": 900,  "kind": "popcorn", "y": 80,  "route": "brisk" },
    { "atX": 1000, "kind": "dome",    "y": 90,  "route": "hover" },
    { "atX": 1100, "kind": "popcorn", "y": 160, "route": "brisk" },
    { "atX": 1250, "kind": "popcorn", "y": 90,  "route": "wavy", "count": 4, "intervalSec": 0.35 },
    { "atX": 1350, "kind": "turret",  "y": 212, "route": "fixed" },
    { "atX": 1450, "kind": "popcorn", "y": 60,  "route": "fast" },
    { "atX": 1450, "kind": "popcorn", "y": 180, "route": "fast" },
    { "atX": 1550, "kind": "dome",    "y": 110, "route": "hover" },
    { "atX": 1650, "kind": "popcorn", "y": 150, "route": "wavy", "count": 5, "intervalSec": 0.35 },
    { "atX": 1800, "kind": "turret",  "y": 212, "route": "fixed" }
  ]
}
"""


schemaFixture : Schema.Schema
schemaFixture =
    Schema.decodeString schemaText
        |> Result.withDefault { version = Nothing, sections = [] }


parseDoc : String -> D.Value
parseDoc text =
    D.decodeString D.value text
        |> Result.withDefault E.null


checkDoc : String -> List Lint.Problem
checkDoc text =
    Lint.check schemaFixture (parseDoc text)


{-| 実物断片の一意置換で壊れ fixture を作る。 -}
broken : String -> String -> List Lint.Problem
broken before after =
    checkDoc (String.replace before after docText)


suite : Test
suite =
    describe "Lint — スキーマと文書の突き合わせ"
        [ test "fixture 前提 — スキーマ 3 セクションが読めている(空スキーマの 0 件と混同しない)" <|
            \_ ->
                List.map Tuple.first schemaFixture.sections
                    |> Expect.equal [ "meta", "routes", "spawns" ]
        , test "実物の level.json は問題 0 件" <|
            \_ ->
                checkDoc docText |> Expect.equal []
        , test "ぶら下がり参照 — route を \"wavvy\" に壊すと spawns #0 で検出" <|
            \_ ->
                broken "\"route\": \"mid\"" "\"route\": \"wavvy\""
                    |> Expect.equal
                        [ { sectionKey = "spawns"
                          , entry = Just (AtIndex 0)
                          , field = Just "route"
                          , message = "spawns #0 の route \"wavvy\" は routes に居ません"
                          }
                        ]
        , test "スキーマに無いキー — 改名の取り残し \"colour\" を検出" <|
            \_ ->
                broken "\"y\": 120, \"route\": \"mid\"" "\"y\": 120, \"colour\": 1, \"route\": \"mid\""
                    |> Expect.equal
                        [ { sectionKey = "spawns"
                          , entry = Just (AtIndex 0)
                          , field = Just "colour"
                          , message = "spawns #0 にスキーマに無いキー \"colour\" があります"
                          }
                        ]
        , test "required 欠落 — kind を消すと検出" <|
            \_ ->
                broken "{ \"atX\": 200,  \"kind\": \"popcorn\"," "{ \"atX\": 200, "
                    |> Expect.equal
                        [ { sectionKey = "spawns"
                          , entry = Just (AtIndex 0)
                          , field = Just "kind"
                          , message = "spawns #0 の kind が書かれていません"
                          }
                        ]
        , test "required 欠落 — default があっても知らせる(文言は控えめ)" <|
            \_ ->
                let
                    miniSchema =
                        Schema.decodeString
                            """
{ "sections": { "spawns": { "kind": "list", "fields": {
    "count": { "type": "int", "required": true, "default": 1 } } } } }
"""
                            |> Result.withDefault { version = Nothing, sections = [] }
                in
                Lint.check miniSchema (parseDoc "{ \"spawns\": [ {} ] }")
                    |> List.map .message
                    |> Expect.equal [ "spawns #0 の count が書かれていません(既定値あり)" ]
        , test "範囲外 — y=400 は 0〜240 の外" <|
            \_ ->
                broken "\"y\": 120, \"route\": \"mid\"" "\"y\": 400, \"route\": \"mid\""
                    |> Expect.equal
                        [ { sectionKey = "spawns"
                          , entry = Just (AtIndex 0)
                          , field = Just "y"
                          , message = "spawns #0 の y は 400 で範囲(0〜240)の外です"
                          }
                        ]
        , test "範囲外 — min だけの atX は「以上」で言う" <|
            \_ ->
                broken "\"atX\": 200" "\"atX\": -5"
                    |> Expect.equal
                        [ { sectionKey = "spawns"
                          , entry = Just (AtIndex 0)
                          , field = Just "atX"
                          , message = "spawns #0 の atX は -5 で範囲(0 以上)の外です"
                          }
                        ]
        , test "範囲外 — record セクション(meta)は entry 無しで指す" <|
            \_ ->
                broken "\"scrollSpeed\": 60" "\"scrollSpeed\": 999"
                    |> Expect.equal
                        [ { sectionKey = "meta"
                          , entry = Nothing
                          , field = Just "scrollSpeed"
                          , message = "meta の scrollSpeed は 999 で範囲(0〜200)の外です"
                          }
                        ]
        , test "横断 — 参照先セクションが別文書に居れば問題にしない(enemies の weapon)" <|
            \_ ->
                Lint.checkAcross
                    [ { resource = "weapons"
                      , path = "assets/weapons.json"
                      , doc = parseDoc "{ \"weapons\": { \"claw\": {} } }"
                      , schema = Nothing
                      }
                    ]
                    crossSchema
                    (parseDoc "{ \"enemies\": { \"slime\": { \"weapon\": \"claw\" } } }")
                    |> Expect.equal []
        , test "横断 — 同一文書にも横断辞書にも居なければ従来どおり検出する" <|
            \_ ->
                Lint.checkAcross
                    [ { resource = "weapons"
                      , path = "assets/weapons.json"
                      , doc = parseDoc "{ \"weapons\": { \"claw\": {} } }"
                      , schema = Nothing
                      }
                    ]
                    crossSchema
                    (parseDoc "{ \"enemies\": { \"slime\": { \"weapon\": \"cursed\" } } }")
                    |> List.map .message
                    |> Expect.equal [ "enemies \"slime\" の weapon \"cursed\" は weapons に居ません" ]
        ]


{-| 横断テスト用: weapon が別文書のセクション(weapons)を指すスキーマ。 -}
crossSchema : Schema.Schema
crossSchema =
    Schema.decodeString
        """
{ "sections": { "enemies": { "kind": "catalog", "fields": {
    "weapon": { "type": {"ref": "weapons"} } } } } }
"""
        |> Result.withDefault { version = Nothing, sections = [] }
