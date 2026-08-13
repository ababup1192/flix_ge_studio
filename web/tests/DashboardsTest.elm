module DashboardsTest exposing (suite)

{-| 汎用ダッシュボード(generic)の導出ロジック:
エントリ一覧(名前+肖像)・攻略本ページ(額装 3 点とメーター/%/重み棒)・
ref プレビューの解決とぶら下がり。見せ方は全てスキーマの宣言から決まることを固定する。
-}

import Dashboards
import Expect
import Json.Decode as D
import Json.Encode as E
import Schema
import Test exposing (Test, describe, test)


enemiesText : String
enemiesText =
    """{
  "enemies": {
    "slime": { "name": "スライム", "hp": 10, "speed": 20.0, "weapon": "claw",
               "aggro": 0.35, "drops": { "claw": 70, "cannon": 30 },
               "portrait": "assets/portraits/slime.ui.json",
               "description": "草原にぷるんと湧く。" },
    "golem": { "name": "ゴーレム", "hp": 60, "speed": 8.0, "weapon": "cannon" },
    "ghost": { "name": "ゴースト", "hp": 15, "speed": 40.0, "weapon": "cursed" }
  }
}"""


enemiesSchemaText : String
enemiesSchemaText =
    """{
  "version": 1,
  "sections": {
    "enemies": {
      "kind": "catalog",
      "fields": {
        "name":   { "type": "text", "label": "名前", "order": 1, "required": true },
        "hp":     { "type": "int", "label": "体力", "order": 2, "min": 1, "max": 99, "required": true },
        "speed":  { "type": "float", "label": "速さ", "order": 3 },
        "weapon": { "type": {"ref": "weapons"}, "label": "武器", "order": 4, "required": true },
        "aggro":  { "type": "float", "label": "索敵反応", "order": 5, "min": 0, "max": 1, "widget": "unit" },
        "drops":  { "type": {"custom": "weights"}, "label": "ドロップ率", "order": 6,
                    "widget": {"weights": {"total": 100}} },
        "portrait":    { "type": "text", "widget": "uiDoc", "label": "肖像", "order": 7 },
        "description": { "type": "text", "widget": "multiline", "label": "解説", "order": 8 }
      }
    }
  }
}"""


weaponsText : String
weaponsText =
    """{
  "weapons": {
    "claw":   { "name": "爪", "damage": 4, "rate": 1.5, "portrait": "assets/portraits/claw.ui.json" },
    "cannon": { "name": "大砲", "damage": 12, "rate": 0.5 }
  }
}"""


weaponsSchemaText : String
weaponsSchemaText =
    """{
  "version": 1,
  "sections": {
    "weapons": {
      "kind": "catalog",
      "fields": {
        "name":   { "type": "text", "label": "名前", "order": 1, "required": true },
        "damage": { "type": "int", "label": "威力", "order": 2, "required": true },
        "rate":   { "type": "float", "label": "連射", "order": 3 },
        "portrait": { "type": "text", "widget": "uiDoc", "label": "肖像", "order": 4 }
      }
    }
  }
}"""


parse : String -> D.Value
parse text =
    D.decodeString D.value text |> Result.withDefault E.null


schemaOf : String -> Maybe Schema.Schema
schemaOf text =
    Schema.decodeString text |> Result.toMaybe


docs : List Dashboards.SourceDoc
docs =
    [ { resource = "enemies", path = "assets/enemies.json", doc = parse enemiesText, schema = schemaOf enemiesSchemaText }
    , { resource = "weapons", path = "assets/weapons.json", doc = parse weaponsText, schema = schemaOf weaponsSchemaText }
    ]


viewWith : Maybe String -> Dashboards.ViewModel
viewWith selected =
    Dashboards.generic.view { uses = [ "enemies", "weapons" ], docs = docs, selected = selected }


fieldValue : String -> Maybe Dashboards.Detail -> Maybe Dashboards.FieldValue
fieldValue label detail =
    detail
        |> Maybe.map .fields
        |> Maybe.withDefault []
        |> List.filter (\f -> f.label == label)
        |> List.head
        |> Maybe.map .value


suite : Test
suite =
    describe "汎用ダッシュボード(攻略本)の導出"
        [ test "主リソース(uses 先頭)のエントリ一覧を文書に書いた順で出す" <|
            \() ->
                (viewWith Nothing).entries
                    |> List.map (.entry >> .id)
                    |> Expect.equal [ "slime", "golem", "ghost" ]
        , test "一覧の行は名前と肖像サムネ(uiDoc)を持つ(無いエントリは Nothing)" <|
            \() ->
                (viewWith Nothing).entries
                    |> List.map (\item -> ( item.name, item.portrait ))
                    |> Expect.equal
                        [ ( Just "スライム", Just "assets/portraits/slime.ui.json" )
                        , ( Just "ゴーレム", Nothing )
                        , ( Just "ゴースト", Nothing )
                        ]
        , test "額装 3 点: 名前=最初の素の text・肖像=uiDoc・解説=multiline を抜き出す" <|
            \() ->
                (viewWith (Just "slime")).detail
                    |> Maybe.map (\d -> ( d.title, d.portrait, d.flavor ))
                    |> Expect.equal
                        (Just
                            ( Just "スライム"
                            , Just "assets/portraits/slime.ui.json"
                            , Just "草原にぷるんと湧く。"
                            )
                        )
        , test "額装 3 点はフィールド行から抜ける(残りは order 順)" <|
            \() ->
                (viewWith (Just "slime")).detail
                    |> Maybe.map (.fields >> List.map .label)
                    |> Expect.equal (Just [ "体力", "速さ", "武器", "索敵反応", "ドロップ率" ])
        , test "min/max 付きの数値はメーターになる(体力 10 / 1〜99)" <|
            \() ->
                fieldValue "体力" (viewWith (Just "slime")).detail
                    |> Expect.equal (Just (Dashboards.Meter { value = 10, min = 1, max = 99, text = "10" }))
        , test "min/max の無い数値は 1 行テキストのまま(速さ)" <|
            \() ->
                fieldValue "速さ" (viewWith (Just "slime")).detail
                    |> Expect.equal (Just (Dashboards.Plain "20"))
        , test "widget unit の数値は % 表示になる(0.35 → 35%)" <|
            \() ->
                fieldValue "索敵反応" (viewWith (Just "slime")).detail
                    |> Expect.equal (Just (Dashboards.Percent { ratio = 0.35, text = "35%" }))
        , test "custom weights は重み棒(total と文書順の配分)になる" <|
            \() ->
                fieldValue "ドロップ率" (viewWith (Just "slime")).detail
                    |> Expect.equal
                        (Just (Dashboards.WeightBars { total = 100, entries = [ ( "claw", 70 ), ( "cannon", 30 ) ] }))
        , test "ref フィールドは参照先の要約(プレビュー)に解決され、肖像は行でなくサムネの席に乗る" <|
            \() ->
                fieldValue "武器" (viewWith (Just "slime")).detail
                    |> Expect.equal
                        (Just
                            (Dashboards.RefValue
                                { target = "weapons"
                                , id = "claw"
                                , peek =
                                    Just
                                        { entry = { resource = "weapons", path = "assets/weapons.json", id = "claw" }
                                        , portrait = Just "assets/portraits/claw.ui.json"
                                        , lines = [ ( "名前", "爪" ), ( "威力", "4" ), ( "連射", "1.5" ) ]
                                        }
                                }
                            )
                        )
        , test "参照先に居ない id(ぶら下がり)はプレビューなしの RefValue になる" <|
            \() ->
                fieldValue "武器" (viewWith (Just "ghost")).detail
                    |> Expect.equal
                        (Just (Dashboards.RefValue { target = "weapons", id = "cursed", peek = Nothing }))
        , test "スキーマの無い文書は額装なし・書いてある順の生のキーで要約する" <|
            \() ->
                let
                    bare =
                        [ { resource = "enemies", path = "assets/enemies.json", doc = parse enemiesText, schema = Nothing } ]
                in
                Dashboards.generic.view { uses = [ "enemies" ], docs = bare, selected = Just "golem" }
                    |> .detail
                    |> Maybe.map (\d -> ( d.title, d.fields ))
                    |> Expect.equal
                        (Just
                            ( Nothing
                            , [ { label = "name", value = Dashboards.Plain "ゴーレム" }
                              , { label = "hp", value = Dashboards.Plain "60" }
                              , { label = "speed", value = Dashboards.Plain "8" }
                              , { label = "weapon", value = Dashboards.Plain "cannon" }
                              ]
                            )
                        )
        ]
