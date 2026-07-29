module SchemaFormTest exposing (suite)

{-| スキーマ+選択エントリ → フォーム行モデルの純ロジック。
order 順・label の既定・ref の選択肢導出・widget の割り当てを固定する。
-}

import Doc
import Expect
import Json.Decode as D
import Json.Encode as E
import Schema
import SchemaForm exposing (Control(..))
import Test exposing (Test, describe, test)


schemaFixture : Schema.Schema
schemaFixture =
    """
{
  "sections": {
    "routes": {
      "kind": "catalog",
      "fields": {
        "type":  { "type": {"enum": ["straight", "sine"]}, "order": 1, "widget": "segmented" },
        "speed": { "type": "float", "order": 2, "min": 0, "max": 150, "step": 5 }
      }
    },
    "spawns": {
      "kind": "list",
      "fields": {
        "route": { "type": {"ref": "routes"}, "label": "軌道", "order": 2, "required": true,
                   "help": "軌道は routes で定義した動きの名前。" },
        "atX":   { "type": "int", "order": 1, "min": 0 },
        "pos":   { "type": "vec2", "order": 3 },
        "note":  { "type": "text" },
        "boss":  { "type": "bool", "order": 4, "default": false }
      }
    },
    "enemies": {
      "kind": "catalog",
      "fields": {
        "aggro": { "type": "float", "order": 1, "min": 0, "max": 1, "widget": "unit" },
        "crit":  { "type": "int", "order": 2, "min": 0, "max": 100, "widget": "percent" },
        "tint":  { "type": "color", "order": 3 },
        "skin":  { "type": "texture", "order": 4 },
        "home":  { "type": "vec2", "order": 5 },
        "drops": { "type": {"custom": "weights"}, "order": 6, "widget": {"weights": {"total": 100}} },
        "brain": { "type": {"custom": "mystery"}, "order": 7 },
        "portrait": { "type": "text", "widget": "uiDoc", "order": 8 },
        "bio":   { "type": "text", "widget": "multiline", "order": 9 },
        "arm":   { "type": {"ref": "weapons"}, "order": 10 }
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
    { "atX": 200, "route": "wavy", "pos": [1, 2] }
  ],
  "enemies": {
    "slime": { "aggro": 0.35, "crit": 20, "tint": "#33cc66", "skin": "slime_a",
               "home": {"x": 4, "y": 8},
               "drops": { "claw": 70, "cannon": 30 }, "brain": { "mode": "idle" },
               "portrait": "assets/portraits/slime.ui.json", "bio": "ぷるぷる。",
               "arm": "claw" },
    "ghost": { "tint": { "r": 1, "g": 0, "b": 0 } }
  }
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


rowsIn : String -> D.Value -> List SchemaForm.Row
rowsIn key entry =
    SchemaForm.rows { doc = docFixture, textures = [ "slime_a", "slime_b" ], others = [] } (sectionOf key) entry


firstSpawn : D.Value
firstSpawn =
    Doc.list "spawns" docFixture
        |> List.head
        |> Maybe.withDefault E.null


catalogEntry : String -> String -> D.Value
catalogEntry key name =
    Doc.record key docFixture
        |> D.decodeValue (D.field name D.value)
        |> Result.withDefault E.null


spawnRows : List SchemaForm.Row
spawnRows =
    rowsIn "spawns" firstSpawn


slimeControls : String -> List Control
slimeControls name =
    rowsIn "enemies" (catalogEntry "enemies" "slime")
        |> List.filter (\r -> r.name == name)
        |> List.map .control


suite : Test
suite =
    describe "SchemaForm — フォーム行モデルの生成"
        [ test "行は order 順・order 無しは末尾。label 無しはフィールド名で出す" <|
            \_ ->
                spawnRows
                    |> List.map (\r -> ( r.name, r.label, r.required ))
                    |> Expect.equal
                        [ ( "atX", "atX", False )
                        , ( "route", "軌道", True )
                        , ( "pos", "pos", False )
                        , ( "boss", "boss", False )
                        , ( "note", "note", False )
                        ]
        , test "help — 書いた欄だけ行に載る(書いていない欄は Nothing のまま)" <|
            \_ ->
                spawnRows
                    |> List.map (\r -> ( r.name, r.help ))
                    |> Expect.equal
                        [ ( "atX", Nothing )
                        , ( "route", Just "軌道は routes で定義した動きの名前。" )
                        , ( "pos", Nothing )
                        , ( "boss", Nothing )
                        , ( "note", Nothing )
                        ]
        , test "ref — 選択肢は参照先 catalog のエントリ名(文書順)・現在値が選択される" <|
            \_ ->
                spawnRows
                    |> List.filter (\r -> r.name == "route")
                    |> List.map .control
                    |> Expect.equal
                        [ RefControl { target = "routes", choices = [ "wavy", "slow" ], selected = Just "wavy" } ]
        , test "int — min だけでは slider にならない(可動域が決まらない)" <|
            \_ ->
                spawnRows
                    |> List.filter (\r -> r.name == "atX")
                    |> List.map .control
                    |> Expect.equal
                        [ NumberControl
                            { isInt = True
                            , value = Just 200
                            , min = Just 0
                            , max = Nothing
                            , step = Nothing
                            , slider = False
                            , percentFactor = Nothing
                            }
                        ]
        , test "float — min と max が揃えば slider(step も通る)" <|
            \_ ->
                rowsIn "routes" (catalogEntry "routes" "slow")
                    |> List.filter (\r -> r.name == "speed")
                    |> List.map .control
                    |> Expect.equal
                        [ NumberControl
                            { isInt = False
                            , value = Just 60
                            , min = Just 0
                            , max = Just 150
                            , step = Just 5
                            , slider = True
                            , percentFactor = Nothing
                            }
                        ]
        , test "enum — widget segmented はボタン列になる" <|
            \_ ->
                rowsIn "routes" (catalogEntry "routes" "wavy")
                    |> List.filter (\r -> r.name == "type")
                    |> List.map .control
                    |> Expect.equal
                        [ EnumControl { choices = [ "straight", "sine" ], selected = Just "sine", segmented = True } ]
        , test "widget unit — 0〜1 の確率は % 併記(×100)の slider になる" <|
            \_ ->
                slimeControls "aggro"
                    |> Expect.equal
                        [ NumberControl
                            { isInt = False
                            , value = Just 0.35
                            , min = Just 0
                            , max = Just 1
                            , step = Nothing
                            , slider = True
                            , percentFactor = Just 100
                            }
                        ]
        , test "widget percent — 0〜100 はそのまま % 併記(×1)" <|
            \_ ->
                slimeControls "crit"
                    |> Expect.equal
                        [ NumberControl
                            { isInt = True
                            , value = Just 20
                            , min = Just 0
                            , max = Just 100
                            , step = Nothing
                            , slider = True
                            , percentFactor = Just 1
                            }
                        ]
        , test "vec2 — {x,y} の形は 2 連数値欄になる" <|
            \_ ->
                slimeControls "home"
                    |> Expect.equal [ Vec2Control { x = Just 4, y = Just 8 } ]
        , test "vec2 — {x,y} でない形(配列)は生 JSON 行へ倒す(壊さず編集可)" <|
            \_ ->
                spawnRows
                    |> List.filter (\r -> r.name == "pos")
                    |> List.map .control
                    |> Expect.equal [ RawJsonControl "[1,2]" ]
        , test "color — #rrggbb 文字列はピッカー・{r,g,b} の形は読み取り表示のみ" <|
            \_ ->
                ( slimeControls "tint"
                , rowsIn "enemies" (catalogEntry "enemies" "ghost")
                    |> List.filter (\r -> r.name == "tint")
                    |> List.map .control
                )
                    |> Expect.equal
                        ( [ ColorControl (Just "#33cc66") ]
                        , [ ReadOnlyControl "{\"r\":1,\"g\":0,\"b\":0}" ]
                        )
        , test "texture — 候補は project.json の manifest 名・現在値も持つ" <|
            \_ ->
                slimeControls "skin"
                    |> Expect.equal
                        [ TextureControl { value = Just "slime_a", choices = [ "slime_a", "slime_b" ] } ]
        , test "custom weights — widget の total と文書順の配分を持つ" <|
            \_ ->
                slimeControls "drops"
                    |> Expect.equal
                        [ WeightsControl
                            { config = { total = 100, decimals = 0 }
                            , entries = [ ( "claw", 70 ), ( "cannon", 30 ) ]
                            }
                        ]
        , test "custom weights — 値が無ければ空の配分(行の追加から始める)" <|
            \_ ->
                rowsIn "enemies" (catalogEntry "enemies" "ghost")
                    |> List.filter (\r -> r.name == "drops")
                    |> List.map .control
                    |> Expect.equal
                        [ WeightsControl { config = { total = 100, decimals = 0 }, entries = [] } ]
        , test "他の custom は生 JSON 行のまま(widget は非解釈)" <|
            \_ ->
                slimeControls "brain"
                    |> Expect.equal [ RawJsonControl "{\"mode\":\"idle\"}" ]
        , test "値が無ければ default を見せる(bool)。default も無ければ空" <|
            \_ ->
                spawnRows
                    |> List.filter (\r -> r.name == "boss" || r.name == "note")
                    |> List.map .control
                    |> Expect.equal [ BoolControl (Just False), TextControl Nothing ]
        , test "text widget uiDoc — 肖像欄(サムネ付きパス入力)になる" <|
            \_ ->
                slimeControls "portrait"
                    |> Expect.equal [ UiDocControl (Just "assets/portraits/slime.ui.json") ]
        , test "text widget multiline — 長文欄(textarea)になる" <|
            \_ ->
                slimeControls "bio"
                    |> Expect.equal [ MultilineControl (Just "ぷるぷる。") ]
        , test "ref — 参照先セクションが同一文書に無ければ横断辞書から選択肢を引く" <|
            \_ ->
                SchemaForm.rows
                    { doc = docFixture
                    , textures = []
                    , others =
                        [ { resource = "weapons"
                          , path = "assets/weapons.json"
                          , doc =
                                D.decodeString D.value """{ "weapons": { "claw": {}, "cannon": {} } }"""
                                    |> Result.withDefault E.null
                          , schema = Nothing
                          }
                        ]
                    }
                    (sectionOf "enemies")
                    (catalogEntry "enemies" "slime")
                    |> List.filter (\r -> r.name == "arm")
                    |> List.map .control
                    |> Expect.equal
                        [ RefControl { target = "weapons", choices = [ "claw", "cannon" ], selected = Just "claw" } ]
        , test "ref — 横断辞書が無ければ選択肢は空のまま(従来どおり)" <|
            \_ ->
                slimeControls "arm"
                    |> Expect.equal
                        [ RefControl { target = "weapons", choices = [], selected = Just "claw" } ]
        , test "type grid — 行の列は改行で継がれ、行数が高さの目安になる" <|
            \_ ->
                let
                    gridSection =
                        """{"sections": {"grid": {"kind": "record", "fields": {"rows": {"type": "grid"}}}}}"""
                            |> Schema.decodeString
                            |> Result.toMaybe
                            |> Maybe.andThen (\s -> s.sections |> List.head |> Maybe.map Tuple.second)

                    entry =
                        E.object [ ( "rows", E.list E.string [ "##T##", "#...#" ] ) ]
                in
                gridSection
                    |> Maybe.map
                        (\sec ->
                            SchemaForm.rows { doc = E.null, textures = [], others = [] } sec entry
                                |> List.map .control
                        )
                    |> Expect.equal
                        (Just [ GridControl { text = Just "##T##\n#...#", lineCount = 2 } ])
        , test "type list(text) — 文字列の列は行の並び。値が無ければ空・文字列でない列は生 JSON へ" <|
            \_ ->
                let
                    saysControls entry =
                        linesSection
                            |> Maybe.map
                                (\sec ->
                                    SchemaForm.rows { doc = E.null, textures = [], others = [] } sec entry
                                        |> List.map .control
                                )
                in
                ( saysControls (E.object [ ( "says", E.list E.string [ "あれ……", "" ] ) ])
                , saysControls (E.object [])
                , saysControls (E.object [ ( "says", E.list E.int [ 1, 2 ] ) ])
                )
                    |> Expect.equal
                        ( Just [ ListTextControl [ "あれ……", "" ] ]
                        , Just [ ListTextControl [] ]
                        , Just [ RawJsonControl "[1,2]" ]
                        )
        , test "文字列の列の 1 操作 — 差し替え・追加・削除・入れ替え(空行は間引かない・範囲外は何もしない)" <|
            \_ ->
                let
                    items =
                        [ "a", "", "c" ]

                    apply edit =
                        SchemaForm.applyListEdit edit items
                in
                ( ( apply (SchemaForm.SetLine 1 "b"), apply SchemaForm.AddLine )
                , ( apply (SchemaForm.RemoveLine 0), apply (SchemaForm.MoveLine 2 -1) )
                , ( apply (SchemaForm.MoveLine 0 -1), apply (SchemaForm.SetLine 9 "x") )
                )
                    |> Expect.equal
                        ( ( [ "a", "b", "c" ], [ "a", "", "c", "" ] )
                        , ( [ "", "c" ], [ "a", "c", "" ] )
                        , ( items, items )
                        )
        ]


{-| 文字列の列(台詞など)1 フィールドだけのセクション。 -}
linesSection : Maybe Schema.Section
linesSection =
    """{"sections": {"trigger": {"kind": "record", "fields": {"says": {"type": {"list": "text"}}}}}}"""
        |> Schema.decodeString
        |> Result.toMaybe
        |> Maybe.andThen (\s -> s.sections |> List.head |> Maybe.map Tuple.second)
