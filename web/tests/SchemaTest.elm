module SchemaTest exposing (suite)

{-| スキーマ方言のデコーダを実物 fixture(flix_ge_shooting の level.schema.json の写し)で
固定する。widget / default は Value のままなので、比較は JSON 文字列に落として行う。
-}

import Expect
import Json.Encode as E
import Schema exposing (FieldType(..))
import Test exposing (Test, describe, test)


{-| /Users/abab/Desktop/flix_ge_shooting/assets/level.schema.json の写し(2026-07-17 時点) -}
levelSchemaFixture : String
levelSchemaFixture =
    """
{
  "//": "level.json の形の宣言(新方言)。type はゲームも検証に使う。widget はエディタ専用。",
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


{-| FieldType を比較しやすい文字列へ(widget と違い type は方言の骨格なので全タグ pin) -}
typeName : FieldType -> String
typeName t =
    case t of
        TText ->
            "text"

        TInt ->
            "int"

        TFloat ->
            "float"

        TBool ->
            "bool"

        TColor ->
            "color"

        TVec2 ->
            "vec2"

        TTexture ->
            "texture"

        TJson ->
            "json"

        TGrid ->
            "grid"

        TEnum choices ->
            "enum(" ++ String.join "," choices ++ ")"

        TRef target ->
            "ref:" ++ target

        TList inner ->
            "list(" ++ typeName inner ++ ")"

        TRecord fields ->
            "record(" ++ String.join "," (List.map Tuple.first fields) ++ ")"

        TCustom tag ->
            "custom:" ++ tag


type alias FieldPin =
    { type_ : String
    , label : Maybe String
    , order : Maybe Int
    , widget : Maybe String
    , required : Bool
    , default : Maybe String
    , min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float
    }


fieldPin : Schema.Field -> FieldPin
fieldPin f =
    { type_ = typeName f.type_
    , label = f.label
    , order = f.order
    , widget = Maybe.map (E.encode 0) f.widget
    , required = f.required
    , default = Maybe.map (E.encode 0) f.default
    , min = f.min
    , max = f.max
    , step = f.step
    }


decoded : Result String Schema.Schema
decoded =
    Schema.decodeString levelSchemaFixture


sectionFields : String -> Result String (List ( String, FieldPin ))
sectionFields key =
    decoded
        |> Result.map
            (\schema ->
                schema.sections
                    |> List.filter (\( k, _ ) -> k == key)
                    |> List.concatMap (\( _, section ) -> section.fields)
                    |> List.map (Tuple.mapSecond fieldPin)
            )


suite : Test
suite =
    describe "Schema デコーダ(level.schema.json 実物)"
        [ test "version とセクション(キー順・kind)が読める。ルートの \"//\" 注釈は無視" <|
            \_ ->
                decoded
                    |> Result.map (\s -> ( s.version, List.map (Tuple.mapSecond .kind) s.sections ))
                    |> Expect.equal
                        (Ok
                            ( Just 1
                            , [ ( "meta", Schema.RecordKind )
                              , ( "routes", Schema.Catalog )
                              , ( "spawns", Schema.ListKind )
                              ]
                            )
                        )
        , test "meta — scrollSpeed の全属性(float・min/max・widget slider・default 60)" <|
            \_ ->
                sectionFields "meta"
                    |> Expect.equal
                        (Ok
                            [ ( "scrollSpeed"
                              , { type_ = "float"
                                , label = Just "スクロール速度"
                                , order = Just 1
                                , widget = Just "\"slider\""
                                , required = False
                                , default = Just "60"
                                , min = Just 0
                                , max = Just 200
                                , step = Nothing
                                }
                              )
                            ]
                        )
        , test "routes — enum の選択肢と required が読める(フィールドは書いた順)" <|
            \_ ->
                sectionFields "routes"
                    |> Result.map (List.map (\( name, f ) -> ( name, f.type_, f.required )))
                    |> Expect.equal
                        (Ok
                            [ ( "type", "enum(straight,sine,hover)", True )
                            , ( "speed", "float", True )
                            , ( "amp", "float", False )
                            , ( "freq", "float", False )
                            ]
                        )
        , test "spawns — ref の参照先・不透明 widget(オブジェクト形)・default が読める" <|
            \_ ->
                sectionFields "spawns"
                    |> Result.map (List.map (\( name, f ) -> ( name, f.type_, ( f.widget, f.default ) )))
                    |> Expect.equal
                        (Ok
                            [ ( "atX", "int", ( Just "{\"pickOnPreview\":{\"axis\":\"x\"}}", Nothing ) )
                            , ( "kind", "enum(popcorn,turret,dome)", ( Just "\"segmented\"", Nothing ) )
                            , ( "y", "int", ( Just "\"slider\"", Nothing ) )
                            , ( "route", "ref:routes", ( Nothing, Nothing ) )
                            , ( "count", "int", ( Just "\"spinner\"", Just "1" ) )
                            , ( "intervalSec", "float", ( Nothing, Just "0.35" ) )
                            ]
                        )
        , test "未知の type 文字列は Err(場所つきの日本語 1 行)" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"a": {"kind": "record", "fields": {"x": {"type": "date"}}}}}"""
                    |> Expect.equal
                        (Err "スキーマが読めません: sections → a → fields → x → type → 未知の type \"date\"")
        , test "未知の type タグ(オブジェクト形)も Err" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"a": {"kind": "record", "fields": {"x": {"type": {"fancy": []}}}}}}"""
                    |> Expect.equal
                        (Err "スキーマが読めません: sections → a → fields → x → type → 未知の type タグ \"fancy\"")
        , test "fields 内の \"//\" キーは注釈として読み飛ばす" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"a": {"kind": "record", "fields": {"//メモ": "自由文", "x": {"type": "int"}}}}}"""
                    |> Result.map
                        (\s ->
                            s.sections
                                |> List.concatMap (\( _, section ) -> section.fields)
                                |> List.map Tuple.first
                        )
                    |> Expect.equal (Ok [ "x" ])
        , test "入れ子 type(list / record / custom)も読める" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"a": {"kind": "record", "fields": {"x": {"type": {"list": {"record": {"p": {"type": "vec2"}, "tag": {"type": {"custom": "gradient"}}}}}}}}}}"""
                    |> Result.map
                        (\s ->
                            s.sections
                                |> List.concatMap (\( _, section ) -> section.fields)
                                |> List.map (\( name, f ) -> ( name, typeName f.type_ ))
                        )
                    |> Expect.equal (Ok [ ( "x", "list(record(p,tag))" ) ])
        , test "kind map(sprites.schema.json の形)は Catalog に畳まれ、fields は item から読む" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"sprites": {"kind": "map", "label": "レシピ", "item": {"kind": "record", "fields": {"unit": {"type": {"enum": ["tile", "px"]}}, "parts": {"type": "json", "required": true}}}}}}"""
                    |> Result.map
                        (\s ->
                            s.sections
                                |> List.map
                                    (\( key, section ) ->
                                        ( key
                                        , section.kind
                                        , section.fields |> List.map (\( n, f ) -> ( n, typeName f.type_ ))
                                        )
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "sprites"
                              , Schema.Catalog
                              , [ ( "unit", "enum(tile,px)" ), ( "parts", "json" ) ]
                              )
                            ]
                        )
        , test "kind list も item(入れ子)方言で中身を宣言できる(dungeon.schema.json の形)" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"rooms": {"kind": "list", "label": "部屋", "item": {"kind": "record", "fields": {"x": {"type": "int"}, "y": {"type": "int"}}}}}}"""
                    |> Result.map
                        (\s ->
                            s.sections
                                |> List.map (\( k, sec ) -> ( k, sec.kind, List.map Tuple.first sec.fields ))
                        )
                    |> Expect.equal (Ok [ ( "rooms", Schema.ListKind, [ "x", "y" ] ) ])
        , test "kind map で item が無い宣言は空 fields(キーの出し入れだけの catalog)" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"sprites": {"kind": "map"}}}"""
                    |> Result.map (\s -> s.sections |> List.map (Tuple.mapSecond (\sec -> ( sec.kind, sec.fields ))))
                    |> Expect.equal (Ok [ ( "sprites", ( Schema.Catalog, [] ) ) ])
        , test "未知の kind は Err にせず Unsupported(1 セクション未対応で文書全体を壊れ扱いしない)" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"fx": {"kind": "curve", "shape": []}, "meta": {"kind": "record", "fields": {"x": {"type": "int"}}}}}"""
                    |> Result.map (\s -> s.sections |> List.map (Tuple.mapSecond .kind))
                    |> Expect.equal
                        (Ok
                            [ ( "fx", Schema.Unsupported "curve" )
                            , ( "meta", Schema.RecordKind )
                            ]
                        )
        , test "kind value / field はセクション自体が 1 個のフィールド宣言(light/shader の形)" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {
                         "darkness": {"kind": "value", "type": "float", "label": "暗幕の濃さ", "min": 0, "max": 1, "default": 0.85},
                         "name": {"kind": "field", "type": "string", "label": "名前", "order": 1, "required": true}}}"""
                    |> Result.map
                        (\s ->
                            s.sections
                                |> List.map
                                    (\( key, sec ) ->
                                        case sec.kind of
                                            Schema.ValueKind f ->
                                                ( key, typeName f.type_, ( f.min, f.max, f.required ) )

                                            _ ->
                                                ( key, "(not value)", ( Nothing, Nothing, False ) )
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "darkness", "float", ( Just 0, Just 1, False ) )
                            , ( "name", "text", ( Nothing, Nothing, True ) )
                            ]
                        )
        , test "enabledWhen: equals(単一)も in(複数)も許容値の列に落として読む" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {"s": {"kind": "record", "fields": {
                         "one":  {"type": "int", "enabledWhen": {"field": "shape", "equals": "ngon"}},
                         "many": {"type": "int", "enabledWhen": {"field": "shape", "in": ["circle", "ngon"]}},
                         "none": {"type": "int"},
                         "bad":  {"type": "int", "enabledWhen": {"field": "shape"}}}}}}"""
                    |> Result.map
                        (\s ->
                            s.sections
                                |> List.concatMap (\( _, sec ) -> sec.fields)
                                |> List.map
                                    (\( n, f ) ->
                                        ( n
                                        , f.enabledWhen
                                            |> Maybe.map
                                                (\c -> ( c.field, List.length c.allowed ))
                                        )
                                    )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "one", Just ( "shape", 1 ) )
                            , ( "many", Just ( "shape", 2 ) )
                            , ( "none", Nothing )

                            -- 値の指定が無い未知形は「条件なし(常に有効)」へ倒す(fail-open)
                            , ( "bad", Nothing )
                            ]
                        )
        , test "セクションの label が読める(タブ表示名・無ければ Nothing でキー名にフォールバック)" <|
            \_ ->
                Schema.decodeString
                    """{"sections": {
                         "rooms": {"kind": "list", "label": "部屋(矩形)", "item": {"kind": "record", "fields": {"x": {"type": "int"}}}},
                         "grid":  {"kind": "record", "fields": {"w": {"type": "int"}}}}}"""
                    |> Result.map (\s -> s.sections |> List.map (\( key, sec ) -> ( key, sec.label )))
                    |> Expect.equal
                        (Ok [ ( "rooms", Just "部屋(矩形)" ), ( "grid", Nothing ) ])
        , describe "パース失敗の振り分け(赤エラーか、フォーム対象外の案内か)"
            [ test "$schema/properties 持ち(draft-07 等)はフォーム対象外の種類 = 穏やか表示" <|
                \_ ->
                    let
                        draft07 =
                            """{"$schema": "http://json-schema.org/draft-07/schema#",
                                "type": "object",
                                "properties": {"frames": {"type": "array"}}}"""
                    in
                    -- sections 方言としては読めないが、壊れ扱いにはしない
                    ( Schema.decodeString draft07 |> Result.toMaybe, Schema.isJsonSchema draft07 )
                        |> Expect.equal ( Nothing, True )
            , test "sections 方言の壊れ(JSON-Schema でもない)は従来どおり赤" <|
                \_ ->
                    let
                        broken =
                            """{"version": 1, "sections": 42}"""
                    in
                    ( Schema.decodeString broken |> Result.toMaybe, Schema.isJsonSchema broken )
                        |> Expect.equal ( Nothing, False )
            ]
        ]
