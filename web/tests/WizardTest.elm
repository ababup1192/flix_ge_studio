module WizardTest exposing (suite)

{-| ウィザード生成物(3 点セット)の中身を固定する。

生成スキーマは「開いた瞬間に Schema.decodeString が通る = フォームが出る」ことが
実質の検証なので、文字列 pin に加えてデコード成功も確かめる。

-}

import Expect
import Schema
import Test exposing (Test, describe, test)
import Wizard exposing (PathSeg(..), Shape(..), TypeChoice(..))


{-| 検証シナリオと同じ enemies(catalog)の下書き。 -}
enemiesDraft : Wizard.Draft
enemiesDraft =
    { id = "enemies"
    , title = "敵図鑑"
    , path = Nothing
    , shape = ShapeCatalog
    , fields =
        [ { name = "name", type_ = CText, enumValues = "", refTarget = "", label = "名前", required = True, minText = "", maxText = "" }
        , { name = "hp", type_ = CInt, enumValues = "", refTarget = "", label = "", required = False, minText = "0", maxText = "99" }
        , { name = "speed", type_ = CFloat, enumValues = "", refTarget = "", label = "", required = False, minText = "", maxText = "" }
        , { name = "sprite", type_ = CTexture, enumValues = "", refTarget = "", label = "", required = False, minText = "", maxText = "" }
        ]
    }


field : String -> TypeChoice -> Wizard.FieldDraft
field name type_ =
    { name = name, type_ = type_, enumValues = "", refTarget = "", label = "", required = False, minText = "", maxText = "" }


suite : Test
suite =
    describe "Wizard — 3 点セットの生成"
        [ describe "データ雛形(型 → 零値)"
            [ test "catalog: トップレベルキー = id・sample 1 件・全型の零値" <|
                \_ ->
                    Wizard.dataText
                        { enemiesDraft
                            | fields =
                                enemiesDraft.fields
                                    ++ [ { name = "size", type_ = CEnum, enumValues = "small, big", refTarget = "", label = "", required = False, minText = "", maxText = "" }
                                       , { name = "route", type_ = CRef, enumValues = "", refTarget = "routes", label = "", required = False, minText = "", maxText = "" }
                                       , field "alive" CBool
                                       , field "tint" CColor
                                       , field "origin" CVec2
                                       ]
                        }
                        |> Expect.equal
                            (String.join "\n"
                                [ "{"
                                , "  \"//\": \"ウィザード生成・自由に編集可\","
                                , "  \"enemies\": {"
                                , "    \"sample\": {"
                                , "      \"name\": \"\","
                                , "      \"hp\": 0,"
                                , "      \"speed\": 0,"
                                , "      \"sprite\": \"\","
                                , "      \"size\": \"small\","
                                , "      \"route\": \"\","
                                , "      \"alive\": false,"
                                , "      \"tint\": \"#ffffff\","
                                , "      \"origin\": {"
                                , "        \"x\": 0,"
                                , "        \"y\": 0"
                                , "      }"
                                , "    }"
                                , "  }"
                                , "}"
                                , ""
                                ]
                            )
            , test "list: 零値レコード 1 件入りの配列" <|
                \_ ->
                    Wizard.dataText
                        { id = "waves", title = "", path = Nothing, shape = ShapeList, fields = [ field "atX" CInt ] }
                        |> Expect.equal
                            (String.join "\n"
                                [ "{"
                                , "  \"//\": \"ウィザード生成・自由に編集可\","
                                , "  \"waves\": ["
                                , "    {"
                                , "      \"atX\": 0"
                                , "    }"
                                , "  ]"
                                , "}"
                                , ""
                                ]
                            )
            , test "record: 零値レコードそのもの" <|
                \_ ->
                    Wizard.dataText
                        { id = "meta", title = "", path = Nothing, shape = ShapeRecord, fields = [ field "speed" CFloat ] }
                        |> Expect.equal
                            (String.join "\n"
                                [ "{"
                                , "  \"//\": \"ウィザード生成・自由に編集可\","
                                , "  \"meta\": {"
                                , "    \"speed\": 0"
                                , "  }"
                                , "}"
                                , ""
                                ]
                            )
            ]
        , describe "スキーマ(新方言)"
            [ test "sections 1 個・order は並び順・数値の min/max・required" <|
                \_ ->
                    Wizard.schemaText enemiesDraft
                        |> Expect.equal
                            (String.join "\n"
                                [ "{"
                                , "  \"version\": 1,"
                                , "  \"sections\": {"
                                , "    \"enemies\": {"
                                , "      \"kind\": \"catalog\","
                                , "      \"fields\": {"
                                , "        \"name\": {"
                                , "          \"type\": \"text\","
                                , "          \"label\": \"名前\","
                                , "          \"order\": 1,"
                                , "          \"required\": true"
                                , "        },"
                                , "        \"hp\": {"
                                , "          \"type\": \"int\","
                                , "          \"order\": 2,"
                                , "          \"min\": 0,"
                                , "          \"max\": 99"
                                , "        },"
                                , "        \"speed\": {"
                                , "          \"type\": \"float\","
                                , "          \"order\": 3"
                                , "        },"
                                , "        \"sprite\": {"
                                , "          \"type\": \"texture\","
                                , "          \"order\": 4"
                                , "        }"
                                , "      }"
                                , "    }"
                                , "  }"
                                , "}"
                                , ""
                                ]
                            )
            , test "enum / ref はタグ形で書かれ、生成物は Schema.decodeString が読める(= フォームが出る)" <|
                \_ ->
                    let
                        draft =
                            { id = "spawns"
                            , title = ""
                            , path = Nothing
                            , shape = ShapeList
                            , fields =
                                [ { name = "kind", type_ = CEnum, enumValues = "popcorn, turret ,dome", refTarget = "", label = "敵種", required = True, minText = "", maxText = "" }
                                , { name = "route", type_ = CRef, enumValues = "", refTarget = "routes", label = "軌道", required = False, minText = "", maxText = "" }
                                ]
                            }

                        decoded =
                            Schema.decodeString (Wizard.schemaText draft)

                        sectionOf schema =
                            schema.sections |> List.head

                        kindOf schema =
                            sectionOf schema |> Maybe.map (\( key, s ) -> ( key, s.kind ))

                        typesOf schema =
                            sectionOf schema
                                |> Maybe.map (\( _, s ) -> s.fields |> List.map (\( n, f ) -> ( n, f.type_ )))
                    in
                    decoded
                        |> Result.map (\schema -> ( kindOf schema, typesOf schema ))
                        |> Expect.equal
                            (Ok
                                ( Just ( "spawns", Schema.ListKind )
                                , Just
                                    [ ( "kind", Schema.TEnum [ "popcorn", "turret", "dome" ] )
                                    , ( "route", Schema.TRef "routes" )
                                    ]
                                )
                            )
            ]
        , describe "project.json への追記編集の導出"
            [ test "path は追記先の配列(editor.resources)・エントリは id/pattern/title" <|
                \_ ->
                    ( Wizard.declEdit enemiesDraft |> .path, Wizard.declText enemiesDraft )
                        |> Expect.equal
                            ( [ Key "editor", Key "resources" ]
                            , String.join "\n"
                                [ "{"
                                , "  \"id\": \"enemies\","
                                , "  \"pattern\": \"assets/enemies.json\","
                                , "  \"title\": \"敵図鑑\""
                                , "}"
                                ]
                            )
            , test "タイトル空なら title キーごと書かない" <|
                \_ ->
                    Wizard.declText { enemiesDraft | title = "  " }
                        |> Expect.equal
                            (String.join "\n"
                                [ "{"
                                , "  \"id\": \"enemies\","
                                , "  \"pattern\": \"assets/enemies.json\""
                                , "}"
                                ]
                            )
            ]
        , describe "置き場所"
            [ test "既定は assets/<id>.json・スキーマは sibling 規約" <|
                \_ ->
                    ( Wizard.dataPathOf enemiesDraft, Wizard.schemaPathOf enemiesDraft )
                        |> Expect.equal ( "assets/enemies.json", "assets/enemies.schema.json" )
            , test "置き場所を触ったらそちらが正(スキーマも追従)" <|
                \_ ->
                    let
                        moved =
                            { enemiesDraft | path = Just "data/mobs.json" }
                    in
                    ( Wizard.dataPathOf moved, Wizard.schemaPathOf moved )
                        |> Expect.equal ( "data/mobs.json", "data/mobs.schema.json" )
            ]
        , describe "検査"
            [ test "そろった下書きは問題なし" <|
                \_ ->
                    Wizard.validate [ "assets/level.json" ] enemiesDraft
                        |> Expect.equal []
            , test "id の日本語・フィールド名重複・enum 値なし・ref 先なし・min>max を全部知らせる" <|
                \_ ->
                    Wizard.validate []
                        { id = "敵"
                        , title = ""
                        , path = Nothing
                        , shape = ShapeCatalog
                        , fields =
                            [ field "hp" CInt
                            , { name = "hp", type_ = CInt, enumValues = "", refTarget = "", label = "", required = False, minText = "9", maxText = "1" }
                            , { name = "size", type_ = CEnum, enumValues = " , ", refTarget = "", label = "", required = False, minText = "", maxText = "" }
                            , { name = "route", type_ = CRef, enumValues = "", refTarget = " ", label = "", required = False, minText = "", maxText = "" }
                            ]
                        }
                        |> Expect.equal
                            [ "名前(id)は半角英数で入力してください(先頭は英字): 敵"
                            , "フィールド名が重複しています: hp"
                            , "フィールド「hp」: min が max を超えています"
                            , "フィールド「size」: enum の値をカンマ区切りで 1 つ以上入れてください"
                            , "フィールド「route」: ref の参照先セクション名を入れてください"
                            ]
            , test "既にあるファイル(データ側・スキーマ側どちらでも)と重なったら止める" <|
                \_ ->
                    Wizard.validate [ "assets/enemies.schema.json" ] enemiesDraft
                        |> Expect.equal [ "既にあるファイルと重なります: assets/enemies.schema.json" ]
            ]
        , describe "並べ替え"
            [ test "↓で隣と入れ替わり、端からはみ出す指定はそのまま" <|
                \_ ->
                    let
                        fields =
                            [ field "a" CText, field "b" CInt, field "c" CBool ]

                        names fs =
                            List.map .name fs
                    in
                    ( names (Wizard.moveField 0 1 fields)
                    , names (Wizard.moveField 2 1 fields)
                    , names (Wizard.moveField 0 -1 fields)
                    )
                        |> Expect.equal ( [ "b", "a", "c" ], [ "a", "b", "c" ], [ "a", "b", "c" ] )
            ]
        ]
