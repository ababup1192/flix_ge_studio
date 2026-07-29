module ApiContractTest exposing (suite)

{-| editor_server 応答の形(コミット済み Editor.flix が正)を Api のデコーダで
読めることを固定する契約テスト。fixture はサーバ実装から手で写した JSON 文字列。
-}

import Api
import Dict
import Expect
import Json.Decode as D
import Set
import Test exposing (Test, describe, test)


healthFixture : String
healthFixture =
    """
    {
      "ok": true,
      "dir": "/Users/abab/Desktop/flix_ge_shapes",
      "title": "Flix Shapes Gallery",
      "design": { "w": 480, "h": 530 },
      "version": "0.1.0"
    }
    """


healthNoProjectFixture : String
healthNoProjectFixture =
    """{"ok": false, "error": {"message": "no project selected"}}"""


projectsFixture : String
projectsFixture =
    """
    {
      "current": null,
      "recent": [
        { "dir": "/Users/abab/Desktop/flix_ge_shapes", "title": "Flix Shapes Gallery" }
      ],
      "found": [
        { "dir": "/Users/abab/Desktop/flix_ge_shooting", "title": "Flix Shooting" }
      ]
    }
    """


filesFixture : String
filesFixture =
    """
    {
      "root": "/Users/abab/Desktop/flix_ge_shapes",
      "ui": [
        { "path": "assets/gallery.ui.json", "composable": false },
        { "path": "assets/hud.ui.json", "composable": true, "paletteFile": "assets/theme.palette.json" }
      ],
      "hitbox": ["assets/player.hitbox.json"],
      "palette": ["assets/theme.palette.json"]
    }
    """


{-| title / plugin / schema が全部ある宣言と、全部無い宣言(素の JSON 群)の両方。
サーバは無いフィールドをキーごと落とすので、fixture もキー無しで写す。
-}
resourcesFixture : String
resourcesFixture =
    """
    {
      "root": "/Users/abab/Desktop/flix_ge_shooting",
      "resources": [
        {
          "id": "level",
          "pattern": "assets/level.json",
          "title": "レベル",
          "plugin": "shooterLevel",
          "files": [ { "path": "assets/level.json", "schema": "assets/level.schema.json" } ]
        },
        {
          "id": "misc",
          "pattern": "assets/*.hitbox.json",
          "files": [ { "path": "assets/player.hitbox.json" } ]
        }
      ],
      "dashboards": []
    }
    """


fileFixture : String
fileFixture =
    """{"path": "assets/characters.json", "content": "{}", "mtime": 1784274206557}"""


putOkFixture : String
putOkFixture =
    """{"mtime": 1784517940797, "ok": true}"""


putConflictFixture : String
putConflictFixture =
    """{"currentMtime": 1784274206557, "error": "conflict: ディスク上のファイルが編集開始後に変わっています", "ok": false}"""


{-| 2026-07-20 の flix_ge_dungeon 実応答(curl GET /resources)から写した全 6 kind。
左レールのグループ表示が読むのはこの形。
-}
dungeonResourcesFixture : String
dungeonResourcesFixture =
    """
    {
      "dashboards": [],
      "resources": [
        { "files": [ { "path": "assets/b1.dungeon.json" }, { "path": "assets/b2.dungeon.json" },
                     { "path": "assets/b3.dungeon.json" }, { "path": "assets/tiletest.dungeon.json" } ],
          "id": "dungeon", "pattern": "assets/*.dungeon.json", "title": "ダンジョン" },
        { "files": [ { "path": "assets/characters.json", "schema": "assets/characters.schema.json" } ],
          "id": "characters", "pattern": "assets/characters.json", "title": "キャラクター" },
        { "files": [ { "path": "assets/cave.theme.json" }, { "path": "assets/grass.theme.json" },
                     { "path": "assets/stone.theme.json" } ],
          "id": "theme", "pattern": "assets/*.theme.json", "title": "テーマ" },
        { "files": [ { "path": "assets/cave.light.json" }, { "path": "assets/grass.light.json" },
                     { "path": "assets/stone.light.json" } ],
          "id": "light", "pattern": "assets/*.light.json", "title": "ライト" },
        { "files": [ { "path": "assets/sprites.json", "schema": "assets/sprites.schema.json" } ],
          "id": "sprites", "pattern": "assets/sprites.json", "title": "飾りと松明" },
        { "files": [ { "path": "assets/magma.shader.json" }, { "path": "assets/water.shader.json" } ],
          "id": "shader", "pattern": "assets/*.shader.json", "title": "シェーダー面" }
      ],
      "root": "/Users/abab/Desktop/flix_ge_dungeon",
      "warnings": []
    }
    """


resourcesWarningsFixture : String
resourcesWarningsFixture =
    """
    {
      "resources": [],
      "warnings": ["『assets/cave.light.json』が JSON として読めません(読み込みは既定値へ倒れます)"]
    }
    """


suite : Test
suite =
    describe "editor_server 応答の契約"
        [ describe "/health"
            [ test "ok:true — dir・題名・設計解像度・版が読める" <|
                \_ ->
                    D.decodeString Api.healthResultDecoder healthFixture
                        |> Expect.equal
                            (Ok
                                (Api.HealthOk
                                    { dir = "/Users/abab/Desktop/flix_ge_shapes"
                                    , title = "Flix Shapes Gallery"
                                    , design = { w = 480, h = 530 }
                                    , version = "0.1.0"
                                    }
                                )
                            )
            , test "ok:false — 未選択応答の error:{message} が失敗文言として読める" <|
                \_ ->
                    D.decodeString Api.healthResultDecoder healthNoProjectFixture
                        |> Expect.equal (Ok (Api.HealthErr "no project selected"))
            ]
        , describe "/projects"
            [ test "current 未選択(null)+ recent / found の {dir, title} 列が読める" <|
                \_ ->
                    D.decodeString Api.projectsDecoder projectsFixture
                        |> Expect.equal
                            (Ok
                                { current = Nothing
                                , recent = [ { dir = "/Users/abab/Desktop/flix_ge_shapes", title = "Flix Shapes Gallery" } ]
                                , found = [ { dir = "/Users/abab/Desktop/flix_ge_shooting", title = "Flix Shooting" } ]
                                }
                            )
            ]
        , describe "/files"
            [ test "ui(メタ付き)/hitbox/palette をならして 1 本のパス列にする(重複は落とす)" <|
                \_ ->
                    D.decodeString Api.filesDecoder filesFixture
                        |> Expect.equal
                            (Ok
                                { root = "/Users/abab/Desktop/flix_ge_shapes"
                                , paths =
                                    [ "assets/gallery.ui.json"
                                    , "assets/hud.ui.json"
                                    , "assets/player.hitbox.json"
                                    , "assets/theme.palette.json"
                                    ]
                                }
                            )
            ]
        , describe "/bake/proxy(焼き応答)"
            [ test "コマ送りの総数は間引き後の枚数(演じた総コマではない)" <|
                \_ ->
                    let
                        body =
                            """{"ok":true,"reachable":true,"body":"{\\"baked\\":[{\\"gif\\":\\"debug/cutscene/prologue.gif\\",\\"frames\\":679,\\"notes\\":[]}]}"}"""
                    in
                    D.decodeString Api.bakeResultDecoder body
                        |> Result.map (\result -> ( result.frames, Api.pngFrameCount result ))
                        -- 30fps で 679 コマ演じ、PNG は stride=2 の 340 枚
                        |> Expect.equal (Ok ( 679, 340 ))
            ]
        , describe "/resources"
            [ test "title・plugin・schema 付きの宣言がグループ構造のまま読める" <|
                \_ ->
                    D.decodeString Api.resourcesDecoder resourcesFixture
                        |> Result.map (.groups >> List.head)
                        |> Expect.equal
                            (Ok
                                (Just
                                    { id = "level"
                                    , bakeUrl = Nothing
                                    , bakeCmd = Nothing
                                    , performUrl = Nothing
                                    , pattern = "assets/level.json"
                                    , title = Just "レベル"
                                    , plugin = Just "shooterLevel"
                                    , files = [ { path = "assets/level.json", schema = Just "assets/level.schema.json" } ]
                                    }
                                )
                            )
            , test "title・plugin・schema が無い宣言は Nothing として読める(欠けで失敗しない)" <|
                \_ ->
                    D.decodeString Api.resourcesDecoder resourcesFixture
                        |> Result.map (.groups >> List.drop 1)
                        |> Expect.equal
                            (Ok
                                [ { id = "misc"
                                  , bakeUrl = Nothing
                                  , bakeCmd = Nothing
                                  , performUrl = Nothing
                                  , pattern = "assets/*.hitbox.json"
                                  , title = Nothing
                                  , plugin = Nothing
                                  , files = [ { path = "assets/player.hitbox.json", schema = Nothing } ]
                                  }
                                ]
                            )
            , test "flix_ge_dungeon の実応答: 6 kind が宣言順のまま題名付きで読める" <|
                \_ ->
                    D.decodeString Api.resourcesDecoder dungeonResourcesFixture
                        |> Result.map (.groups >> List.map (\g -> ( g.id, g.title, List.length g.files )))
                        |> Expect.equal
                            (Ok
                                [ ( "dungeon", Just "ダンジョン", 4 )
                                , ( "characters", Just "キャラクター", 1 )
                                , ( "theme", Just "テーマ", 3 )
                                , ( "light", Just "ライト", 3 )
                                , ( "sprites", Just "飾りと松明", 1 )
                                , ( "shader", Just "シェーダー面", 2 )
                                ]
                            )
            , test "flix_ge_dungeon の実応答: characters にはスキーマが宣言されている" <|
                \_ ->
                    D.decodeString Api.resourcesDecoder dungeonResourcesFixture
                        |> Result.map
                            (.groups
                                >> List.concatMap .files
                                >> List.filterMap .schema
                            )
                        |> Expect.equal (Ok [ "assets/characters.schema.json", "assets/sprites.schema.json" ])
            , test "warnings(宣言と実ファイルのずれ)が読める・欠けは空" <|
                \_ ->
                    ( D.decodeString Api.resourcesDecoder resourcesWarningsFixture |> Result.map .warnings
                    , D.decodeString Api.resourcesDecoder resourcesFixture |> Result.map .warnings
                    )
                        |> Expect.equal
                            ( Ok [ "『assets/cave.light.json』が JSON として読めません(読み込みは既定値へ倒れます)" ]
                            , Ok []
                            )
            ]
        , describe "/file"
            [ test "GET — content と mtime(保存の ifMtime の種)が読める" <|
                \_ ->
                    D.decodeString Api.fileContentDecoder fileFixture
                        |> Expect.equal
                            (Ok { path = "assets/characters.json", content = "{}", mtime = Just 1784274206557 })
            , test "PUT 200 — 新しい mtime が読める(baking 無しの旧サーバは False に倒す)" <|
                \_ ->
                    D.decodeString Api.putFileResultDecoder putOkFixture
                        |> Expect.equal (Ok (Api.PutOk { mtime = Just 1784517940797, baking = False }))
            , test "PUT 409 — 競合(今ディスクに居る版の mtime)として読める" <|
                \_ ->
                    D.decodeString Api.putFileResultDecoder putConflictFixture
                        |> Expect.equal (Ok (Api.PutConflict { currentMtime = 1784274206557 }))
            ]
        , describe "/sprite/colors"
            [ test "ok:true — 「値 → #rrggbb」の表と、解けなかった値が読める。ok:false は空(仮色へ fail-open)" <|
                \_ ->
                    ( D.decodeString Api.spriteColorsDecoder
                        """{"ok": true, "colors": {"accent": "#ffc95e"}, "unresolved": ["body", "trim"]}"""
                    , D.decodeString Api.spriteColorsDecoder
                        """{"ok": false, "error": "JSON parse failed: assets/novel.sprite.json"}"""
                    )
                        |> Expect.equal
                            ( Ok
                                { table = Dict.fromList [ ( "accent", "#ffc95e" ) ]
                                , unresolved = Set.fromList [ "body", "trim" ]
                                }
                            , Ok Api.noSpriteColors
                            )
            , test "unresolved を返さない旧サーバでも表は読める(印が出ないだけ)" <|
                \_ ->
                    D.decodeString Api.spriteColorsDecoder
                        """{"ok": true, "colors": {"accent": "#ffc95e"}}"""
                        |> Expect.equal
                            (Ok { table = Dict.fromList [ ( "accent", "#ffc95e" ) ], unresolved = Set.empty })
            ]
        ]
