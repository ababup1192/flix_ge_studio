module FlowTest exposing (suite)

{-| 配線フローのテスト(elm-program-test)。

update が返す Effect を模擬ポートへ流し、apiResponse を注入して
「どの操作でどの封筒が飛ぶ / 飛ばないか」をブラウザ無しで検査する。
封筒 id は update の採番(reqCounter)が決定的なので、フローごとに追って書く。

-}

import Effect exposing (Effect)
import Expect
import Json.Decode as D
import Json.Encode as E
import Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Ports
import SimulatedEffect.Process
import SimulatedEffect.Task
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, containing, tag, text)


type alias App =
    ProgramTest Main.Model Main.Msg Effect


start : App
start =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulate
        |> ProgramTest.withSimulatedSubscriptions
            (\_ -> SimulatedEffect.Ports.subscribe "apiResponse" D.value Main.GotApiResponse)
        |> ProgramTest.start ()


{-| Effect → 模擬ポート。本物の perform と対になる唯一の変換。 -}
simulate : Effect -> ProgramTest.SimulatedEffect Main.Msg
simulate effect =
    case effect of
        Effect.SendApi req ->
            SimulatedEffect.Ports.send "apiRequest" (Effect.encodeRequest req)

        Effect.ExpireNotice info ->
            SimulatedEffect.Process.sleep info.afterMs
                |> SimulatedEffect.Task.perform
                    (\_ -> Main.NoticeExpired { seq = info.seq, message = info.message })

        Effect.Autosave info ->
            SimulatedEffect.Process.sleep info.afterMs
                |> SimulatedEffect.Task.perform (\_ -> Main.AutosaveFired info.seq)

        Effect.NoFx ->
            SimulatedEffect.Cmd.none

        Effect.Batch effects ->
            SimulatedEffect.Cmd.batch (List.map simulate effects)



-- 封筒の出し入れ


respondOk : Int -> String -> E.Value -> App -> App
respondOk id kind body =
    ProgramTest.simulateIncomingPort "apiResponse"
        (E.object
            [ ( "id", E.int id )
            , ( "kind", E.string kind )
            , ( "ok", E.bool True )
            , ( "body", body )
            ]
        )


respondErr : Int -> String -> String -> App -> App
respondErr id kind message =
    ProgramTest.simulateIncomingPort "apiResponse"
        (E.object
            [ ( "id", E.int id )
            , ( "kind", E.string kind )
            , ( "ok", E.bool False )
            , ( "body", E.object [ ( "message", E.string message ) ] )
            ]
        )


{-| ここまでに飛んだ封筒の kind 列を検査して記録を消す(次の検査は続きから)。 -}
ensureKinds : List String -> App -> App
ensureKinds expected =
    ProgramTest.ensureOutgoingPortValues "apiRequest" (D.field "kind" D.string) (Expect.equal expected)


expectKinds : List String -> App -> Expect.Expectation
expectKinds expected =
    ProgramTest.expectOutgoingPortValues "apiRequest" (D.field "kind" D.string) (Expect.equal expected)



-- サーバ応答のフィクスチャ


healthBody : E.Value
healthBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "dir", E.string "/proj" )
        , ( "title", E.string "テスト" )
        , ( "design", E.object [ ( "w", E.float 320 ), ( "h", E.float 240 ) ] )
        , ( "version", E.string "0.0.0" )
        ]


filesBody : E.Value
filesBody =
    E.object
        [ ( "root", E.string "/proj" )
        , ( "ui", E.list identity [] )
        , ( "hitbox", E.list E.string [ "hitbox.json" ] )
        , ( "palette", E.list E.string [] )
        ]


resourcesBody : E.Value
resourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "level" )
                    , ( "title", E.string "レベル" )
                    , ( "plugin", E.string "shooterLevel" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/level.json" )
                                , ( "schema", E.string "assets/level.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


{-| GET /changes の応答(パス → mtime)。開いているファイルの鮮度だけ見る。 -}
changesBody : String -> Int -> E.Value
changesBody path mtime =
    E.object
        [ ( "token", E.string ("t" ++ String.fromInt mtime) )
        , ( "files", E.object [ ( path, E.int mtime ) ] )
        ]


fileBody : String -> String -> E.Value
fileBody path content =
    -- mtime はサーバが常に添える(保存の ifMtime の種)
    E.object [ ( "path", E.string path ), ( "content", E.string content ), ( "mtime", E.int 111 ) ]


levelText : String
levelText =
    """{
  "meta": { "scrollSpeed": 60 },
  "routes": {
    "slow": { "type": "straight", "speed": 60 },
    "fast": { "type": "straight", "speed": 100 }
  },
  "spawns": [
    { "atX": 200, "kind": "popcorn", "y": 120, "route": "slow" },
    { "atX": 400, "kind": "turret", "y": 212, "route": "fast" }
  ]
}"""


schemaText : String
schemaText =
    """{
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
        "speed": { "type": "float", "label": "速さ", "order": 2, "min": 0, "max": 150, "required": true }
      }
    },
    "spawns": {
      "kind": "list",
      "fields": {
        "atX":   { "type": "int", "label": "出現位置", "order": 1, "required": true, "min": 0 },
        "kind":  { "type": {"enum": ["popcorn", "turret", "dome"]},
                   "label": "敵種", "order": 2, "widget": "segmented", "required": true },
        "y":     { "type": "int", "label": "高さ", "order": 3, "required": true, "min": 0, "max": 240 },
        "route": { "type": {"ref": "routes"}, "label": "軌道", "order": 4, "required": true }
      }
    }
  }
}"""


{-| docEdit 応答用: scrollSpeed を 72 に進めた本文(開いた時と違えば dirty)。 -}
levelEditedText : String
levelEditedText =
    String.replace "\"scrollSpeed\": 60" "\"scrollSpeed\": 72" levelText


{-| applyDocAppend 応答用: spawns に 3 本目(雛形)が入った本文。 -}
levelWithThirdSpawn : String
levelWithThirdSpawn =
    String.replace
        "{ \"atX\": 400, \"kind\": \"turret\", \"y\": 212, \"route\": \"fast\" }"
        "{ \"atX\": 400, \"kind\": \"turret\", \"y\": 212, \"route\": \"fast\" },\n    { \"atX\": 0, \"kind\": \"popcorn\", \"y\": 0, \"route\": \"\" }"
        levelText


{-| PUT /file 成功(200)の応答。mtime は次の保存の ifMtime になる。 -}
putOkBody : E.Value
putOkBody =
    E.object [ ( "ok", E.bool True ), ( "mtime", E.int 999 ) ]


{-| PUT /file 競合(409)の応答。currentMtime は今ディスクに居る版。 -}
putConflictBody : E.Value
putConflictBody =
    E.object
        [ ( "ok", E.bool False )
        , ( "error", E.string "conflict: ディスク上のファイルが編集開始後に変わっています" )
        , ( "currentMtime", E.int 222 )
        ]


previewBody : E.Value
previewBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "png", E.string "cGxhY2Vob2xkZXI=" )
        , ( "width", E.int 480 )
        , ( "height", E.int 240 )
        , ( "scale", E.float 1 )
        , ( "design", E.object [ ( "w", E.float 480 ), ( "h", E.float 240 ) ] )
        , ( "rects"
          , E.object [ ( "spawns/0", E.list E.float [ 196, 116, 8, 8 ] ) ]
          )
        ]



{-| GET /projects の応答。found に候補 2 件。 -}
projectsBody : E.Value
projectsBody =
    E.object
        [ ( "current", E.null )
        , ( "recent", E.list identity [] )
        , ( "found"
          , E.list identity
                [ E.object [ ( "dir", E.string "/Users/me/Desktop/flix_ge_dungeon" ), ( "title", E.string "ダンジョン" ) ]
                , E.object [ ( "dir", E.string "/Users/me/Desktop/flix_ge_other" ), ( "title", E.string "べつ" ) ]
                ]
          )
        ]


{-| GET /projects の応答(取り直し後)。消えたダンジョンは外れ、found は「べつ」だけ。 -}
projectsBodyPruned : E.Value
projectsBodyPruned =
    E.object
        [ ( "current", E.null )
        , ( "recent", E.list identity [] )
        , ( "found"
          , E.list identity
                [ E.object [ ( "dir", E.string "/Users/me/Desktop/flix_ge_other" ), ( "title", E.string "べつ" ) ]
                ]
          )
        ]


{-| Tauri list_running_games 応答。games に {pid, cwd} 配列。 -}
runningGamesBody : List String -> E.Value
runningGamesBody cwds =
    E.object
        [ ( "games"
          , E.list (\c -> E.object [ ( "pid", E.int 42 ), ( "cwd", E.string c ) ]) cwds
          )
        ]


{-| GET /atelier/slots の応答(素材スロット 1 件)。スロットのカード
(atelier-slot)はこの宣言から生えるので、素材セクションの検査はこれを
流してから行う。
-}
atelierSlotsBody : E.Value
atelierSlotsBody =
    E.object
        [ ( "slots"
          , E.list identity
                [ E.object
                    [ ( "file", E.string "assets/villager.sprite.json" )
                    , ( "title", E.string "村人の見た目" )
                    ]
                ]
          )
        ]


{-| GET /journey/state の応答。id と nav だけが仕様(文言は見ない)。 -}
journeyBody : String -> String -> E.Value
journeyBody id nav =
    E.object
        [ ( "suggestion"
          , E.object
                [ ( "id", E.string id )
                , ( "title", E.string "見出し" )
                , ( "detail", E.string "本文" )
                , ( "nav", E.string nav )
                ]
          )
        , ( "checks"
          , E.object
                [ ( "atelierCandidates", E.int 0 )
                , ( "staleGallery", E.bool False )
                , ( "diffCount", E.int 0 )
                ]
          )
        ]


{-| health は 200 だが未選択({ok:false})→ projects(2)・runningGames(3) を要求した
picker 状態。プロジェクト未選択は「サーバは居るが選べていない」で HTTP 200 の
{ok:false} が契約なので、respondErr(サーバ不在)でなく respondOk で流す。
-}
pickerBooted : App
pickerBooted =
    start
        |> ensureKinds [ "health" ]
        |> respondOk 1 "health" (E.object [ ( "ok", E.bool False ), ( "error", E.string "プロジェクト未選択" ) ])
        |> ensureKinds [ "projects", "runningGames" ]
        |> respondOk 2 "projects" projectsBody



-- 定番の途中状態(id は封筒採番の続き番号)


{-| health(1) → files(2)・resources(3) まで済んだ状態。 -}
booted : App
booted =
    bootedWith resourcesBody


{-| resources 応答だけ差し替えて起動を済ませ、アトリエの入口で止まった状態。
journeyState は採番外(id 0)の読み取り封筒で、以降の id はこれまで通り 4 から。
-}
landingWith : E.Value -> App
landingWith resources =
    start
        |> ensureKinds [ "health" ]
        |> respondOk 1 "health" healthBody
        |> ensureKinds [ "files", "resources", "journeyState" ]
        |> respondOk 2 "files" filesBody
        |> respondOk 3 "resources" resources
        |> ProgramTest.clickButton "アトリエ"
        -- アトリエは開くたび候補えらび(swap)とアーカイバの材料を取り直す(採番外 id 0)
        |> ensureKinds [ "atelierCandidates", "gameStatus", "atelierSlots", "atelierArchive", "galleryList", "journeyChanges" ]


{-| 入口から調整(Doc エディタ)へ入った状態(編集フローの検査はここから)。 -}
bootedWith : E.Value -> App
bootedWith resources =
    landingWith resources
        |> ProgramTest.clickButton "パラメータを変える"


{-| kind = ui の宣言(プラグイン・スキーマ無し)。エンジン焼き(/preview/ui)の対象。 -}
uiResourcesBody : E.Value
uiResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "ui" )
                    , ( "title", E.string "画面" )
                    , ( "files", E.list identity [ E.object [ ( "path", E.string "assets/menu.ui.json" ) ] ] )
                    ]
                ]
          )
        ]


{-| dungeon 系 kind の宣言。エンジン焼きの口を持たない = 走るゲームが本番プレビュー。 -}
dungeonResourcesBody : E.Value
dungeonResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "dungeon" )
                    , ( "title", E.string "ダンジョン" )
                    , ( "files", E.list identity [ E.object [ ( "path", E.string "assets/b1.dungeon.json" ) ] ] )
                    ]
                ]
          )
        ]


{-| kind 共有スキーマ(schemaPath)をサーバが実在確認して添えた形。 -}
dungeonDeclaredSchemaBody : E.Value
dungeonDeclaredSchemaBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "dungeon" )
                    , ( "title", E.string "ダンジョン" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/b1.dungeon.json" )
                                , ( "schema", E.string "assets/dungeon.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


{-| kind map スキーマ(sprites 型)の宣言と中身。 -}
spritesResourcesBody : E.Value
spritesResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "sprites" )
                    , ( "title", E.string "飾りと松明" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/sprites.json" )
                                , ( "schema", E.string "assets/sprites.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


spritesSchemaText : String
spritesSchemaText =
    """{"sections": {"sprites": {"kind": "map", "label": "レシピ", "item": {"kind": "record", "fields": {"unit": {"type": "text"}, "parts": {"type": "json", "required": true}}}}}}"""


spritesText : String
spritesText =
    """{"sprites": {"stairs": {"unit": "tile", "parts": []}}}"""


{-| 全セクションがフォーム未対応 kind のスキーマ。 -}
unsupportedSchemaText : String
unsupportedSchemaText =
    """{"sections": {"points": {"kind": "curve", "shape": []}}}"""


{-| kind value(単一スカラー・light の darkness 型)入りのスキーマと中身。 -}
lightSchemaText : String
lightSchemaText =
    """{"sections": {
         "darkness": {"kind": "value", "type": "float", "label": "暗幕の濃さ", "min": 0, "max": 1, "default": 0.85},
         "rim": {"kind": "record", "fields": {"alpha": {"type": "float", "label": "縁の濃さ"}}}}}"""


{-| group で束ねる単一値だけのスキーマ(タブ 2 枚に畳まれる)。 -}
groupedSchemaText : String
groupedSchemaText =
    """{"sections": {
         "darkness": {"kind": "value", "type": "float", "group": "明かり"},
         "rimAlpha": {"kind": "value", "type": "float", "group": "明かり"},
         "volume": {"kind": "value", "type": "float", "group": "音"}}}"""


groupedDocText : String
groupedDocText =
    """{"darkness": 0.55, "rimAlpha": 0.2, "volume": 0.8}"""


lightDocText : String
lightDocText =
    """{"darkness": 0.55, "rim": {"alpha": 0.2}}"""


{-| type grid(ASCII マップ)のスキーマと中身。 -}
gridSchemaText : String
gridSchemaText =
    """{"sections": {"grid": {"kind": "record", "fields": {"rows": {"type": "grid", "label": "マップ行", "required": true}}}}}"""


gridDocText : String
gridDocText =
    """{"grid": {"rows": ["##T##", "#...#"]}}"""


{-| enabledWhen(characters の sides は shape=ngon のときだけ)のスキーマと中身。 -}
charactersSchemaText : String
charactersSchemaText =
    """{"sections": {"player": {"kind": "record", "fields": {
      "shape": {"type": {"enum": ["circle", "box", "ngon"]}, "label": "体の形", "order": 1, "widget": "segmented", "default": "circle"},
      "sides": {"type": "int", "label": "角の数", "order": 2, "min": 3, "max": 12, "default": 6,
                "enabledWhen": {"field": "shape", "equals": "ngon"}},
      "radius": {"type": "float", "label": "半径", "order": 3,
                 "enabledWhen": {"field": "shape", "in": ["circle", "ngon"]}}}}}}"""


charactersDocText : String
charactersDocText =
    """{"player": {"shape": "circle", "sides": 6}}"""


charactersDocNgonText : String
charactersDocNgonText =
    String.replace "\"circle\"" "\"ngon\"" charactersDocText


{-| segmented を外した enum(ドロップダウンの select で出る)。色と同じライブ保存
経路に乗るかを見るため、segmented(ボタン)版と分けて用意する。 -}
dropdownShapeSchemaText : String
dropdownShapeSchemaText =
    String.replace ", \"widget\": \"segmented\"" "" charactersSchemaText


{-| 宣言と実ファイルのずれ(warnings)を差した resources 応答。 -}
warnedResourcesBody : E.Value
warnedResourcesBody =
    E.object
        [ ( "resources", E.list identity [] )
        , ( "warnings"
          , E.list E.string [ "『assets/cave.light.json』が JSON として読めません(読み込みは既定値へ倒れます)" ]
          )
        ]


{-| level.json を開き、本文(4)・スキーマ(5)・プレビュー(6, 取り直し 7)まで済んだ状態。 -}
openedLevel : App
openedLevel =
    booted
        |> ProgramTest.clickButton "assets/level.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/level.json" levelText)
        |> ensureKinds [ "previewItems" ]
        -- スキーマ確定は往復中なので stale 印だけが立つ
        |> respondOk 5 "getFile" (fileBody "assets/level.schema.json" schemaText)
        |> respondOk 6 "previewItems" previewBody
        |> ensureKinds [ "previewItems" ]
        |> respondOk 7 "previewItems" previewBody


{-| プラグイン宣言の無い hitbox.json を開き、本文(4)・スキーマ欠け(5)まで済んだ状態。
宣言外のファイルは既定の一覧に出ない —「🗂 すべてのファイル」を通ってから開く。
-}
openedHitbox : App
openedHitbox =
    booted
        |> ProgramTest.clickButton "🗂 すべてのファイル"
        |> ProgramTest.clickButton "hitbox.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "hitbox.json" "{ }")
        |> respondErr 5 "getFile" "スキーマが見つかりません"


{-| テキスト編集で dirty にした状態。 -}
dirtyHitbox : App
dirtyHitbox =
    openedHitbox
        |> ProgramTest.simulateDomEvent
            (Query.find [ tag "textarea", class "resize-none" ])
            ( "input", E.object [ ( "target", E.object [ ( "value", E.string "{ \"a\": 1 }" ) ] ) ] )



-- 部品操作


{-| sprites を開き、stairs の parts(type json = RawJson 欄)を選んだ状態。 -}
openedRawJson : App
openedRawJson =
    bootedWith spritesResourcesBody
        |> ProgramTest.clickButton "assets/sprites.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/sprites.json" spritesText)
        |> respondOk 5 "getFile" (fileBody "assets/sprites.schema.json" spritesSchemaText)
        |> ProgramTest.simulateDomEvent
            (Query.find [ tag "tr", containing [ text "stairs" ] ])
            ( "click", E.object [] )


typeRawJson : String -> App -> App
typeRawJson value =
    ProgramTest.simulateDomEvent
        (Query.find [ class "raw-json" ])
        ( "input", E.object [ ( "target", E.object [ ( "value", E.string value ) ] ) ] )


typeNumberBox : String -> App -> App
typeNumberBox value =
    ProgramTest.simulateDomEvent
        (Query.find [ class "number-box" ])
        ( "input", E.object [ ( "target", E.object [ ( "value", E.string value ) ] ) ] )


keydownOn : List Test.Html.Selector.Selector -> String -> App -> App
keydownOn selectors key =
    ProgramTest.simulateDomEvent
        (Query.find selectors)
        ( "keydown", E.object [ ( "key", E.string key ), ( "shiftKey", E.bool False ) ] )


discardDialogText : String
discardDialogText =
    "移動すると編集は失われます。"



-- テスト本体


suite : Test
suite =
    describe "配線フロー"
        [ test "起動: health が通ると files と resources(とホームの提案)を要求する" <|
            \() ->
                start
                    |> ensureKinds [ "health" ]
                    |> respondOk 1 "health" healthBody
                    |> expectKinds [ "files", "resources", "journeyState" ]
        , test "ホーム: 知らせ(changed)から見比べを開き、閉じると seen が飛んで提案を取り直す" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0
                        "journeyState"
                        (E.object
                            [ ( "suggestion"
                              , E.object
                                    [ ( "id", E.string "changed" )
                                    , ( "title", E.string "見た目が変わりました(1場面)" )
                                    , ( "detail", E.string "心当たりがなければ、前と今を見比べてください。" )
                                    , ( "nav", E.string "changes" )
                                    ]
                              )
                            , ( "checks"
                              , E.object
                                    [ ( "atelierCandidates", E.int 0 ) ]
                              )
                            ]
                        )
                    |> ProgramTest.clickButton "見比べる"
                    |> ensureKinds [ "journeyChanges" ]
                    |> respondOk 0
                        "journeyChanges"
                        (E.object
                            [ ( "baking", E.bool False )
                            , ( "seen", E.bool False )
                            , ( "changes"
                              , E.list identity
                                    [ E.object
                                        [ ( "name", E.string "title.png" )
                                        , ( "ver", E.int 3 )
                                        , ( "before", E.string "golden/archive/title.v3.png" )
                                        , ( "after", E.string "golden/title.png" )
                                        ]
                                    ]
                              )
                            ]
                        )
                    -- モーダルに 1 場面目(1 / 1・v3)と「前」「今」の 2 枚が出る
                    |> ProgramTest.ensureViewHas [ text "1 / 1" ]
                    |> ProgramTest.ensureViewHas [ text "v3" ]
                    |> ProgramTest.ensureViewHas [ class "changes-dialog" ]
                    |> ProgramTest.clickButton "閉じる"
                    |> ensureKinds [ "journeyChangesSeen" ]
                    |> respondOk 0 "journeyChangesSeen" (E.object [ ( "ok", E.bool True ) ])
                    |> expectKinds [ "journeyState" ]
        , test "ホーム: create(新しい一巡)は済み(✓)も現在地も点けず、始まりの一言を出す" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0
                        "journeyState"
                        (E.object
                            [ ( "suggestion"
                              , E.object
                                    [ ( "id", E.string "create" )
                                    , ( "title", E.string "新しく作ろう" )
                                    , ( "detail", E.string "まだ何も無いので、最初の候補から。" )
                                    , ( "nav", E.string "atelier" )
                                    ]
                              )
                            , ( "checks"
                              , E.object
                                    [ ( "atelierCandidates", E.int 0 )
                                    , ( "staleGallery", E.bool False )
                                    , ( "diffCount", E.int 0 )
                                    ]
                              )
                            ]
                        )
                    |> ProgramTest.ensureViewHas [ text "新しい一巡を始めましょう" ]
                    |> ProgramTest.ensureViewHasNot [ text "✓ 候補を選ぶ" ]
                    |> ProgramTest.expectViewHasNot [ class "journey-step-current" ]
        , test "ホーム: launch(起動してみましょう)は、その場で gameStart が飛び実況が出る" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0 "journeyState" (journeyBody "launch" "launch")
                    |> ProgramTest.clickButton "▶ 起動する"
                    |> ensureKinds [ "gameStart" ]
                    -- ホームに居たまま、提案カードの下に実況が出る
                    |> ProgramTest.ensureViewHas [ text "起動しています…(初回は少しかかります)" ]
                    |> ProgramTest.expectViewHas [ class "launch-line" ]
        , test "ホーム: 起動が確認できたら提案を取り直す(カードが次の一歩へ進む)" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0 "journeyState" (journeyBody "launch" "launch")
                    |> ProgramTest.clickButton "▶ 起動する"
                    |> ensureKinds [ "gameStart" ]
                    |> respondOk 4 "gameStart" (E.object [ ( "ok", E.bool True ) ])
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool True ) ])
                    |> expectKinds [ "journeyState" ]
        , test "ホーム: arrange(アレンジ)はアトリエ入口に着く" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0 "journeyState" (journeyBody "arrange" "arrange")
                    |> ProgramTest.clickButton "アレンジする"
                    |> ensureKinds [ "atelierCandidates", "gameStatus", "atelierSlots", "atelierArchive", "galleryList", "journeyChanges" ]
                    -- 入口(3枚のカード)から選んでもらう
                    |> ProgramTest.expectViewHas [ class "atelier-landing" ]
        , test "ホーム: /journey/state が無いサーバでも落ちず、アレンジの一手に倒れる" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondErr 0 "journeyState" "HTTP 404"
                    |> ProgramTest.expectViewHas [ text "ゲームをアレンジしてみましょう" ]
        , test "アトリエ: タブから開くと入口が出て、カードで素材へ、「← アトリエ」で入口へ戻る" <|
            \() ->
                landingWith resourcesBody
                    |> ProgramTest.ensureViewHas [ class "atelier-landing" ]
                    |> ProgramTest.ensureViewHas [ text "パラメータを変える" ]
                    |> respondOk 0 "atelierSlots" atelierSlotsBody
                    |> ProgramTest.clickButton "素材を切り替える"
                    |> ProgramTest.ensureViewHas [ class "atelier-slot" ]
                    |> ProgramTest.clickButton "← アトリエ"
                    |> ProgramTest.expectViewHas [ class "atelier-landing" ]
        , test "ミニプレイヤー: アトリエタブでは出て、ホームでは出ない" <|
            \() ->
                landingWith resourcesBody
                    |> ProgramTest.ensureViewHas [ class "mini-player" ]
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> ProgramTest.expectViewHasNot [ class "mini-player" ]
        , test "ミニプレイヤー: 絵をクリックすると拡大が開き、閉じると消える" <|
            \() ->
                landingWith resourcesBody
                    |> respondOk 0
                        "galleryList"
                        (E.object
                            [ ( "gallery"
                              , E.list identity [ E.object [ ( "name", E.string "title.png" ) ] ]
                              )
                            ]
                        )
                    |> ProgramTest.clickButton "🎞️ ミニプレイヤー"
                    |> ProgramTest.ensureViewHasNot [ class "mini-zoom" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "mini-shot" ])
                        ( "click", E.object [] )
                    |> ProgramTest.ensureViewHas [ class "mini-zoom" ]
                    |> ProgramTest.clickButton "閉じる"
                    |> ProgramTest.expectViewHasNot [ class "mini-zoom" ]
        , test "プロジェクトを選ぶ: 編集中から選択画面を開き、「← いまのゲームに戻る」で編集へ戻れる" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "プロジェクトを選ぶ"
                    |> ensureKinds [ "projects", "runningGames" ]
                    |> ProgramTest.ensureViewHas [ class "picker" ]
                    |> ProgramTest.clickButton "← いまのゲームに戻る"
                    -- 何も再読み込みしない(封筒ゼロ)。開いていた編集がそのまま生きている
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHasNot [ class "picker" ]
                    |> ProgramTest.expectViewHas [ class "edit-toolbar" ]
        , test "アトリエ: 入口の「ゲームを広げる」から部屋へ、「場面を足す」で下書きが届きコピーが見える" <|
            \() ->
                landingWith resourcesBody
                    |> ProgramTest.clickButton "ゲームを広げる"
                    |> ProgramTest.ensureViewHas [ text "足したいものを選ぶと、AI に渡す依頼文の下書きができます。あなたの言葉を足して仕上げてください。" ]
                    |> ProgramTest.clickButton "場面を足す"
                    -- 押した瞬間に反応(取得中でも黙らせない)
                    |> ProgramTest.ensureViewHas [ text "⏳ 下書きを作っています…" ]
                    |> ensureKinds [ "promptExtend" ]
                    |> respondOk 4
                        "promptExtend"
                        (E.object
                            [ ( "title", E.string "場面を足す" )
                            , ( "prompt", E.string "このゲームに新しい場面を 1 つ足してください。" )
                            ]
                        )
                    |> ProgramTest.ensureViewHas [ class "atelier-extend-draft" ]
                    |> ProgramTest.expectViewHas [ text "📋 プロンプトをコピー" ]
        , test "調整の境界: 素材(material)には②への行き来リンク、tuning には完結の一言だけ" <|
            \() ->
                booted
                    |> ProgramTest.clickButton "assets/level.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/level.json" levelText)
                    -- 宣言はあるが素材スロットではない = tuning。リンクは出さない
                    |> ProgramTest.ensureViewHas [ text "レベルは値を変えるだけで完結します(切り替えるものはありません)" ]
                    |> ProgramTest.ensureViewHasNot [ text "別のレベルに切り替える(まず候補を作ります)→" ]
                    -- 素材スロットの宣言が届いたら、同じファイルは行き来リンクに変わる
                    -- (題は宣言題の括弧前だけを織り込む — 実際の宣言題は全角括弧)
                    |> respondOk 0
                        "atelierSlots"
                        (E.object
                            [ ( "slots"
                              , E.list identity
                                    [ E.object
                                        [ ( "file", E.string "assets/level.json" )
                                        , ( "title", E.string "レベル\u{FF08}調整卓\u{FF09}" )
                                        ]
                                    ]
                              )
                            ]
                        )
                    |> ProgramTest.ensureViewHasNot [ text "レベルは値を変えるだけで完結します(切り替えるものはありません)" ]
                    |> ProgramTest.expectViewHas [ text "別のレベルに切り替える(まず候補を作ります)→" ]
        , test "素材のカード: 押すと開いて候補づくりが見え、ヘッダの再クリックで閉じる" <|
            \() ->
                landingWith resourcesBody
                    |> respondOk 0 "atelierSlots" atelierSlotsBody
                    |> ProgramTest.clickButton "素材を切り替える"
                    |> ProgramTest.ensureViewHasNot [ class "atelier-create" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "atelier-slot" ])
                        ( "click", E.object [] )
                    |> ProgramTest.ensureViewHas [ class "atelier-create" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "atelier-slot-head" ])
                        ( "click", E.object [] )
                    |> ProgramTest.expectViewHasNot [ class "atelier-create" ]
        , test "🗂 すべてのファイル: 着地の見出しも「🗂 すべてのファイル」になり、戻すと ⚙️ に戻る" <|
            \() ->
                booted
                    |> ProgramTest.ensureViewHas [ text "⚙️ パラメータを変える" ]
                    |> ProgramTest.clickButton "🗂 すべてのファイル"
                    |> ProgramTest.ensureViewHasNot [ text "⚙️ パラメータを変える" ]
                    |> ProgramTest.ensureViewHas [ text "🗂 すべてのファイル" ]
                    |> ProgramTest.clickButton "宣言された素材だけに戻す"
                    |> ProgramTest.expectViewHas [ text "⚙️ パラメータを変える" ]
        , test "ホーム: pick(候補を比べて選ぼう)もアトリエ入口(3枚のカード)に着地する" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0 "journeyState" (journeyBody "pick" "atelier")
                    |> ProgramTest.clickButton "アトリエへ"
                    |> ensureKinds [ "atelierCandidates", "gameStatus", "atelierSlots", "atelierArchive", "galleryList", "journeyChanges" ]
                    |> respondOk 0 "atelierSlots" atelierSlotsBody
                    |> ProgramTest.ensureViewHasNot [ class "atelier-slot" ]
                    |> ProgramTest.expectViewHas [ class "atelier-landing" ]
        , test "起動中: 走っているゲームの cwd が候補 dir と一致すると『● 起動中』が出る" <|
            \() ->
                pickerBooted
                    |> respondOk 3 "runningGames" (runningGamesBody [ "/Users/me/Desktop/flix_ge_dungeon" ])
                    |> ProgramTest.expectViewHas [ text "● 起動中" ]
        , test "起動中: 末尾スラッシュ違いの cwd でも一致してバッジが出る" <|
            \() ->
                pickerBooted
                    |> respondOk 3 "runningGames" (runningGamesBody [ "/Users/me/Desktop/flix_ge_dungeon/" ])
                    |> ProgramTest.expectViewHas [ text "● 起動中" ]
        , test "起動中: 一致する cwd が無ければバッジは出ない" <|
            \() ->
                pickerBooted
                    |> respondOk 3 "runningGames" (runningGamesBody [ "/Users/me/Desktop/nowhere" ])
                    |> ProgramTest.expectViewHasNot [ text "● 起動中" ]
        , test "起動中: 走っているゲームが空(ブラウザ環境)ならバッジは出ない" <|
            \() ->
                pickerBooted
                    |> respondOk 3 "runningGames" (runningGamesBody [])
                    |> ProgramTest.expectViewHasNot [ text "● 起動中" ]
        , test "プロジェクト選択に失敗するとエラーを見せ、候補一覧を取り直す(消えた項目は候補から外れる)" <|
            \() ->
                pickerBooted
                    |> respondOk 3 "runningGames" (runningGamesBody [])
                    |> ProgramTest.clickButton "ダンジョン"
                    |> ensureKinds [ "selectProject" ]
                    |> respondErr 4 "selectProject" "プロジェクトが見つかりません(削除または移動されています): /Users/me/Desktop/flix_ge_dungeon"
                    |> ensureKinds [ "projects" ]
                    |> respondOk 5 "projects" projectsBodyPruned
                    |> ProgramTest.ensureViewHas [ text "プロジェクトが見つかりません(削除または移動されています): /Users/me/Desktop/flix_ge_dungeon" ]
                    |> ProgramTest.expectViewHasNot [ text "ダンジョン" ]
        , test "level.json を開くと本文とスキーマの getFile が飛び、本文が届くと previewItems が飛ぶ" <|
            \() ->
                booted
                    |> ProgramTest.clickButton "assets/level.json"
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string))
                        (Expect.equal
                            [ ( "getFile", "assets/level.json" )
                            , ( "getFile", "assets/level.schema.json" )
                            ]
                        )
                    |> respondOk 4 "getFile" (fileBody "assets/level.json" levelText)
                    |> expectKinds [ "previewItems" ]
        , test "プラグイン宣言の無いファイルでは previewItems が飛ばない" <|
            \() ->
                openedHitbox
                    |> expectKinds []
        , test "スキーマ応答が届くとフォームが view に出る" <|
            \() ->
                openedLevel
                    |> ProgramTest.expectViewHas [ text "スクロール速度" ]
        , test "数値欄のタイプでは docEdit を出さず、Enter で applyDocEdit が飛ぶ" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> ensureKinds []
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "meta", "scrollSpeed" ] ) ])
        , test "ライブ反映 ON: 数値欄のタイプ(onInput)だけで applyDocEdit が飛ぶ(blur を待たない)" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> typeNumberBox "72"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "meta", "scrollSpeed" ] ) ])
        , test "ライブ反映 ON: 壊れた途中値(不完全な JSON)は反映しない" <|
            \() ->
                openedRawJson
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    -- 閉じていない JSON はパースが弾くので docEdit を出さない
                    |> typeRawJson "[{\"shape\":"
                    |> expectKinds []
        , test "ライブ反映 ON: 妥当な途中 JSON は onInput だけで applyDocEdit が飛ぶ" <|
            \() ->
                openedRawJson
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> typeRawJson "[1, 2, 3]"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "sprites", "stairs", "parts" ] ) ])
        , test "ライブ反映 ON: タイプ→docEdit 応答後に debounce 自動保存(putFile)へ流れる" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> typeNumberBox "72"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 9 "applyDocEdit" (E.object [ ( "text", E.string levelEditedText ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.advanceTime 250
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "content" ] D.string))
                        (Expect.equal [ ( "putFile", levelEditedText ) ])
        , test "ライブ反映 OFF(既定): 数値欄のタイプでは docEdit を出さない(blur/Enter まで待つ)" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> expectKinds []
        , test "docEdit 往復中の追加編集は列に積まれ、応答後に飛ぶ" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> typeNumberBox "80"
                    |> keydownOn [ class "number-box" ] "Enter"
                    -- 1 本目の応答が来るまで 2 本目は飛ばない
                    |> ensureKinds []
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelText ) ])
                    |> expectKinds [ "applyDocEdit", "previewItems" ]
        , test "スライダー(OFF): ドラッグ中(sl-input)は draft に載せるだけ、離す(sl-change)で applyDocEdit" <|
            \() ->
                openedLevel
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "sl-range" ])
                        ( "sl-input", E.object [ ( "target", E.object [ ( "value", E.float 100 ) ] ) ] )
                    -- ライブ反映 OFF なのでドラッグ中は doc へ書かない(1 手遅れ回避は
                    -- value=draft 追従で担う)
                    |> ensureKinds []
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "sl-range" ])
                        ( "sl-change", E.object [ ( "target", E.object [ ( "value", E.float 100 ) ] ) ] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "meta", "scrollSpeed" ] ) ])
        , test "スライダー(ライブ ON): ドラッグ中(sl-input)から applyDocEdit が飛ぶ(1 手遅れなし)" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "sl-range" ])
                        ( "sl-input", E.object [ ( "target", E.object [ ( "value", E.float 100 ) ] ) ] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "meta", "scrollSpeed" ] ) ])
        , test "保存: 事前の getFile 往復なしで、開いた時の mtime を ifMtime に添えた putFile が飛ぶ" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "保存"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\k c m -> ( k, c, m ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "content" ] D.string)
                            (D.at [ "payload", "ifMtime" ] D.int)
                        )
                        (Expect.equal [ ( "putFile", "{ \"a\": 1 }", 111 ) ])
        , test "保存: 409(外部変更)で競合ダイアログ → 構わず上書きは相手の版の ifMtime で putFile" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "保存"
                    |> ensureKinds [ "putFile" ]
                    |> respondOk 6 "putFile" putConflictBody
                    |> ProgramTest.ensureViewHas [ text "このファイルは別の場所で変更されています。再読み込みしてください(自分の編集で構わず上書きもできます)。" ]
                    |> ProgramTest.clickButton "構わず上書き"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\k c m -> ( k, c, m ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "content" ] D.string)
                            (D.at [ "payload", "ifMtime" ] D.int)
                        )
                        (Expect.equal [ ( "putFile", "{ \"a\": 1 }", 222 ) ])
        , test "保存: 409 → 再読込で本文と mtime を取り直し、編集は破棄される" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "保存"
                    |> ensureKinds [ "putFile" ]
                    |> respondOk 6 "putFile" putConflictBody
                    |> ProgramTest.clickButton "再読込(自分の編集を捨てる)"
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 7 "getFile" (fileBody "hitbox.json" "{ \"b\": 2 }")
                    |> ProgramTest.ensureViewHasNot [ text "未保存" ]
                    |> expectKinds []
        , test "保存成功: 宣言リソースのファイルは即反映の文言が出る" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelEditedText ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.clickButton "保存"
                    |> ensureKinds [ "putFile" ]
                    |> respondOk 10 "putFile" putOkBody
                    |> ProgramTest.expectViewHas [ text "保存しました — 走るゲームに即反映されます" ]
        , test "保存成功: 宣言に無いファイルは素の「保存しました」(即反映は謳わない)" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "保存"
                    |> ensureKinds [ "putFile" ]
                    |> respondOk 6 "putFile" putOkBody
                    |> ProgramTest.ensureViewHas [ text "保存しました" ]
                    |> ProgramTest.expectViewHasNot [ text "即反映" ]
        , test "dirty のままファイル切替: 確認ダイアログが出て、破棄を選ぶと切替が進む" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "assets/level.json"
                    -- 答えるまで getFile は飛ばない
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHas [ text discardDialogText ]
                    |> ProgramTest.clickButton "破棄して開く"
                    |> expectKinds [ "getFile", "getFile" ]
        , test "dirty のままファイル切替: やめるを選ぶと留まり編集も残る" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "assets/level.json"
                    |> ProgramTest.clickButton "やめる"
                    |> ProgramTest.ensureViewHasNot [ text discardDialogText ]
                    |> ProgramTest.ensureViewHas [ text "未保存" ]
                    |> expectKinds []
        , test "dirty のままウィザードへ: 破棄を選ぶとウィザードが開く" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "+ 新しいファイル"
                    |> ProgramTest.ensureViewHas [ text discardDialogText ]
                    |> ProgramTest.clickButton "破棄して開く"
                    |> ProgramTest.expectViewHas [ text "確認と生成" ]
        , test "リネーム確定で applyDocEdits(キー改名+参照書き換えのバッチ)が飛ぶ" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "routes"
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", containing [ text "slow" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.clickButton "改名"
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "rename-input" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "slowest" ) ] ) ] )
                    |> keydownOn [ class "rename-input" ] "Enter"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair
                            (D.field "kind" D.string)
                            (D.at [ "payload", "edits" ] (D.list D.value) |> D.map List.length)
                        )
                        -- キー改名 1 + spawns からの参照書き換え 1
                        (Expect.equal [ ( "applyDocEdits", 2 ) ])
        , test "モード: ビジュアル既定では textarea が無く、コードに切り替えると出る" <|
            \() ->
                openedLevel
                    |> ProgramTest.ensureViewHasNot [ tag "textarea", class "resize-none" ]
                    |> ProgramTest.clickButton "コード"
                    |> ProgramTest.expectViewHas [ tag "textarea", class "resize-none" ]
        , test "モード: ビジュアル(テキスト非表示)での編集も保存(ifMtime 付き putFile)に乗る" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelEditedText ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.clickButton "保存"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "content" ] D.string))
                        (Expect.equal [ ( "putFile", levelEditedText ) ])
        , test "追加(list): applyDocAppend が飛び、応答が届くと新しい行のフォームが出る" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "spawns"
                    |> ProgramTest.ensureViewHas [ text "エントリを選んでください" ]
                    |> ProgramTest.clickButton "+ 追加"
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocAppend", [ "spawns" ] ) ])
                    |> respondOk 8 "applyDocAppend" (E.object [ ( "text", E.string levelWithThirdSpawn ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.ensureViewHasNot [ text "エントリを選んでください" ]
                    |> ProgramTest.expectView
                        (Query.find [ class "entry-id" ] >> Query.has [ text "#2" ])
        , test "追加(catalog): 重複 id は理由付きで拒まれ、空いた id で applyDocEdit が飛ぶ" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "routes"
                    |> ProgramTest.clickButton "+ 追加"
                    |> ProgramTest.ensureViewHas [ text "新しいエントリの id を入れてください。値はスキーマの既定値で入ります。" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "add-id" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "slow" ) ] ) ] )
                    |> ProgramTest.clickButton "追加する"
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHas [ text "\"slow\" は既にあります" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "add-id" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "mid" ) ] ) ] )
                    |> ProgramTest.clickButton "追加する"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "routes", "mid" ] ) ])
        , test "削除ガード: 使用中の catalog id は件数付きの確認が出て、確定で applyDocRemove が飛ぶ" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "routes"
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", containing [ text "slow" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.clickButton "削除"
                    -- 答えるまで封筒は飛ばない
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHas [ text "\"slow\" は 1 箇所から使われています。削除すると参照が宙に浮きます。" ]
                    |> ProgramTest.clickButton "削除する"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocRemove", [ "routes", "slow" ] ) ])
        , test "削除: 使用 0 件(list の行)は確認なしで即 applyDocRemove が飛ぶ" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "spawns"
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", containing [ text "turret" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.clickButton "削除"
                    |> ProgramTest.ensureViewHasNot [ text "箇所から使われています" ]
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair
                            (D.field "kind" D.string)
                            (D.at [ "payload", "path" ]
                                (D.list (D.oneOf [ D.string, D.int |> D.map String.fromInt ]))
                            )
                        )
                        (Expect.equal [ ( "applyDocRemove", [ "spawns", "1" ] ) ])
        , test "古い応答(id 不一致)は無視され、正しい id の応答だけが反映される" <|
            \() ->
                booted
                    |> ProgramTest.clickButton "assets/level.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 99 "getFile" (fileBody "assets/level.json" "STALE")
                    -- 古い応答では開いた扱いにならない(プレースホルダのまま)
                    |> ProgramTest.ensureViewHas [ text "左の一覧からファイルを選んでください" ]
                    |> respondOk 4 "getFile" (fileBody "assets/level.json" levelText)
                    |> ProgramTest.expectViewHasNot [ text "左の一覧からファイルを選んでください" ]
        , test "kind=ui: 本文が届くと doc 入りの previewUi(エンジン焼き)が飛ぶ" <|
            \() ->
                bootedWith uiResourcesBody
                    |> ProgramTest.clickButton "assets/menu.ui.json"
                    -- schema 未宣言のファイルはスキーマを取りに行かない(404 を作らない)
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/menu.ui.json" "{ }")
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "scale" ] D.int))
                        (Expect.equal [ ( "previewUi", 2 ) ])
        , test "kind=ui: エンジン焼きが右ペインに出て、専用エディタ flix_ge_editor の案内も出る" <|
            \() ->
                bootedWith uiResourcesBody
                    |> ProgramTest.clickButton "assets/menu.ui.json"
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/menu.ui.json" "{ }")
                    |> ensureKinds [ "previewUi" ]
                    |> respondOk 5 "previewUi" previewBody
                    |> ProgramTest.ensureViewHas [ text "エンジンプレビュー" ]
                    |> ProgramTest.expectViewHas [ text "flix_ge_editor" ]
        , test "dungeon 系 kind(schema 未宣言): スキーマもプレビューも取りに行かず、走るゲーム案内とテキスト編集が出る" <|
            \() ->
                bootedWith dungeonResourcesBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    -- 隣接の b1.dungeon.schema.json を推測して 404 を作らない
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" "{ }")
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHasNot [ text "開けません" ]
                    |> ProgramTest.ensureViewHas [ text "走るゲームが本番プレビュー" ]
                    |> ProgramTest.ensureViewHas [ text "保存すると即反映されます" ]
                    |> ProgramTest.expectViewHas [ tag "textarea", class "resize-none" ]
        , test "kind 共有スキーマ: 宣言の schema パスを取りに行く(隣接 b1.dungeon.schema.json は推測しない)" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string))
                        (Expect.equal
                            [ ( "getFile", "assets/b1.dungeon.json" )
                            , ( "getFile", "assets/dungeon.schema.json" )
                            ]
                        )
        , test "kind map のスキーマ: catalog として表(エントリ名)が出る" <|
            \() ->
                bootedWith spritesResourcesBody
                    |> ProgramTest.clickButton "assets/sprites.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/sprites.json" spritesText)
                    |> respondOk 5 "getFile" (fileBody "assets/sprites.schema.json" spritesSchemaText)
                    |> ProgramTest.ensureViewHasNot [ text "読めません" ]
                    |> ProgramTest.expectViewHas [ text "stairs" ]
        , test "type json(自由形): 生 JSON は 1 行 input でなく複数行の textarea で編集できる" <|
            \() ->
                bootedWith spritesResourcesBody
                    |> ProgramTest.clickButton "assets/sprites.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/sprites.json" spritesText)
                    |> respondOk 5 "getFile" (fileBody "assets/sprites.schema.json" spritesSchemaText)
                    -- 行(stairs)を選ぶと右に parts の生 JSON 欄が出る。
                    -- 1 行 input でなく複数行 textarea であること(タグと class で pin)
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", containing [ text "stairs" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.expectView
                        (Query.find [ class "raw-json" ] >> Query.has [ tag "textarea" ])
        , test "未対応 kind だけのスキーマ: フォームに出さず、件数の 1 行+テキスト編集(分割)になる" <|
            \() ->
                bootedWith spritesResourcesBody
                    |> ProgramTest.clickButton "assets/sprites.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/sprites.json" "{ }")
                    |> respondOk 5 "getFile" (fileBody "assets/sprites.schema.json" unsupportedSchemaText)
                    |> ProgramTest.ensureViewHasNot [ text "壊れて" ]
                    -- 未対応セクションはタブにも出さない
                    |> ProgramTest.ensureViewHasNot [ text "points" ]
                    |> ProgramTest.ensureViewHas
                        [ text "フォーム未対応の項目が 1 件あります(テキスト編集で編集できます)" ]
                    |> ProgramTest.expectViewHas [ tag "textarea", class "resize-none" ]
        , test "resources の warnings(宣言と実ファイルのずれ)が左レールに出る" <|
            \() ->
                bootedWith warnedResourcesBody
                    |> ProgramTest.expectViewHas
                        [ text "『assets/cave.light.json』が JSON として読めません(読み込みは既定値へ倒れます)" ]
        , test "保存トースト: 3.5 秒で自動で消える" <|
            \() ->
                dirtyHitbox
                    |> ProgramTest.clickButton "保存"
                    |> ensureKinds [ "putFile" ]
                    |> respondOk 6 "putFile" putOkBody
                    |> ProgramTest.ensureViewHas [ text "保存しました" ]
                    |> ProgramTest.advanceTime 3500
                    |> ProgramTest.expectViewHasNot [ text "保存しました" ]
        , test "type grid: 編集の blur で改行区切りが行の列(List String)として書き戻る" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" gridDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" gridSchemaText)
                    |> ProgramTest.simulateDomEvent (Query.find [ class "grid-box" ]) ( "focus", E.object [] )
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "grid-box" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "##T##\n#..X#" ) ] ) ] )
                    |> ProgramTest.simulateDomEvent (Query.find [ class "grid-box" ]) ( "blur", E.object [] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair
                            (D.at [ "payload", "path" ] (D.list D.string))
                            (D.at [ "payload", "value" ] (D.list D.string))
                        )
                        (Expect.equal [ ( [ "grid", "rows" ], [ "##T##", "#..X#" ] ) ])
        , test "kind value: タブに出て、編集の Enter で文書直下のキーへ書き戻る" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" lightDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" lightSchemaText)
                    -- 単一値は group 未指定なので「基本」タブへ束ねる(値ごとにタブを割らない)
                    |> ProgramTest.ensureViewHas [ text "基本" ]
                    |> typeNumberBox "0.7"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair
                            (D.at [ "payload", "path" ] (D.list D.string))
                            (D.at [ "payload", "value" ] D.float)
                        )
                        (Expect.equal [ ( [ "darkness" ], 0.7 ) ])
        , test "外部変更の見張り: 打ちかけが無ければ黙って読み直す" <|
            \() ->
                openedLevel
                    |> respondOk 0 "changes" (changesBody "assets/level.json" 222)
                    -- 取り直しの getFile が飛ぶ(何もしなくても最新になる)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.at [ "kind" ] D.string)
                        (\kinds -> Expect.equal (List.filter ((==) "getFile") kinds) [ "getFile" ])
        , test "外部変更の見張り: mtime が同じなら何も起きない" <|
            \() ->
                openedLevel
                    |> respondOk 0 "changes" (changesBody "assets/level.json" 111)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.at [ "kind" ] D.string)
                        (\kinds -> Expect.equal (List.filter ((==) "getFile") kinds) [])
        , test "外部変更の見張り: 打ちかけがあれば読み直さず帯で知らせる" <|
            \() ->
                dirtyHitbox
                    |> respondOk 0 "changes" (changesBody "hitbox.json" 222)
                    -- 勝手に取り直さない(打ちかけを捨てない)
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.at [ "kind" ] D.string)
                        (\kinds -> Expect.equal (List.filter ((==) "getFile") kinds) [])
                    |> ProgramTest.expectViewHas [ text "このファイルは外で変わりました" ]
        , test "セクションタブ: 一覧を持つ種類は label があれば表示名・無ければキー名で出る" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" lightDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" lightSchemaText)
                    -- rim(record)は label 無し → キー名 rim がタブに出る
                    |> ProgramTest.expectViewHas [ text "rim" ]
        , test "group: 同じ group の単一値は 1 枚のタブに束ねる(値ごとにタブを割らない)" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" groupedDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" groupedSchemaText)
                    -- 3 つの値が「明かり」と「音」の 2 枚に畳まれる(既定の「基本」は出ない)
                    |> ProgramTest.ensureViewHas [ text "明かり" ]
                    |> ProgramTest.expectViewHas [ text "音" ]
        , test "ペイン幅は可動域に丸まる(左 160..480・右 240..640)" <|
            \() ->
                [ Main.clampPaneWidth Main.LeftPane 20
                , Main.clampPaneWidth Main.LeftPane 9999
                , Main.clampPaneWidth Main.RightPane 20
                , Main.clampPaneWidth Main.RightPane 9999
                ]
                    |> Expect.equal [ 160, 480, 240, 640 ]
        , test "覚えていた幅(uiPrefs・id 0 の一方向封筒)が左レールに復元される" <|
            \() ->
                booted
                    |> respondOk 0 "uiPrefs" (E.object [ ( "leftW", E.int 300 ), ( "rightW", E.int 400 ) ])
                    |> ProgramTest.expectView
                        (Query.find [ class "pane-files" ]
                            >> Query.has [ Test.Html.Selector.style "width" "300px" ]
                        )
        , test "ライブ反映 ON: 編集は即保存されず ~250ms 後に自動保存され、トーストは出ない" <|
            \() ->
                openedHitbox
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "textarea", class "resize-none" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "{ \"a\": 1 }" ) ] ) ] )
                    -- debounce 中はまだ飛ばない
                    |> ensureKinds []
                    |> ProgramTest.advanceTime 250
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "ifMtime" ] D.int))
                        (Expect.equal [ ( "putFile", 111 ) ])
                    |> respondOk 7 "putFile" putOkBody
                    |> ProgramTest.expectViewHasNot [ text "保存しました" ]
        , test "ライブ反映: 連続編集は 1 回の自動保存に畳まれる(最後の編集から ~250ms)" <|
            \() ->
                openedHitbox
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "textarea", class "resize-none" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "{ \"a\": 1 }" ) ] ) ] )
                    |> ProgramTest.advanceTime 150
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "textarea", class "resize-none" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "{ \"a\": 2 }" ) ] ) ] )
                    -- 1 本目の期限が来ても(予約番号がずれているので)保存しない
                    |> ProgramTest.advanceTime 150
                    |> ensureKinds []
                    |> ProgramTest.advanceTime 150
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "content" ] D.string))
                        (Expect.equal [ ( "putFile", "{ \"a\": 2 }" ) ])
        , test "active-docs: いま画面に出ているファイルに 🎮 マークが付く" <|
            \() ->
                bootedWith dungeonResourcesBody
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool True ) ])
                    |> ProgramTest.update Main.ActivePollTick
                    |> ensureKinds [ "activeDocs", "gameStatus" ]
                    |> respondOk 4
                        "activeDocs"
                        (fileBody "debug/active-docs.json" """{"active": {"dungeon": "assets/b1.dungeon.json"}}""")
                    |> ProgramTest.ensureViewHas [ text "assets/b1.dungeon.json" ]
                    |> ProgramTest.expectViewHas [ text "🎮 表示中" ]
        , test "active-docs: ゲームが止まるとバッジは消え、また起動すれば戻る" <|
            \() ->
                bootedWith dungeonResourcesBody
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool True ) ])
                    |> ProgramTest.update Main.ActivePollTick
                    |> ensureKinds [ "activeDocs", "gameStatus" ]
                    |> respondOk 4
                        "activeDocs"
                        (fileBody "debug/active-docs.json" """{"active": {"dungeon": "assets/b1.dungeon.json"}}""")
                    |> ProgramTest.ensureViewHas [ text "🎮 表示中" ]
                    -- 停止が伝わった瞬間、active-docs の中身に関わらず引っ込める
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool False ) ])
                    |> ProgramTest.ensureViewHasNot [ text "🎮 表示中" ]
                    -- 止まっている間は activeDocs を読み直さない(状態だけ問う)
                    |> ProgramTest.update Main.ActivePollTick
                    |> ensureKinds [ "gameStatus" ]
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool True ) ])
                    |> ProgramTest.expectViewHas [ text "🎮 表示中" ]
        , test "active-docs: 開いているファイルが表示中でないとヘッダに注意が出る" <|
            \() ->
                bootedWith dungeonResourcesBody
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool True ) ])
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" "{ }")
                    |> ProgramTest.update Main.ActivePollTick
                    |> ensureKinds [ "activeDocs", "gameStatus" ]
                    |> respondOk 5
                        "activeDocs"
                        (fileBody "debug/active-docs.json" """{"active": {"dungeon": "assets/b2.dungeon.json"}}""")
                    |> ProgramTest.expectViewHas
                        [ text "画面はこのファイルを表示していません(表示中: b2.dungeon.json)" ]
        , test "active-docs: 404 や形違いは何も出さない(fail-open)" <|
            \() ->
                bootedWith dungeonResourcesBody
                    |> respondOk 0 "gameStatus" (E.object [ ( "running", E.bool True ) ])
                    |> ProgramTest.update Main.ActivePollTick
                    |> ensureKinds [ "activeDocs", "gameStatus" ]
                    |> respondErr 4 "activeDocs" "HTTP 404: /file — read failed"
                    |> ProgramTest.ensureViewHasNot [ text "🎮" ]
                    |> ProgramTest.update Main.ActivePollTick
                    |> ensureKinds [ "activeDocs", "gameStatus" ]
                    |> respondOk 5 "activeDocs" (fileBody "debug/active-docs.json" "{ \"noActive\": 1 }")
                    |> ProgramTest.expectViewHasNot [ text "🎮" ]
        , test "enabledWhen: 条件を満たさないフィールドはフォームに出ず、shape 切替で現れる" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" charactersDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" charactersSchemaText)
                    -- shape=circle の間、効かない sides(equals ngon)は出さない。
                    -- radius(in [circle, ngon])は circle でも出る
                    |> ProgramTest.ensureViewHasNot [ text "角の数" ]
                    |> ProgramTest.ensureViewHas [ text "半径" ]
                    |> ProgramTest.clickButton "ngon"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 6 "applyDocEdit" (E.object [ ( "text", E.string charactersDocNgonText ) ])
                    -- ngon では sides も radius も出る(in の一致)
                    |> ProgramTest.ensureViewHas [ text "角の数" ]
                    |> ProgramTest.expectViewHas [ text "半径" ]
        , test "enabledWhen in: 配列のどれにも一致しない shape ではフォームに出ない" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" """{"player": {"shape": "box"}}""")
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" charactersSchemaText)
                    -- shape=box は in [circle, ngon] に無い → radius は出ない
                    |> ProgramTest.expectViewHasNot [ text "半径" ]
        , test "ドロップダウン enum: select の change で applyDocEdit が飛ぶ(色と同じライブ保存経路)" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" charactersDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" dropdownShapeSchemaText)
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "select" ])
                        ( "change", E.object [ ( "target", E.object [ ( "value", E.string "ngon" ) ] ) ] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocEdit", [ "player", "shape" ] ) ])
        , test "ドロップダウン enum(ライブ ON): change → applyDocEdit 応答 → 自動保存(putFile)まで届く" <|
            \() ->
                bootedWith dungeonDeclaredSchemaBody
                    |> ProgramTest.clickButton "assets/b1.dungeon.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/b1.dungeon.json" charactersDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/dungeon.schema.json" dropdownShapeSchemaText)
                    |> ProgramTest.clickButton "ライブ反映"
                    |> ensureKinds [ "saveUiPrefs" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "select" ])
                        ( "change", E.object [ ( "target", E.object [ ( "value", E.string "ngon" ) ] ) ] )
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 7 "applyDocEdit" (E.object [ ( "text", E.string charactersDocNgonText ) ])
                    |> ProgramTest.advanceTime 300
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.field "kind" D.string)
                        (Expect.equal [ "putFile" ])
        ]
