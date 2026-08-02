module FlowTest exposing (suite)

{-| 配線フローのテスト(elm-program-test)。

update が返す Effect を模擬ポートへ流し、apiResponse を注入して
「どの操作でどの封筒が飛ぶ / 飛ばないか」をブラウザ無しで検査する。
封筒 id は update の採番(reqCounter)が決定的なので、フローごとに追って書く。

-}

import Atelier
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

        Effect.Delayed info ->
            SimulatedEffect.Process.sleep info.afterMs
                |> SimulatedEffect.Task.perform (\_ -> Main.SfxWaitTick info.seq)

        Effect.SearchDebounce info ->
            SimulatedEffect.Process.sleep info.afterMs
                |> SimulatedEffect.Task.perform (\_ -> Main.SearchDebounced info.seq)

        Effect.FramePeek info ->
            SimulatedEffect.Process.sleep info.afterMs
                |> SimulatedEffect.Task.perform (\_ -> Main.FramePeeked info.seq)

        Effect.WakePoll info ->
            SimulatedEffect.Process.sleep info.afterMs
                |> SimulatedEffect.Task.perform (\_ -> Main.WakePolled info.seq)

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
                    , ( "pattern", E.string "assets/*.json" )
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


{-| GET /changes の応答(パス → mtime、複数)。一覧の増減の見張り(syncFileListIfNeeded)
の検査用 — キー集合そのものを検査材料にする。
-}
changesBodyMany : List ( String, Int ) -> E.Value
changesBodyMany files =
    E.object
        [ ( "token", E.string "t" )
        , ( "files", E.object (files |> List.map (\( path, mtime ) -> ( path, E.int mtime ) )) )
        ]


{-| GET /changes の応答。token をファイルの mtime から独立に指定できる版
(「前回の焼きから何も変わっていないか」= bakedToken の突き合わせの検査用。
token は他のファイルの変化でも動く集計値なので、同じファイルの同じ mtime の
まま token だけ変える/変えない、を作れる必要がある)。
-}
changesBodyToken : String -> Int -> String -> E.Value
changesBodyToken path mtime token =
    E.object
        [ ( "token", E.string token )

        -- 一覧の見張り(syncFileListIfNeeded)を誤って起こさないよう、cutsResourcesBody
        -- が知っている全ファイル(場面 + hitbox.json)を key 集合として揃えておく
        , ( "files", E.object [ ( path, E.int mtime ), ( "hitbox.json", E.int 1 ) ] )
        ]


{-| changesBodyToken の複数ファイル版(twoCutsResourcesBody のような 2 本立ての
resources で、一覧の見張りを誤って起こさないよう全ファイルの key を揃える)。
-}
changesBodyTokenMany : List ( String, Int ) -> String -> E.Value
changesBodyTokenMany files token =
    E.object
        [ ( "token", E.string token )
        , ( "files", E.object (( "hitbox.json", E.int 1 ) :: (files |> List.map (\( path, mtime ) -> ( path, E.int mtime )))) )
        ]


{-| resourcesBody に "assets/level2.json" が増えた版(一覧の増減の見張りの検査用)。 -}
resourcesBodyWithLevel2 : E.Value
resourcesBodyWithLevel2 =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "level" )
                    , ( "pattern", E.string "assets/*.json" )
                    , ( "title", E.string "レベル" )
                    , ( "plugin", E.string "shooterLevel" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/level.json" )
                                , ( "schema", E.string "assets/level.schema.json" )
                                ]
                            , E.object
                                [ ( "path", E.string "assets/level2.json" )
                                , ( "schema", E.string "assets/level.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
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
                         "help": "背景が流れる速さ。上げるほど道中が短くなる。",
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
        |> ensureKinds [ "projects", "runningGames", "workspace" ]
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
        |> ensureKinds [ "files", "resources", "goldenStatus", "journeyState" ]
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


{-| マップ 1 枚(ビジュアルのマップエディタが開く宣言)。 -}
mapResourcesBody : E.Value
mapResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "rooms" )
                    , ( "title", E.string "部屋" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/room.map.json" )
                                , ( "schema", E.string "assets/map.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


{-| 曲の文書 1 本と、project.json が宣言している音(サーバが /resources で返す形)。 -}
musicResourcesBody : E.Value
musicResourcesBody =
    E.object
        [ ( "sounds", E.list E.string [ "moonlight" ] )
        , ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "music" )
                    , ( "pattern", E.string "assets/*.music.json" )
                    , ( "title", E.string "音楽" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/kaidan.music.json" )
                                , ( "schema", E.string "assets/music.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


{-| 1 行 = 1 種類の一覧(oneOf)を持つ Doc。カットの並びを 1 本だけ持つ。 -}
cutsResourcesBody : E.Value
cutsResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "scenes" )
                    , ( "bakeUrl", E.string "http://127.0.0.1:8792/cutscene" )
                    , ( "performUrl", E.string "http://127.0.0.1:7777/scene" )
                    , ( "pattern", E.string "assets/*.scene.json" )
                    , ( "title", E.string "場面" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/prologue.scene.json" )
                                , ( "schema", E.string "assets/scene.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


cutsSchemaText : String
cutsSchemaText =
    """{
  "version": 1,
  "sections": {
    "cuts": { "kind": "list", "oneOf": true, "label": "カット",
              "fields": {
                "wait":   { "type": "float", "label": "待つ", "order": 1 },
                "say":    { "type": {"list": "text"}, "label": "言葉", "order": 2 },
                "pan":    { "type": "float", "label": "見回す速さ", "order": 9 } } }
  }
}"""


cutsText : String
cutsText =
    """{
  "id": "prologue",
  "room": "hall1",
  "cuts": [
    { "wait": 1.0 },
    { "say": ["……だれか いるの?"], "pan": 1.4 }
  ]
}"""


{-| oneOf の表(#/カット/内容 の 3 列)の検査用。鍵ごとの列見出しに戻っていないか、
カット列・内容列の要約が出るかを見る(cutsSchemaText よりフィールドを増やした版)。
-}
oneOfSchemaText : String
oneOfSchemaText =
    """{
  "version": 1,
  "sections": {
    "cuts": { "kind": "list", "oneOf": true, "label": "カット",
              "fields": {
                "wait":   { "type": "float", "label": "待つ(秒。とても長い説明がここに入り、見出しでは省略されてほしい)", "order": 1 },
                "walkTo": { "type": "object", "label": "歩く先のマス", "order": 2 },
                "say":    { "type": {"list": "text"}, "label": "言葉", "order": 3 },
                "noWait": { "type": "bool", "label": "待たない(添え鍵)", "order": 4 }
              }
    }
  }
}"""


oneOfDocText : String
oneOfDocText =
    """{
  "id": "prologue",
  "room": "hall1",
  "cuts": [
    { "wait": 1.0 },
    { "walkTo": { "x": 35, "y": 4 }, "noWait": true },
    { "say": ["……だれか いるの?"] }
  ]
}"""


{-| cutsResourcesBody の 2 本立て版(ファイル切り替え中に焼きが裏で進む検査用)。 -}
twoCutsResourcesBody : E.Value
twoCutsResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "scenes" )
                    , ( "bakeUrl", E.string "http://127.0.0.1:8792/cutscene" )
                    , ( "performUrl", E.string "http://127.0.0.1:7777/scene" )
                    , ( "pattern", E.string "assets/*.scene.json" )
                    , ( "title", E.string "場面" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/prologue.scene.json" )
                                , ( "schema", E.string "assets/scene.schema.json" )
                                ]
                            , E.object
                                [ ( "path", E.string "assets/second.scene.json" )
                                , ( "schema", E.string "assets/scene.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


secondCutsText : String
secondCutsText =
    """{
  "id": "second",
  "room": "hall1",
  "cuts": [ { "wait": 1.0 } ]
}"""


{-| oneOf の Doc を開き、2 行目(言葉 + 添え物)を選んだ状態。 -}
openedCuts : App
openedCuts =
    bootedWith cutsResourcesBody
        |> ProgramTest.clickButton "assets/prologue.scene.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
        |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
        -- 開いた拍に「前回の焼き」を探す(この見本には無い)
        |> ensureKinds [ "mediaExists" ]
        |> respondOk 6 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
        |> ProgramTest.simulateDomEvent
            (Query.find [ tag "tr", containing [ text "だれか" ] ])
            ( "click", E.object [] )


{-| マスの欄を持つ場面(walkTo)。部屋の間取りは横断辞書から引く。 -}
tilePickSchemaText : String
tilePickSchemaText =
    """{
  "version": 1,
  "sections": {
    "room": { "kind": "field", "type": {"ref": "rooms"}, "label": "部屋" },
    "cuts": { "kind": "list", "oneOf": true, "label": "カット",
              "fields": { "walkTo": { "type": "object", "label": "歩く先", "order": 1 } } }
  }
}"""


tilePickText : String
tilePickText =
    """{
  "id": "prologue",
  "room": "hall1",
  "cuts": [ { "walkTo": { "x": 6, "y": 7 } } ]
}"""


{-| 20 列の間取りを持つ部屋(横断辞書の 1 冊)。 -}
roomsText : String
roomsText =
    """{ "rows": [ "####################", "#..................#" ] }"""


openedTilePick : App
openedTilePick =
    bootedWith tilePickResourcesBody
        |> ProgramTest.clickButton "assets/prologue.scene.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" tilePickText)
        |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" tilePickSchemaText)
        -- ref の参照先(部屋)を横断辞書に読み込む
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 6 "getFile" (fileBody "assets/hall1.map.json" roomsText)
        |> respondOk 7 "getFile" (fileBody "assets/map.schema.json" tilePickSchemaText)
        |> ProgramTest.clickButton "カット"
        |> ProgramTest.simulateDomEvent
            (Query.find [ tag "tr", containing [ text "#0" ] ])
            ( "click", E.object [] )


tilePickResourcesBody : E.Value
tilePickResourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "scenes" )
                    , ( "pattern", E.string "assets/*.scene.json" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/prologue.scene.json" )
                                , ( "schema", E.string "assets/scene.schema.json" )
                                ]
                            ]
                      )
                    ]
                , E.object
                    [ ( "id", E.string "rooms" )
                    , ( "pattern", E.string "assets/*.map.json" )
                    , ( "files"
                      , E.list identity
                            [ E.object
                                [ ( "path", E.string "assets/hall1.map.json" )
                                , ( "schema", E.string "assets/map.schema.json" )
                                ]
                            ]
                      )
                    ]
                ]
          )
        ]


{-| GET /golden/status の応答(採番外 id 0 の読み取り封筒で届く)。 -}
goldenEnvelope : E.Value -> D.Value
goldenEnvelope body =
    E.object
        [ ( "id", E.int 0 )
        , ( "kind", E.string "goldenStatus" )
        , ( "ok", E.bool True )
        , ( "body", body )
        ]


goldenStatusBody : E.Value
goldenStatusBody =
    E.object
        [ ( "enabled", E.bool True )
        , ( "now", E.int 1785400000 )
        , ( "total", E.int 3 )
        , ( "broken", E.int 1 )
        , ( "items"
          , E.list identity
                [ E.object
                    [ ( "name", E.string "chem.png" )
                    , ( "kind", E.string "image" )
                    , ( "match", E.bool True )
                    , ( "goldenMtime", E.int 1785237743 )
                    ]
                , E.object
                    [ ( "name", E.string "pit.png" )
                    , ( "kind", E.string "image" )
                    , ( "match", E.bool False )
                    , ( "goldenMtime", E.int 1785237743 )
                    , ( "since", E.int 1785140000 )
                    ]
                , E.object
                    [ ( "name", E.string "step1.wav" )
                    , ( "kind", E.string "sound" )
                    , ( "match", E.bool True )
                    , ( "goldenMtime", E.int 1785237743 )
                    ]
                ]
          )
        ]


{-| 1 枚焼きの予約を先に済ませた状態(時計を進めるテスト用)。 -}
cutsWarm : App
cutsWarm =
    openedCuts
        |> ProgramTest.advanceTime 200
        |> ensureKinds [ "cutsceneFrame" ]
        |> respondOk 7 "cutsceneFrame" frameShotBody


{-| POST /scene の応答(取り次ぎが text/plain の本文をそのまま包む)。 -}
performOkBody : E.Value
performOkBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "reachable", E.bool True )
        , ( "body", E.string "ok\n" )
        ]


performErrorBody : E.Value
performErrorBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "reachable", E.bool True )
        , ( "body", E.string "error: 脚本が読めません\n" )
        ]


{-| POST /bake/wake の応答(撃ったが、まだ立ち上がっていない)。 -}
wakeWaitingBody : E.Value
wakeWaitingBody =
    E.object
        [ ( "ok", E.bool False )
        , ( "reachable", E.bool False )
        , ( "woke", E.bool True )
        , ( "log", E.string "『make hosts』で起こしています" )
        ]


{-| POST /bake/wake の応答(既に起きていた場合)。 -}
wakeOkBody : E.Value
wakeOkBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "reachable", E.bool True )
        , ( "woke", E.bool False )
        , ( "waitedMs", E.int 0 )
        , ( "log", E.string "既に起きています" )
        ]


{-| POST /cutscene/frame の応答(1 枚焼き)。 -}
frameShotBody : E.Value
frameShotBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "reachable", E.bool True )
        , ( "body", E.string """{"png":"debug/cutscene/prologue-cut2.png","cut":2,"notes":[]}""" )
        ]


{-| POST /bake/proxy の応答(焼き係の本文を包んだ形)。カット 2 を飛ばした。 -}
bakeOkBody : E.Value
bakeOkBody =
    E.object
        [ ( "ok", E.bool True )
        , ( "reachable", E.bool True )
        , ( "body"
          , E.string
                ("""{"baked":[{"gif":"debug/cutscene/prologue.gif","frames":679,"""
                    ++ """"notes":["カット 2 を飛ばした: 着けないマス"]}]}"""
                )
          )
        ]


{-| /sfx/shape の応答(2 秒の曲・包絡 4 本)。 -}
moonlightShapeBody : E.Value
moonlightShapeBody =
    E.object
        [ ( "name", E.string "moonlight.wav" )
        , ( "sampleRate", E.int 22050 )
        , ( "ms", E.float 2000 )
        , ( "peak", E.float 0.8 )
        , ( "bandLo", E.float 60 )
        , ( "bandHi", E.float 8000 )
        , ( "peaks", E.list E.float [ 0.2, 0.8, 0.5, 0.1 ] )
        , ( "bands", E.list identity [ E.list E.float [ 0.1, 0.2, 0.3, 0.4 ] ] )
        ]


{-| 曲の一覧(map kind)を 1 つ持つスキーマ。grid 欄は無い = つまみ系。 -}
musicSchemaText : String
musicSchemaText =
    """{
  "version": 1,
  "sections": {
    "bpm":   { "kind": "field", "type": "float", "label": "速さ", "min": 20, "max": 240 },
    "tunes": { "kind": "map", "label": "曲",
               "item": { "kind": "record", "fields": {
                 "looping": { "type": "bool", "label": "繰り返す" },
                 "decay":   { "type": "float", "label": "余韻", "min": 0, "max": 2 } } } }
  }
}"""


musicText : String
musicText =
    """{
  "bpm": 60,
  "tunes": { "moonlight": { "looping": true, "decay": 1.0,
    "notes": [
      { "at": 0.0, "len": 4.0, "midi": 37, "gain": 0.6 },
      { "at": 2.0, "len": 2.0, "midi": 61, "gain": 0.4 }
    ] } }
}"""


{-| 曲の文書を開き、一覧の moonlight を選んだ状態。 -}
openedMusic : App
openedMusic =
    bootedWith musicResourcesBody
        |> ProgramTest.clickButton "assets/kaidan.music.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/kaidan.music.json" musicText)
        |> respondOk 5 "getFile" (fileBody "assets/music.schema.json" musicSchemaText)
        |> ProgramTest.clickButton "曲"
        |> ProgramTest.simulateDomEvent
            (Query.find [ tag "tr", containing [ text "moonlight" ] ])
            ( "click", E.object [] )
        -- 行を選ぶと、その音の実測(波形)を同じ /sfx/shape で取りに行く
        |> ensureKinds [ "sfxShape" ]
        |> respondOk 6 "sfxShape" moonlightShapeBody


{-| kaidan の map と同じ骨格: マスを見ない行(on:enter)が 1 行だけ入った triggers。 -}
mapText : String
mapText =
    """{
  "rows": [ "####", "#..#", "####" ],
  "triggers": [
    { "on": "enter", "needs": "", "says": [ "だれも いない" ], "gives": "", "once": true }
  ]
}"""


{-| triggers は x,y を宣言するが必須ではない(= マスを見ない行も書ける)。 -}
mapSchemaText : String
mapSchemaText =
    """{
  "version": 1,
  "sections": {
    "rows": { "kind": "field", "type": "grid", "label": "間取り" },
    "triggers": {
      "kind": "list",
      "label": "マスの仕掛け",
      "fields": {
        "x": { "type": "int", "label": "x", "order": 1, "min": 0 },
        "y": { "type": "int", "label": "y", "order": 2, "min": 0 },
        "on": { "type": {"enum": ["step", "inspect", "enter"]}, "default": "step",
                "label": "発火のしかた", "order": 3, "required": true },
        "needs": { "type": "text", "label": "要る持ち物", "order": 4 },
        "says": { "type": {"list": "text"}, "label": "言葉", "order": 5 },
        "gives": { "type": "text", "label": "渡す物", "order": 6 },
        "once": { "type": "bool", "label": "一度きり", "order": 7, "default": true }
      }
    }
  }
}"""


{-| マップを開き、配置レイヤーへ切り替えた状態(パレットに triggers のチップが出る)。 -}
openedMap : App
openedMap =
    bootedWith mapResourcesBody
        |> ProgramTest.clickButton "assets/room.map.json"
        |> ensureKinds [ "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/room.map.json" mapText)
        |> respondOk 5 "getFile" (fileBody "assets/map.schema.json" mapSchemaText)
        -- レイヤーの行はボタンでなく行そのものが押せる(👁 だけが別ボタン)
        |> ProgramTest.simulateDomEvent
            (Query.find [ class "map-layer", containing [ text "配置" ] ])
            ( "click", E.object [] )


{-| 封筒の kind と payload.path と値(値は JSON 1 行に畳んで比べる)。 -}
kindPathValue : D.Decoder ( String, List String, String )
kindPathValue =
    D.map3 (\kind path value -> ( kind, path, value ))
        (D.field "kind" D.string)
        (D.at [ "payload", "path" ] (D.list (D.oneOf [ D.string, D.int |> D.map String.fromInt ])))
        (D.at [ "payload", "value" ] D.value |> D.map (E.encode 0))


{-| 封筒の kind と payload.path(添字は文字列に揃える)。 -}
kindAndPath : D.Decoder ( String, List String )
kindAndPath =
    D.map2 Tuple.pair
        (D.field "kind" D.string)
        (D.at [ "payload", "path" ] (D.list (D.oneOf [ D.string, D.int |> D.map String.fromInt ])))


{-| 部屋の行(一覧の 1 行)を押す。見出しは #添字 と要約の 2 つの span なので
clickButton の文字合わせでは当たらない。
-}
clickRoomRow : App -> App
clickRoomRow =
    ProgramTest.simulateDomEvent
        (Query.find [ class "place-room-row" ])
        ( "click", E.object [] )


{-| 波形の帯を押す / なぞる(器の幅 400px の何割の位置か)。 -}
waveEvent : String -> Float -> App -> App
waveEvent name ratio =
    ProgramTest.simulateDomEvent
        (Query.find [ class "waveform" ])
        ( name
        , E.object
            [ ( "offsetX", E.float (ratio * 400) )
            , ( "currentTarget", E.object [ ( "clientWidth", E.float 400 ) ] )
            ]
        )


{-| クラス名で 1 つだけの要素を押す(文字合わせでは当たらないボタン向け)。 -}
clickOn : String -> App -> App
clickOn className =
    ProgramTest.simulateDomEvent (Query.find [ class className ]) ( "click", E.object [] )


{-| 検索パネルを開いて「turret」で探し、結果が返った所まで。 -}
searched : App -> App
searched app =
    app
        |> ProgramTest.update Main.SearchToggled
        |> typeSearch "turret"
        |> ProgramTest.advanceTime 150
        |> ensureKinds [ "search" ]
        |> respondOk 8 "search" searchBody


{-| 一覧の行を右クリックする(カーソル位置と窓の大きさを添える)。 -}
rightClickFileRow : App -> App
rightClickFileRow =
    ProgramTest.simulateDomEvent
        (Query.find [ class "file-row" ])
        ( "contextmenu"
        , E.object
            [ ( "clientX", E.float 120 )
            , ( "clientY", E.float 200 )
            , ( "view", E.object [ ( "innerWidth", E.float 1280 ), ( "innerHeight", E.float 800 ) ] )
            ]
        )


{-| 横断検索の窓に打つ。 -}
typeSearch : String -> App -> App
typeSearch value =
    ProgramTest.simulateDomEvent
        (Query.find [ class "search-query" ])
        ( "input", E.object [ ( "target", E.object [ ( "value", E.string value ) ] ) ] )


{-| GET /search の応答(ファイル名 1 件 + 中身 2 件 = 一覧は 3 行)。 -}
searchBody : E.Value
searchBody =
    E.object
        [ ( "files", E.list E.string [ "assets/level.json" ] )
        , ( "filesTotal", E.int 1 )
        , ( "total", E.int 2 )
        , ( "truncated", E.bool False )
        , ( "results"
          , E.list identity
                [ hitBody [ E.string "spawns", E.int 0, E.string "kind" ] "turret"
                , hitBody [ E.string "spawns", E.int 1, E.string "route" ] "fast"
                ]
          )
        ]


hitBody : List E.Value -> String -> E.Value
hitBody path value =
    E.object
        [ ( "file", E.string "assets/level.json" )
        , ( "path", E.list identity path )
        , ( "value", E.string value )
        , ( "excerpt", E.string value )
        ]


{-| 一覧の行に常設した操作ボタン(↑ ↓ 複製 ✕)の index 番目を押す。 -}
clickRowOp : String -> Int -> App -> App
clickRowOp className index =
    ProgramTest.simulateDomEvent
        (Query.findAll [ class className ] >> Query.index index)
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
                    |> expectKinds [ "files", "resources", "goldenStatus", "journeyState" ]
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
        , test "ホーム: 起動がしくじったら実況は消えず、理由と知らせが出る" <|
            \() ->
                bootedWith resourcesBody
                    |> ProgramTest.clickButton "ホーム"
                    |> ensureKinds [ "journeyState", "journeyChanges" ]
                    |> respondOk 0 "journeyState" (journeyBody "launch" "launch")
                    |> ProgramTest.clickButton "▶ 起動する"
                    |> ensureKinds [ "gameStart" ]
                    |> respondOk 4 "gameStart" (E.object [ ( "ok", E.bool True ) ])
                    |> respondOk 0
                        "gameLog"
                        (E.object
                            [ ( "running", E.bool False )
                            , ( "exitCode", E.int 2 )
                            , ( "lines", E.list E.string [ "Unable to locate a Java Runtime" ] )
                            ]
                        )
                    |> ProgramTest.ensureViewHas [ class "launch-line" ]
                    |> ProgramTest.ensureViewHas [ text "Unable to locate a Java Runtime" ]
                    |> ProgramTest.expectViewHas [ text "起動に失敗しました。ログを確認してください(ターミナルの make debug でも試せます)" ]
        , test "起動のしくじりは、どの画面に居ても届くよう知らせにもなる" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    starting =
                        { m0 | atelier = Atelier.gameStarted m0.atelier }

                    died =
                        E.object
                            [ ( "id", E.int 0 )
                            , ( "kind", E.string "gameLog" )
                            , ( "ok", E.bool True )
                            , ( "body"
                              , E.object
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 2 )
                                    , ( "lines", E.list E.string [ "boom" ] )
                                    ]
                              )
                            ]

                    ( after, _ ) =
                        Main.update (Main.GotApiResponse died) starting
                in
                after.notice
                    |> Expect.equal (Just "ゲームを起動できませんでした — 右上の ⚠ ログ で詳細を見られます")
        , test "描き出し(runnerLog)のしくじりも、知らせと ⚠ ログの両方になる" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    died =
                        E.object
                            [ ( "id", E.int 0 )
                            , ( "kind", E.string "runnerLog" )
                            , ( "ok", E.bool True )
                            , ( "body"
                              , E.object
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 1 )
                                    , ( "lines", E.list E.string [ "compile error" ] )
                                    ]
                              )
                            ]

                    ( after, _ ) =
                        Main.update (Main.GotApiResponse died) m0
                in
                ( after.notice, after.lastFailure )
                    |> Expect.equal
                        ( Just "プレビューを描き出せませんでした — 右上の ⚠ ログ で詳細を見られます"
                        , Just { title = "プレビューを描き出せませんでした", lines = [ "compile error" ] }
                        )
        , test "静かに終わった(exitCode 0)だけなら知らせは出さない" <|
            \() ->
                let
                    ( m0, _ ) =
                        Main.init ()

                    starting =
                        { m0 | atelier = Atelier.gameStarted m0.atelier }

                    closed =
                        E.object
                            [ ( "id", E.int 0 )
                            , ( "kind", E.string "gameLog" )
                            , ( "ok", E.bool True )
                            , ( "body"
                              , E.object
                                    [ ( "running", E.bool False )
                                    , ( "exitCode", E.int 0 )
                                    , ( "lines", E.list E.string [] )
                                    ]
                              )
                            ]

                    ( after, _ ) =
                        Main.update (Main.GotApiResponse closed) starting
                in
                after.notice |> Expect.equal Nothing
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
                    |> ProgramTest.clickButton "プロジェクト"
                    |> ensureKinds [ "projects", "runningGames", "workspace" ]
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
        , test "つまみ系 Doc: モード切替を持たず、フォームと JSON の 2 ペインで開く" <|
            \() ->
                openedLevel
                    -- 盤面(grid 欄)を持たないスキーマ = つまみ系。切替の札は出ない
                    |> ProgramTest.ensureViewHasNot [ text "ビジュアル" ]
                    |> ProgramTest.ensureViewHasNot [ text "コード" ]
                    |> ProgramTest.expectViewHas [ class "form-tabs", class "json-box" ]
        , test "盤面(grid 欄を持つ Doc)は従来のモード切替のまま(型で分ける)" <|
            \() ->
                openedMap
                    |> ProgramTest.expectViewHas [ text "ビジュアル", text "コード" ]
        , test "音の Doc: 宣言された音を選ぶと波形と ▶ が出て、押すと頭から鳴る" <|
            \() ->
                openedMusic
                    |> ProgramTest.ensureViewHas [ class "sound-play", class "waveform" ]
                    |> ProgramTest.clickButton "▶ moonlight"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\kind name offset -> ( kind, name, offset ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "name" ] D.string)
                            (D.at [ "payload", "offset" ] D.float)
                        )
                        (Expect.equal [ ( "playSound", "moonlight.wav", 0 ) ])
        , test "焼き上がりの見比べ: 数字を押すと窓が開き、割れた物が先に並ぶ" <|
            \() ->
                booted
                    |> ProgramTest.update (Main.GotApiResponse (goldenEnvelope goldenStatusBody))
                    |> ProgramTest.ensureViewHas [ text "⚠ 1 / 3" ]
                    |> ProgramTest.clickButton "⚠ 1 / 3"
                    |> ProgramTest.expectViewHas
                        [ text "1 件 割れています(2 / 3 一致)"
                        , text "pit.png"
                        , text "この姿を正にする(祝福)"
                        ]
        , test "焼き上がりの見比べ: 祝福すると、その名前をサーバへ渡して見比べ直す" <|
            \() ->
                booted
                    |> ProgramTest.update (Main.GotApiResponse (goldenEnvelope goldenStatusBody))
                    |> ProgramTest.clickButton "⚠ 1 / 3"
                    -- 窓を開いた拍の見比べ直し(採番外)を挟んでから祝福が飛ぶ
                    |> ensureKinds [ "goldenStatus" ]
                    |> ProgramTest.clickButton "この姿を正にする(祝福)"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "name" ] D.string))
                        (Expect.equal [ ( "goldenBless", "pit.png" ) ])
        , test "行を選ぶと、その瞬間を 1 枚だけ焼いて出す(連打は最後の 1 回だけ)" <|
            \() ->
                openedCuts
                    -- 選んだ拍では投げない(200ms 置く)
                    |> ensureKinds []
                    |> ProgramTest.advanceTime 200
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map3 (\kind path args -> ( kind, path, args ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "path" ] D.string)
                            (D.at [ "payload", "query" ] D.string)
                        )
                        -- 2 行目 = カット 2(1 始まり)
                        (Expect.equal [ ( "cutsceneFrame", "/frame", "cut=2" ) ])
                    |> respondOk 7 "cutsceneFrame" frameShotBody
                    |> ProgramTest.expectViewHas [ class "frame-shot", text "カット 2 の瞬間" ]
        , test "1 枚焼き: 同じ行をもう一度選んでも焼き直さない(控えから出す)" <|
            \() ->
                openedCuts
                    |> ProgramTest.advanceTime 200
                    |> ensureKinds [ "cutsceneFrame" ]
                    |> respondOk 7 "cutsceneFrame" frameShotBody
                    -- 1 行目 → 2 行目と選び直しても、2 行目は控えにある
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", containing [ text "#0" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.advanceTime 200
                    |> ensureKinds [ "cutsceneFrame" ]
                    |> respondOk 8 "cutsceneFrame" frameShotBody
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", containing [ text "だれか" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.advanceTime 200
                    |> ProgramTest.expectOutgoingPortValues "apiRequest" (D.field "kind" D.string) (Expect.equal [])
        , test "焼く: 保存してから焼き係へ取り次ぎ、応答で GIF と注意の件数が出る" <|
            \() ->
                openedCuts
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7 "bakeWake" wakeOkBody
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "url" ] D.string))
                        (Expect.equal [ ( "bakeCutscene", "http://127.0.0.1:8792/cutscene" ) ])
                    |> ProgramTest.ensureViewHas [ text "焼いています…" ]
                    |> respondOk 8 "bakeCutscene" bakeOkBody
                    |> ProgramTest.expectViewHas
                        [ class "bake-gif"
                        , text "⚠ 飛ばしたカット 1 件"
                        ]
        , test "焼く: 注意はその行(1 始まり)に ⚠ が付く" <|
            \() ->
                openedCuts
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7 "bakeWake" wakeOkBody
                    |> ensureKinds [ "bakeCutscene" ]
                    |> respondOk 8 "bakeCutscene" bakeOkBody
                    |> ProgramTest.expectView
                        (Query.findAll [ class "row-note" ] >> Query.count (Expect.equal 1))
        , test "焼く: 焼き完了後、同じ token の changes が来ると「焼く」は押せなくなる" <|
            \() ->
                openedCuts
                    -- 焼く前に token(tA)を知っておく
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tA")
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7 "bakeWake" wakeOkBody
                    |> ensureKinds [ "bakeCutscene" ]
                    |> respondOk 8 "bakeCutscene" bakeOkBody
                    -- 焼き完了(dirty ではない)。bakedToken = { file = prologue, token = tA }
                    -- 同じ token(tA)の changes が届く → 何も変わっていない → 押せない
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tA")
                    |> ProgramTest.expectView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled True ])
        , test "焼く: 焼き完了後でも、別の token の changes(JSON が変わった)が来れば再び押せる" <|
            \() ->
                openedCuts
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tA")
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7 "bakeWake" wakeOkBody
                    |> ensureKinds [ "bakeCutscene" ]
                    |> respondOk 8 "bakeCutscene" bakeOkBody
                    -- 別の token(tB)= 何かが変わった(このファイルの mtime 自体は同じでも、
                    -- 部屋の JSON 等どこかが変わったので token は集計として動く)
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tB")
                    |> ProgramTest.expectView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled False ])
        , test "焼く: 押せなくなった後でも、編集して打ちかけ(dirty)ができれば押せる" <|
            \() ->
                openedCuts
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tA")
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7 "bakeWake" wakeOkBody
                    |> ensureKinds [ "bakeCutscene" ]
                    |> respondOk 8 "bakeCutscene" bakeOkBody
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tA")
                    |> ProgramTest.ensureView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled True ])
                    -- 選んでいる行(言葉)の添え物(見回す速さ)を打ちかけで書き換えて確定する。
                    -- draft は打っただけでは dirty にならない — Enter → applyDocEdit の
                    -- 応答で doc(docText)が実際に書き換わって初めて dirty になる
                    |> typeNumberBox "2.0"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 9
                        "applyDocEdit"
                        (E.object
                            [ ( "text"
                              , E.string
                                    """{
  "id": "prologue",
  "room": "hall1",
  "cuts": [
    { "wait": 1.0 },
    { "say": ["……だれか いるの?"], "pan": 2.0 }
  ]
}"""
                              )
                            ]
                        )
                    |> ProgramTest.expectView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled False ])
        , test "焼く: a を焼いてから b を焼くと、a も b も disabled のまま(片方の控えがもう片方を上書きしない)" <|
            \() ->
                -- 報告されたバグの再現: bakedTokens が 1 枠の Maybe だった時は、
                -- b の焼きの控えが a の控えを上書きし、b は disabled のまま a が
                -- 押せてしまっていた。ファイルごとの Dict なら両方とも disabled のまま
                bootedWith twoCutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 6 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 7 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 8 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
                    -- token(tA)を知っておく(a・b とも同じ mtime のまま = 何も変えていない)
                    |> respondOk 0
                        "changes"
                        (changesBodyTokenMany
                            [ ( "assets/prologue.scene.json", 111 ), ( "assets/second.scene.json", 111 ) ]
                            "tA"
                        )
                    -- a(prologue)を焼く
                    |> clickOn "bake-run"
                    |> respondOk 9 "bakeWake" wakeOkBody
                    |> respondOk 10 "bakeCutscene" bakeOkBody
                    -- b(second)へ切り替えて焼く
                    |> ProgramTest.clickButton "assets/second.scene.json"
                    |> respondOk 11 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 12 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 13 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 14 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 15 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
                    |> clickOn "bake-run"
                    |> respondOk 16 "bakeWake" wakeOkBody
                    |> respondOk 17 "bakeCutscene" bakeOkBody
                    -- b を見ている間: 何も変わっていない(同じ token)ので b は disabled
                    |> ProgramTest.ensureView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled True ])
                    -- a へ戻る
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> respondOk 18 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 19 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 20 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 21 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 22 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
                    -- a も、同じく何も変わっていないので disabled のまま(バグがあれば有効に戻ってしまう)
                    |> ProgramTest.expectView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled True ])
        , test "焼く: Studio を再起動した後(bakedTokens は消えている)でも、復元した GIF が全 JSON より新しければ disabled" <|
            \() ->
                -- bakedTokens はメモリなので Studio 再起動で消える。ここでは焼いたことは
                -- 一度も無い(セッション内の bakedTokens 経路は使わない) — pastBake の復元
                -- (mediaExists の X-Mtime)と changes の mtime 一覧だけでディスクの事実から
                -- 「前回の焼きから何も変わっていない」を言い直せることを見る
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> ensureKinds [ "mediaExists" ]
                    -- 復元した GIF の mtime は 500(サーバの X-Mtime ヘッダ相当)
                    |> respondOk 6 "mediaExists" (E.object [ ( "exists", E.bool True ), ( "mtime", E.int 500 ) ])
                    |> ProgramTest.ensureViewHas [ class "bake-gif", text "前回の焼き" ]
                    -- 全 JSON(場面 111・hitbox.json 1)が GIF(500)より古い → 何も変わっていない
                    |> respondOk 0 "changes" (changesBodyToken "assets/prologue.scene.json" 111 "tX")
                    |> ProgramTest.expectView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled True ])
        , test "焼く: 復元した GIF より新しい JSON がある changes が来れば、押せるようになる" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> ensureKinds [ "mediaExists" ]
                    |> respondOk 6 "mediaExists" (E.object [ ( "exists", E.bool True ), ( "mtime", E.int 500 ) ])
                    |> ProgramTest.ensureViewHas [ class "bake-gif", text "前回の焼き" ]
                    -- 別の JSON(例: 部屋の間取り相当。ここでは hitbox.json で代用)が
                    -- GIF(500)より新しい(600)→ GIF より後に何かが変わった → 押せる
                    |> respondOk 0
                        "changes"
                        (changesBodyTokenMany
                            [ ( "assets/prologue.scene.json", 111 ), ( "hitbox.json", 600 ) ]
                            "tY"
                        )
                    |> ProgramTest.expectView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled False ])
        , test "焼く: 焼き途中に別ファイルへ切り替えても焼きは裏で続き、戻ると届いた焼き上がりが出る" <|
            \() ->
                -- id 番号は「開くたび横断辞書(他ファイル)を読みに行く」分も含めて実測した並び
                -- (2 本立ての resources なので、開くたびにもう 1 本の getFile が飛ぶ)。
                -- 主眼は id 10(bakeCutscene)を最後まで寝かせて、ファイルを行き来しても
                -- 生きたまま戻って来られることだけ
                bootedWith twoCutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 6 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 7 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 8 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
                    -- 焼き始める(起こし → 焼き係へ取り次ぎ、id 10 の応答をまだ寝かせる)
                    |> clickOn "bake-run"
                    |> respondOk 9 "bakeWake" wakeOkBody
                    -- 別ファイルへ切り替える(焼き応答はまだ届いていない)
                    |> ProgramTest.clickButton "assets/second.scene.json"
                    |> respondOk 11 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 12 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    -- 別ファイルを見ている間は「別の脚本を焼いています」の札で、焼くボタンは押せない
                    |> ProgramTest.ensureViewHas [ text "別の脚本を焼いています" ]
                    |> ProgramTest.ensureView
                        (Query.find [ class "bake-run" ] >> Query.has [ Test.Html.Selector.disabled True ])
                    |> respondOk 13 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 14 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 15 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
                    -- 元のファイルへ戻る(まだ焼き応答は届いていない)
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> respondOk 16 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 17 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 18 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 19 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 20 "mediaExists" (E.object [ ( "exists", E.bool False ) ])
                    -- ここでようやく焼き応答(id 10)が届く。戻ったファイルと一致するので、そのまま表示に映る
                    |> respondOk 10 "bakeCutscene" bakeOkBody
                    |> ProgramTest.expectViewHas
                        [ class "bake-gif"
                        , text "⚠ 飛ばしたカット 1 件"
                        ]
        , test "焼く: 起こし待ち中に切り替えても、焼かれるのは押した時のファイル" <|
            \() ->
                -- 「焼く」を押す(起こし待ちが始まる)→ 応答が届く前に別ファイルへ
                -- 切り替える → 起こし完了(BakeAfterWake)で焼かれるのは model.current
                -- (切り替え先)ではなく、押した時に model に控えた path であるべき
                bootedWith twoCutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 4(本文)・5(スキーマ)
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    -- 焼き係を起こし始める(id 9。id 6〜8 は横断辞書・前回焼き探しの分)
                    |> clickOn "bake-run"
                    -- 起こしの応答(id 9)がまだ届かないうちに、別ファイルへ切り替える
                    |> ProgramTest.clickButton "assets/second.scene.json"
                    -- id 10(本文)・11(スキーマ)。本文の応答で model.current が切り替わる
                    |> respondOk 10 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 11 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    -- ここでようやく起こし完了が届く。今開いているのは second だが、
                    -- 焼くべきは押した時の prologue のはず
                    |> respondOk 9 "bakeWake" wakeOkBody
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair
                            (D.field "kind" D.string)
                            (D.maybe (D.at [ "payload", "file" ] D.string))
                        )
                        (\vals ->
                            vals
                                |> List.filter (\( kind, _ ) -> kind == "bakeCutscene")
                                |> Expect.equal [ ( "bakeCutscene", Just "assets/prologue.scene.json" ) ]
                        )

        -- 不具合 2(「スキーマを探しています…」で止まる)の再現の試み。
        -- id での突き合わせが本当に頑丈か、応答の並びを崩して確かめる
        , test "開く: スキーマが本文より先に届いても止まらない(逆順)" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    -- スキーマ(id 5)を先に、本文(id 4)を後に
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> ProgramTest.expectViewHasNot [ text "スキーマを探しています" ]
        , test "開く: A→B→A と速く切り替え、一番古い応答が一番最後に届いても止まらない" <|
            \() ->
                bootedWith twoCutsResourcesBody
                    -- 3 回続けて開き直す(どの応答もまだ受け取っていない)
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 4(本文A-1回目)・5(スキーマA-1回目)
                    |> ProgramTest.clickButton "assets/second.scene.json"
                    -- id 6(本文B)・7(スキーマB)
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 8(本文A-2回目)・9(スキーマA-2回目) ← これだけが「今」の要求
                    |> respondOk 7 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 9 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 6 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 8 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> ProgramTest.expectViewHasNot [ text "スキーマを探しています" ]
        , test "開く: 焼き応答が本文・スキーマ応答の間に割り込んでも止まらない" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    -- スキーマ応答がまだ届く前に、無関係な焼き応答(飛んでいる要求など無い)が割り込む
                    |> respondOk 999 "bakeCutscene" bakeOkBody
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> ProgramTest.expectViewHasNot [ text "スキーマを探しています" ]
        , test "開く: 追い越された古いスキーマ要求の失敗 envelope が今のスキーマの後に届いても壊さない" <|
            \() ->
                bootedWith twoCutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 4(本文A-1回目)・5(スキーマA-1回目、後で失敗させる)
                    |> ProgramTest.clickButton "assets/second.scene.json"
                    -- id 6(本文B)・7(スキーマB) ← 今はこちらが「今」
                    |> respondOk 6 "getFile" (fileBody "assets/second.scene.json" secondCutsText)
                    |> respondOk 7 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    -- 追い越された古い要求(id 5)の失敗がここで届く
                    |> respondErr 5 "getFile" "404 Not Found"
                    |> ProgramTest.expectViewHasNot [ text "スキーマを探しています" ]
        , test "開く: 同じファイルを連打しても、一番新しい要求の応答だけで止まらない" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 4(本文-1回目)・5(スキーマ-1回目)
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 6(本文-2回目)・7(スキーマ-2回目) ← これが「今」
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    -- id 8(本文-3回目)・9(スキーマ-3回目) ← 本当の「今」
                    -- 一番古い要求の応答が一番最後に届く極端な並び
                    |> respondOk 9 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 8 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 7 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 6 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> ProgramTest.expectViewHasNot [ text "スキーマを探しています" ]
        , test "実機で再生: 起こしてから、選んでいるカットを from に添えて頼む" <|
            \() ->
                cutsWarm
                    |> ProgramTest.clickButton "▶ 実機で再生"
                    -- 起こしは焼き係と同じ道(実機の口を起こす)
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "url" ] D.string))
                        (Expect.equal [ ( "bakeWake", "http://127.0.0.1:7777/scene" ) ])
                    |> respondOk 8 "bakeWake" wakeOkBody
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map3 (\kind file query -> ( kind, file, query ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "file" ] D.string)
                            (D.at [ "payload", "query" ] D.string)
                        )
                        -- 2 行目を選んでいる = カット 2 から
                        (Expect.equal [ ( "performScene", "assets/prologue.scene.json", "from=2" ) ])
                    |> respondOk 9 "performScene" performOkBody
                    |> ProgramTest.expectViewHas [ text "実機で再生中(カット 2 から)" ]
        , test "実機で再生: error: の応答は、その言葉をそのまま見せる" <|
            \() ->
                cutsWarm
                    |> ProgramTest.clickButton "▶ 実機で再生"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 8 "bakeWake" wakeOkBody
                    |> ensureKinds [ "performScene" ]
                    |> respondOk 9 "performScene" performErrorBody
                    |> ProgramTest.expectViewHas [ text "error: 脚本が読めません" ]
        , test "oneOf の表: 鍵ごとの列見出しではなく #/カット/内容 の 3 列で出る" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" oneOfDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" oneOfSchemaText)
                    |> ProgramTest.expectView
                        (Expect.all
                            [ Query.hasNot
                                [ text "待つ(秒。とても長い説明がここに入り、見出しでは省略されてほしい)" ]
                            , Query.has [ tag "th", containing [ text "カット" ] ]
                            , Query.has [ tag "th", containing [ text "内容" ] ]
                            ]
                        )
        , test "oneOf の表: カット列に鍵の名前、内容列に値の要約(座標・添え鍵の札)が出る" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" oneOfDocText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" oneOfSchemaText)
                    |> ProgramTest.expectViewHas
                        [ text "wait"
                        , text "say"
                        , text "(35, 4) [待たない]"
                        ]
        , test "前回の焼き: 開いた拍に産物があれば出し、「前回の焼き」の札を添える" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> ensureKinds [ "mediaExists" ]
                    |> respondOk 6 "mediaExists" (E.object [ ( "exists", E.bool True ) ])
                    |> ProgramTest.expectViewHas [ class "bake-gif", text "前回の焼き" ]
        , test "前回の焼き: 復元直後は無かったシークバーが、コマ数を数え直すと出る" <|
            \() ->
                bootedWith cutsResourcesBody
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/prologue.scene.json" cutsText)
                    |> respondOk 5 "getFile" (fileBody "assets/scene.schema.json" cutsSchemaText)
                    |> ensureKinds [ "mediaExists" ]
                    |> respondOk 6 "mediaExists" (E.object [ ( "exists", E.bool True ) ])
                    -- 復元直後(pastBake はコマ数を知らない)は GIF だけで、操作列(シークバー)は無い
                    |> ProgramTest.ensureViewHas [ class "bake-gif", text "前回の焼き" ]
                    |> ProgramTest.ensureViewHasNot [ class "filmstrip" ]
                    -- コマ別 PNG の置き場を数え直す要求が飛ぶ
                    |> ensureKinds [ "mediaCount" ]
                    |> respondOk 7 "mediaCount" (E.object [ ( "ok", E.bool True ), ( "count", E.int 5 ) ])
                    -- 届いたコマ数(5 枚 = 0〜4)でインラインにシークバーが出て、i/n も正しい
                    |> ProgramTest.expectViewHas [ class "filmstrip", text "0 / 4" ]
        , test "ファイルを切り替えると、前のファイルの焼き上がりは消える" <|
            \() ->
                -- 瞬間(frameShot)を選ばず焼く: 右ペインの絵の枠は通し(Film)を出す
                -- (瞬間と通しが両方あれば「最後に指した方」を出すので、ここでは瞬間を挟まない)
                openedCuts
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7 "bakeWake" wakeOkBody
                    |> ensureKinds [ "bakeCutscene" ]
                    |> respondOk 8 "bakeCutscene" bakeOkBody
                    |> ProgramTest.ensureViewHas [ class "bake-gif" ]
                    -- 同じ一覧のもう 1 本を開く(この見本は 1 本なので開き直しで見る)
                    |> ProgramTest.clickButton "assets/prologue.scene.json"
                    |> ProgramTest.expectViewHasNot [ class "bake-gif" ]
        , test "起こし待ち: 届かない間は焼きへ進まず、2 秒おきに訊き直す(撃つのは 1 回目だけ)" <|
            \() ->
                cutsWarm
                    |> clickOn "bake-run"
                    -- 1 回目は起こす(launch=true)
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "launch" ] D.bool))
                        (Expect.equal [ ( "bakeWake", True ) ])
                    |> respondOk 8 "bakeWake" wakeWaitingBody
                    -- 焼きへは進まない
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHas [ text "焼き係を起こしています…" ]
                    |> ProgramTest.advanceTime 2000
                    -- 2 回目からは訊くだけ(launch=false)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "launch" ] D.bool))
                        (Expect.equal [ ( "bakeWake", False ) ])
        , test "起こし待ち: 起きたら焼きへ進む" <|
            \() ->
                cutsWarm
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 8 "bakeWake" wakeWaitingBody
                    |> ProgramTest.advanceTime 2000
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 9 "bakeWake" wakeOkBody
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.field "kind" D.string)
                        (Expect.equal [ "bakeCutscene" ])
        , test "起こし待ち: やめると訊き直しが止まる(遅れて来た応答も効かない)" <|
            \() ->
                cutsWarm
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 8 "bakeWake" wakeWaitingBody
                    |> ProgramTest.clickButton "やめる"
                    |> ProgramTest.ensureViewHasNot [ text "焼き係を起こしています…" ]
                    |> ProgramTest.advanceTime 5000
                    |> ProgramTest.expectOutgoingPortValues "apiRequest" (D.field "kind" D.string) (Expect.equal [])
        , test "焼く: 起こし方の宣言が無いプロジェクトでは、宣言のしかたを案内する" <|
            \() ->
                openedCuts
                    |> clickOn "bake-run"
                    |> ensureKinds [ "bakeWake" ]
                    |> respondOk 7
                        "bakeWake"
                        (E.object
                            [ ( "ok", E.bool False )
                            , ( "reachable", E.bool False )
                            , ( "needsCmd", E.bool True )
                            ]
                        )
                    -- 焼きへは進まない(頼む先が起きていない)
                    |> ensureKinds []
                    |> ProgramTest.expectViewHas [ text "焼き係の起こし方が宣言されていません" ]
        , test "マスの欄({x,y})には絵から選ぶ入口が出て、押した場所がマスになる" <|
            \() ->
                openedTilePick
                    |> ProgramTest.clickButton "マップから選ぶ"
                    |> ProgramTest.ensureViewHas [ class "tile-grid" ]
                    -- 960px 幅の絵 ÷ 20 列 = 48px/マス → (150, 100) は (3, 2)
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "tile-grid" ])
                        ( "click"
                        , E.object
                            [ ( "offsetX", E.float 150 )
                            , ( "offsetY", E.float 100 )
                            , ( "target", E.object [ ( "clientWidth", E.float 960 ) ] )
                            ]
                        )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "value" ] D.value |> D.map (E.encode 0)))
                        (Expect.equal [ ( "applyDocEdit", """{"x":3,"y":2}""" ) ])
        , test "1 行 1 種類(oneOf): 種類のセレクトと、その行の欄だけが出る" <|
            \() ->
                openedCuts
                    |> ProgramTest.ensureViewHas [ class "kind-select" ]
                    -- 選んでいるのは「言葉」。添え物(見回す速さ)も消さずに出す
                    |> ProgramTest.expectView
                        (Query.findAll [ class "form-label" ]
                            >> Query.each (Expect.all [ Query.hasNot [ text "待つ" ] ])
                        )
        , test "1 行 1 種類(oneOf): 種類を変えると 前の鍵を消して 新しい鍵を既定値で書く" <|
            \() ->
                openedCuts
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "kind-select" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "wait" ) ] ) ] )
                    -- 2 本組は直列(前の応答を待ってから次)。まず古い鍵を消す
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocRemove", [ "cuts", "1", "say" ] ) ])
                    |> respondOk 7 "applyDocRemove" (E.object [ ( "text", E.string cutsText ) ])
                    -- 続けて新しい鍵を既定値で書く(戻すのは 1 回で済む 1 手)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocEdit", [ "cuts", "1", "wait" ] ) ])
        , test "譜のある音: ピアノロールが波形の上に出る(音符の数だけ棒が並ぶ)" <|
            \() ->
                openedMusic
                    |> ProgramTest.ensureViewHas [ class "piano-roll", class "waveform" ]
                    |> ProgramTest.expectView
                        (Query.findAll [ class "roll-note" ] >> Query.count (Expect.equal 2))
        , test "譜のある音: 音符を押すと、その行(notes[i])を右の JSON で指し示す" <|
            \() ->
                openedMusic
                    |> ProgramTest.simulateDomEvent
                        (Query.findAll [ class "roll-note" ] >> Query.index 1)
                        ( "click", E.object [] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair
                            (D.field "kind" D.string)
                            (D.at [ "payload", "path" ] (D.list (D.oneOf [ D.string, D.int |> D.map String.fromInt ])))
                        )
                        (Expect.equal [ ( "highlightJson", [ "tunes", "moonlight", "notes", "1" ] ) ])
        , test "音の Doc: 波形のクリックはシーク(次の ▶ はそこから鳴る)" <|
            \() ->
                openedMusic
                    |> waveEvent "mousedown" 0.25
                    |> waveEvent "mouseup" 0.25
                    |> ProgramTest.clickButton "▶ moonlight"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "offset" ] D.float))
                        -- 2 秒の曲の 1/4 = 0.5 秒目から
                        (Expect.equal [ ( "playSound", 0.5 ) ])
        , test "音の Doc: 波形のドラッグで範囲を選ぶと、▶ はその範囲だけ鳴らす" <|
            \() ->
                openedMusic
                    |> waveEvent "mousedown" 0.25
                    |> waveEvent "mousemove" 0.75
                    |> waveEvent "mouseup" 0.75
                    |> ensureKinds []
                    |> ProgramTest.clickButton "▶ moonlight"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\kind offset duration -> ( kind, offset, duration ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "offset" ] D.float)
                            (D.at [ "payload", "duration" ] D.float)
                        )
                        (Expect.equal [ ( "playSound", 0.5, 1 ) ])
        , test "つまみ系 Doc: 欄に触ると、右の JSON でその場所を指し示す" <|
            \() ->
                openedLevel
                    |> ProgramTest.simulateDomEvent (Query.find [ class "number-box" ]) ( "focus", E.object [] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "highlightJson", [ "meta", "scrollSpeed" ] ) ])
        , test "つまみ系 2 ペイン: 幅を持つのは JSON 側だけ(境界が指と同じ向きに動く)" <|
            \() ->
                openedLevel
                    |> ProgramTest.expectView
                        (Query.find [ class "pane-form" ]
                            >> Query.has [ class "flex-1" ]
                        )
        , test "JSON を畳んでも、右のプレビュー(音の ▶)は残る" <|
            \() ->
                openedMusic
                    |> ProgramTest.ensureViewHas [ class "sound-play", class "json-box" ]
                    |> clickOn "json-head"
                    |> ProgramTest.ensureViewHasNot [ class "json-box" ]
                    -- 消えるのは JSON の箱だけ(見え方は残って高さを受け取る)
                    |> ProgramTest.expectViewHas [ class "sound-play", class "json-head" ]
        , test "畳んだ JSON の見出しを押すと戻る(上の道具列まで戻らずに開き直せる)" <|
            \() ->
                openedMusic
                    |> clickOn "json-head"
                    |> ProgramTest.ensureViewHasNot [ class "json-box" ]
                    |> clickOn "json-head"
                    |> ProgramTest.expectViewHas [ class "json-box" ]
        , test "つまみ系 Doc: JSON ペインは畳めて、畳んだ事実は端末に覚える" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "⌨ JSON"
                    |> ProgramTest.ensureViewHasNot [ class "json-box" ]
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "json" ] D.bool))
                        (Expect.equal [ ( "saveUiPrefs", False ) ])
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
        , test "横断検索: 打鍵は 150ms 置いてから 1 回だけ送り、結果は一覧に出る" <|
            \() ->
                openedLevel
                    |> ProgramTest.update Main.SearchToggled
                    |> typeSearch "turret"
                    -- 打っている間は投げない(デバウンス)
                    |> ensureKinds []
                    |> ProgramTest.advanceTime 150
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "q" ] D.string))
                        (Expect.equal [ ( "search", "turret" ) ])
                    |> respondOk 8 "search" searchBody
                    |> ProgramTest.expectViewHas [ text "1 件", text "assets/level.json" ]
        , test "横断検索: ファイル名の一致が先頭に出て、件数はファイル / 中身で分かれる" <|
            \() ->
                openedLevel
                    |> ProgramTest.update Main.SearchToggled
                    |> typeSearch "level"
                    |> ProgramTest.advanceTime 150
                    |> ensureKinds [ "search" ]
                    |> respondOk 8 "search" searchBody
                    |> ProgramTest.expectViewHas
                        [ text "ファイル 1 件 / 中身 2 件"
                        , class "search-file"
                        ]
        , test "横断検索: 検索ボタンを押しても同じパネルが開く(ショートカットを知らなくても入れる)" <|
            \() ->
                openedLevel
                    |> ProgramTest.ensureViewHasNot [ class "search-query" ]
                    |> ProgramTest.clickButton "検索"
                    |> ProgramTest.expectViewHas [ class "search-query" ]
        , test "横断検索: ホーム(アトリエの入口)からも検索ボタンで開ける" <|
            \() ->
                landingWith resourcesBody
                    |> ProgramTest.clickButton "検索"
                    |> ProgramTest.expectViewHas [ class "search-query" ]
        , test "横断検索: ファイル名の当たりを押すと、そのファイルを開く" <|
            \() ->
                booted
                    |> ProgramTest.update Main.SearchToggled
                    |> typeSearch "level"
                    |> ProgramTest.advanceTime 150
                    |> ensureKinds [ "search" ]
                    |> respondOk 4 "search" searchBody
                    |> ProgramTest.simulateDomEvent (Query.find [ class "search-file" ]) ( "click", E.object [] )
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string))
                        -- 本文とスキーマの 2 本(普段どおりの開き方)
                        (Expect.equal
                            [ ( "getFile", "assets/level.json" )
                            , ( "getFile", "assets/level.schema.json" )
                            ]
                        )
        , test "横断検索: ↓↓ Enter で 3 件目(中身の 2 件目)を開く" <|
            \() ->
                openedLevel
                    |> searched
                    |> keydownOn [ class "search-query" ] "ArrowDown"
                    |> keydownOn [ class "search-query" ] "ArrowDown"
                    |> ensureKinds [ "scrollTo", "scrollTo" ]
                    |> keydownOn [ class "search-query" ] "Enter"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "id" ] D.string))
                        (Expect.equal [ ( "scrollTo", "row-spawns-1-route" ) ])
        , test "横断検索: 探し直すと選択は先頭に戻る(Enter は先頭のファイルを開く)" <|
            \() ->
                openedLevel
                    |> searched
                    |> keydownOn [ class "search-query" ] "ArrowDown"
                    |> keydownOn [ class "search-query" ] "ArrowDown"
                    |> ensureKinds [ "scrollTo", "scrollTo" ]
                    |> typeSearch "level"
                    |> ProgramTest.advanceTime 150
                    |> ensureKinds [ "search" ]
                    |> respondOk 10 "search" searchBody
                    |> keydownOn [ class "search-query" ] "Enter"
                    -- 先頭はファイル名の当たり = そのファイルを開く(本文とスキーマ)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string))
                        (Expect.equal
                            [ ( "getFile", "assets/level.json" )
                            , ( "getFile", "assets/level.schema.json" )
                            ]
                        )
        , test "横断検索: 結果を押すと該当ファイルを開き、欄まで画面を送る" <|
            \() ->
                openedLevel
                    |> ProgramTest.update Main.SearchToggled
                    |> typeSearch "turret"
                    |> ProgramTest.advanceTime 150
                    |> ensureKinds [ "search" ]
                    |> respondOk 8 "search" searchBody
                    |> ProgramTest.simulateDomEvent
                        (Query.findAll [ class "search-hit" ] >> Query.index 0)
                        ( "click", E.object [] )
                    -- 開いているファイルの中なので、開き直さず欄へ送るだけ
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "id" ] D.string))
                        (Expect.equal [ ( "scrollTo", "row-spawns-0-kind" ) ])
        , test "置換: ファイルごとに 取得 → 最小編集 → 保存 の直列で書き戻す" <|
            \() ->
                openedLevel
                    |> ProgramTest.update Main.SearchToggled
                    |> typeSearch "turret"
                    |> ProgramTest.advanceTime 150
                    |> ensureKinds [ "search" ]
                    |> respondOk 8 "search" searchBody
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "search-replacement" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "dome" ) ] ) ] )
                    |> ProgramTest.clickButton "すべて置換"
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string))
                        (Expect.equal [ ( "getFile", "assets/level.json" ) ])
                    |> respondOk 9 "getFile" (fileBody "assets/level.json" levelText)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "edits" ] (D.list (D.field "value" D.string))))
                        (Expect.equal [ ( "applyDocEdits", [ "dome", "fast" ] ) ])
        , test "ファイルの動詞: ＋ 新規 は骨格(宣言された欄が全部書かれた本文)で作る" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "＋ 新規"
                    |> ProgramTest.ensureViewHas [ text "「レベル」に新しいファイルを作ります。中身はスキーマの欄を全部書いた骨格です。" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "verb-name" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "stage2" ) ] ) ] )
                    |> ProgramTest.clickButton "作る"
                    -- 骨格を組むためにスキーマを 1 往復取りに行ってから作る
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 8 "getFile" (fileBody "assets/level.schema.json" schemaText)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\kind path content -> ( kind, path, String.contains "\"scrollSpeed\": 60" content ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "path" ] D.string)
                            (D.at [ "payload", "content" ] D.string)
                        )
                        (Expect.equal [ ( "fileNew", "assets/stage2.json", True ) ])
        , test "ファイルの動詞: 行の右クリック → 複製 で、元と新しいパスをサーバへ渡す" <|
            \() ->
                openedLevel
                    |> rightClickFileRow
                    |> ProgramTest.clickButton "複製"
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "verb-name" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "level_b" ) ] ) ] )
                    |> ProgramTest.clickButton "複製する"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\kind path toPath -> ( kind, path, toPath ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "path" ] D.string)
                            (D.at [ "payload", "toPath" ] D.string)
                        )
                        (Expect.equal [ ( "fileDuplicate", "assets/level.json", "assets/level_b.json" ) ])
        , test "ファイルの動詞: 右クリック → 削除 は確認を出してから、そのファイルだけを渡す" <|
            \() ->
                openedLevel
                    |> rightClickFileRow
                    |> ProgramTest.clickButton "削除"
                    |> ProgramTest.ensureViewHas [ text "\"assets/level.json\" を消します。元には戻せません。" ]
                    |> ProgramTest.clickButton "消す"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string))
                        (Expect.equal [ ( "fileDelete", "assets/level.json" ) ])
        , test "ファイルの動詞: Delete キー(行にカーソル)でも削除の確認が出る" <|
            \() ->
                openedLevel
                    |> keydownOn [ class "file-open" ] "Delete"
                    |> ProgramTest.expectViewHas [ text "\"assets/level.json\" を消します。元には戻せません。" ]
        , test "その場の名前変更: F2 で欄になり、Enter で fileRename が飛ぶ" <|
            \() ->
                openedLevel
                    |> keydownOn [ class "file-open" ] "F2"
                    |> ProgramTest.ensureViewHas [ class "file-rename" ]
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "file-rename" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "stage9" ) ] ) ] )
                    |> keydownOn [ class "file-rename" ] "Enter"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.field "payload" D.value)
                            |> D.andThen
                                (\( kind, payload ) ->
                                    if kind == "fileRename" then
                                        D.map2 (\path toPath -> ( path, toPath ))
                                            (D.at [ "payload", "path" ] D.string)
                                            (D.at [ "payload", "toPath" ] D.string)

                                    else
                                        -- 欄へカーソルを置く頼み事は別件(名前の変更そのものではない)
                                        D.succeed ( kind, "" )
                                )
                        )
                        (Expect.equal
                            [ ( "focusId", "" )
                            , ( "assets/level.json", "assets/stage9.json" )
                            ]
                        )
        , test "その場の名前変更: Esc は取り消し(欄が消えて、何も飛ばない)" <|
            \() ->
                openedLevel
                    |> keydownOn [ class "file-open" ] "F2"
                    |> keydownOn [ class "file-rename" ] "Escape"
                    |> ProgramTest.ensureViewHasNot [ class "file-rename" ]
                    -- 飛んだのは欄へカーソルを置く頼み事だけ(ファイルは触らない)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.field "kind" D.string)
                        (Expect.equal [ "focusId" ])
        , test "右クリックメニュー: Esc で閉じる" <|
            \() ->
                openedLevel
                    |> rightClickFileRow
                    |> ProgramTest.ensureViewHas [ class "context-menu" ]
                    |> ProgramTest.update Main.FileMenuClosed
                    |> ProgramTest.expectViewHasNot [ class "context-menu" ]
        , describe "ダイアログは ✕ / Esc / 外側クリックのどれでも閉じる"
            ([ "close-button", "keyboard", "overlay" ]
                |> List.map
                    (\source ->
                        test ("新しいファイルのダイアログ: " ++ source) <|
                            \() ->
                                openedLevel
                                    |> ProgramTest.clickButton "＋ 新規"
                                    |> ProgramTest.ensureViewHas [ class "file-verb" ]
                                    |> ProgramTest.simulateDomEvent
                                        (Query.find [ class "file-verb" ])
                                        ( "sl-request-close"
                                        , E.object [ ( "detail", E.object [ ( "source", E.string source ) ] ) ]
                                        )
                                    |> ProgramTest.expectViewHasNot [ class "file-verb" ]
                    )
            )
        , test "元に戻す: 欄の編集が履歴に積まれ、↩ を押すと旧値を書く applyDocEdit が飛ぶ" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelEditedText ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.ensureViewHas [ text "↩ 1" ]
                    |> ProgramTest.clickButton "↩ 1"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindPathValue
                        (Expect.equal [ ( "applyDocEdit", [ "meta", "scrollSpeed" ], "60" ) ])
        , test "元に戻す: 戻した手はやり直せる(↩ の数が減り、同じ値をもう一度書く)" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelEditedText ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.clickButton "↩ 1"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 10 "applyDocEdit" (E.object [ ( "text", E.string levelText ) ])
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHasNot [ text "↩ 1" ]
                    |> ProgramTest.update Main.RedoPressed
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindPathValue
                        (Expect.equal [ ( "applyDocEdit", [ "meta", "scrollSpeed" ], "72" ) ])
        , test "元に戻す: 追加した行は remove で消える(逆操作は末尾 1 件だけ)" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "spawns"
                    |> ProgramTest.clickButton "+ 追加"
                    |> ensureKinds [ "applyDocAppend" ]
                    |> respondOk 8 "applyDocAppend" (E.object [ ( "text", E.string levelWithThirdSpawn ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.update Main.UndoPressed
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocRemove", [ "spawns", "2" ] ) ])
        , test "元に戻す: テキストを手で書き換えたら履歴は切れる(古い逆操作を当てない)" <|
            \() ->
                openedLevel
                    |> typeNumberBox "72"
                    |> keydownOn [ class "number-box" ] "Enter"
                    |> ensureKinds [ "applyDocEdit" ]
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelEditedText ) ])
                    |> ensureKinds [ "previewItems" ]
                    -- 読み直し(外の中身を取り直す)で正本が入れ替わる。ツールバーの
                    -- ボタンは無くなったので(自動見張り+409で守られるため)、
                    -- Msg を直に(reloadCurrent は 409 ダイアログ等がまだ使う)
                    |> ProgramTest.update Main.ReloadClicked
                    |> ensureKinds [ "getFile" ]
                    |> respondOk 10 "getFile" (fileBody "assets/level.json" levelText)
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHasNot [ text "↩ 1" ]
                    |> ProgramTest.update Main.UndoPressed
                    |> ProgramTest.expectOutgoingPortValues "apiRequest" kindAndPath (Expect.equal [])
        , test "説明(help): 閉じている間は本文が DOM に無く、? を押すと出る(もう一度で消える)" <|
            \() ->
                openedLevel
                    |> ProgramTest.ensureViewHasNot [ text "背景が流れる速さ。上げるほど道中が短くなる。" ]
                    |> ProgramTest.simulateDomEvent (Query.find [ class "help-toggle" ]) ( "click", E.object [] )
                    |> ProgramTest.ensureViewHas [ text "背景が流れる速さ。上げるほど道中が短くなる。" ]
                    |> ProgramTest.simulateDomEvent (Query.find [ class "help-toggle" ]) ( "click", E.object [] )
                    |> ProgramTest.expectViewHasNot [ text "背景が流れる速さ。上げるほど道中が短くなる。" ]
        , test "行操作(list): ↑ は配列を丸ごと書く applyDocEdit 1 本になる" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "spawns"
                    |> clickRowOp "row-up" 1
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocEdit", [ "spawns" ] ) ])
        , test "行操作(list): 複製も同じ 1 本の書き戻し、✕ はその行の applyDocRemove" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "spawns"
                    |> clickRowOp "row-dup" 0
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocEdit", [ "spawns" ] ) ])
                    |> respondOk 8 "applyDocEdit" (E.object [ ( "text", E.string levelWithThirdSpawn ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> clickRowOp "row-delete" 1
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocRemove", [ "spawns", "1" ] ) ])
        , test "追加(list): applyDocAppend が飛び、応答が届くと新しい行のフォームが出る" <|
            \() ->
                openedLevel
                    |> ProgramTest.clickButton "spawns"
                    |> ProgramTest.ensureViewHas [ text "行をクリックすると下にフォームが出ます" ]
                    |> ProgramTest.clickButton "+ 追加"
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        (D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] (D.list D.string)))
                        (Expect.equal [ ( "applyDocAppend", [ "spawns" ] ) ])
                    |> respondOk 8 "applyDocAppend" (E.object [ ( "text", E.string levelWithThirdSpawn ) ])
                    |> ensureKinds [ "previewItems" ]
                    |> ProgramTest.ensureViewHasNot [ text "行をクリックすると下にフォームが出ます" ]
                    |> ProgramTest.expectView
                        (Query.findAll [ class "form-rows" ] >> Query.count (Expect.greaterThan 0))
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
                    |> ProgramTest.expectViewHas [ text "このファイルは外で変わりました(保存時に確認します)" ]
        , test "一覧の見張り: 新しいファイルが増えた changes 応答で一覧に現れる" <|
            \() ->
                booted
                    -- ファイルは開いていない(一覧を見ているだけ)。既知は
                    -- hitbox.json・assets/level.json の 2 本 — そこへ level2 が増える
                    |> respondOk 0
                        "changes"
                        (changesBodyMany
                            [ ( "hitbox.json", 1 )
                            , ( "assets/level.json", 1 )
                            , ( "assets/level2.json", 1 )
                            ]
                        )
                    |> ensureKinds [ "files", "resources" ]
                    |> respondOk 4 "files" filesBody
                    |> respondOk 5 "resources" resourcesBodyWithLevel2
                    |> ProgramTest.expectViewHas [ text "assets/level2.json" ]
        , test "一覧の見張り: 開いていないファイルの削除で一覧から消える" <|
            \() ->
                booted
                    -- assets/level.json を開かないまま、一覧だけ見ている
                    |> ProgramTest.ensureViewHas [ text "assets/level.json" ]
                    -- changes の応答から assets/level.json が消える(ディスクから削除された)
                    |> respondOk 0 "changes" (changesBodyMany [ ( "hitbox.json", 1 ) ])
                    |> ensureKinds [ "files", "resources" ]
                    |> respondOk 4 "files" filesBody
                    |> respondOk 5 "resources" (E.object [ ( "resources", E.list identity [] ) ])
                    |> ProgramTest.expectViewHasNot [ text "assets/level.json" ]
        , test "一覧の見張り: 開いているファイルが消え、打ちかけが無ければ編集を閉じて知らせる" <|
            \() ->
                openedLevel
                    |> respondOk 0 "changes" (changesBodyMany [ ( "hitbox.json", 1 ) ])
                    |> ProgramTest.expectViewHas [ text "assets/level.json はディスクから消えました(編集を閉じました)" ]
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
        , test "マップ(ビジュアル): マスを見ない行は一覧から選べ、選んだ行のフォームが右に出る" <|
            \() ->
                openedMap
                    -- 印は 1 つも無いが、部屋の行はチップの下の一覧に出る
                    |> ProgramTest.clickButton "▸ +部屋 1"
                    |> clickRoomRow
                    -- インスペクタ: 見出しと、その行のフィールド(言葉・要る持ち物…)
                    |> ProgramTest.ensureViewHas [ text "triggers #0" ]
                    |> ProgramTest.expectViewHas [ text "言葉", text "だれも いない" ]
        , test "マップ(ビジュアル): 選んだ部屋の行の says に 1 行足すと、その行の添字つき path で書き戻す" <|
            \() ->
                openedMap
                    |> ProgramTest.clickButton "▸ +部屋 1"
                    |> clickRoomRow
                    |> ProgramTest.clickButton "＋ 行を追加"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "applyDocEdit", [ "triggers", "0", "says" ] ) ])
        , test "マップ(ビジュアル): 「＋ 部屋の行(enter)」は x,y を書かない enter の行を足す" <|
            \() ->
                openedMap
                    |> ProgramTest.clickButton "＋ 部屋の行(enter)"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        (D.map3 (\kind path keys -> ( kind, path, keys ))
                            (D.field "kind" D.string)
                            (D.at [ "payload", "path" ] (D.list D.string))
                            (D.at [ "payload", "value" ] (D.keyValuePairs D.value) |> D.map (List.map Tuple.first))
                        )
                        (Expect.equal
                            [ ( "applyDocAppend"
                              , [ "triggers" ]
                              , [ "on", "needs", "says", "gives", "once" ]
                              )
                            ]
                        )
        ]
