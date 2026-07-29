module DashboardFlowTest exposing (suite)

{-| ダッシュボード(C2.5)の配線フロー:
宣言 → 一覧の節 → uses 全ファイルの読み込み → のぞき窓 → ジャンプ(dirty 関所込み)。
封筒の出し入れの作法は FlowTest と同じ(elm-program-test+模擬ポート)。
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
import Test.Html.Selector exposing (class, tag, text)


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


ensureKinds : List String -> App -> App
ensureKinds expected =
    ProgramTest.ensureOutgoingPortValues "apiRequest" (D.field "kind" D.string) (Expect.equal expected)


{-| kind とパスの対で検査する(ダッシュボードは同じ kind の getFile が続くため)。 -}
kindAndPath : D.Decoder ( String, String )
kindAndPath =
    D.map2 Tuple.pair (D.field "kind" D.string) (D.at [ "payload", "path" ] D.string)



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
        , ( "hitbox", E.list E.string [] )
        , ( "palette", E.list E.string [] )
        ]


groupBody : String -> String -> String -> E.Value
groupBody id title path =
    E.object
        [ ( "id", E.string id )
        , ( "title", E.string title )
        , ( "files"
          , E.list identity
                [ E.object
                    [ ( "path", E.string path )
                    , ( "schema", E.string (String.dropRight 5 path ++ ".schema.json") )
                    ]
                ]
          )
        ]


resourcesBody : E.Value
resourcesBody =
    E.object
        [ ( "resources"
          , E.list identity
                [ groupBody "enemies" "敵図鑑" "assets/enemies.json"
                , groupBody "weapons" "武器" "assets/weapons.json"
                ]
          )
        , ( "dashboards"
          , E.list identity
                [ E.object
                    [ ( "id", E.string "bestiary" )
                    , ( "title", E.string "モンスター図鑑" )
                    , ( "plugin", E.string "generic" )
                    , ( "uses", E.list E.string [ "enemies", "weapons" ] )
                    ]
                ]
          )
        ]


fileBody : String -> String -> E.Value
fileBody path content =
    E.object [ ( "path", E.string path ), ( "content", E.string content ) ]


enemiesText : String
enemiesText =
    """{
  "enemies": {
    "slime": { "name": "スライム", "hp": 10, "speed": 20.0, "weapon": "claw" },
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
        "weapon": { "type": {"ref": "weapons"}, "label": "武器", "order": 4, "required": true }
      }
    }
  }
}"""


weaponsText : String
weaponsText =
    """{
  "weapons": {
    "claw":   { "name": "爪", "damage": 4, "rate": 1.5 },
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
        "rate":   { "type": "float", "label": "連射", "order": 3 }
      }
    }
  }
}"""



-- 定番の途中状態(id は封筒採番の続き番号)


{-| health(1) → files(2)・resources(3) まで済んだ状態。 -}
booted : App
booted =
    start
        |> ensureKinds [ "health" ]
        |> respondOk 1 "health" healthBody
        |> ensureKinds [ "files", "resources", "goldenStatus", "journeyState" ]
        |> respondOk 2 "files" filesBody
        |> respondOk 3 "resources" resourcesBody
        |> ProgramTest.clickButton "アトリエ"
        -- アトリエは開くたび候補えらび(swap)とアーカイバの材料を取り直す(採番外 id 0)
        |> ensureKinds [ "atelierCandidates", "gameStatus", "atelierSlots", "atelierArchive", "galleryList", "journeyChanges" ]
        -- 入口から調整(Doc エディタ)へ
        |> ProgramTest.clickButton "パラメータを変える"


{-| ボードを開いて uses 全ファイル(4〜7)が届いた状態。 -}
openedBoard : App
openedBoard =
    booted
        |> ProgramTest.clickButton "モンスター図鑑"
        |> ensureKinds [ "getFile", "getFile", "getFile", "getFile" ]
        |> respondOk 4 "getFile" (fileBody "assets/enemies.json" enemiesText)
        |> respondOk 5 "getFile" (fileBody "assets/enemies.schema.json" enemiesSchemaText)
        |> respondOk 6 "getFile" (fileBody "assets/weapons.json" weaponsText)
        |> respondOk 7 "getFile" (fileBody "assets/weapons.schema.json" weaponsSchemaText)


clickPeekCard : App -> App
clickPeekCard =
    ProgramTest.simulateDomEvent
        (Query.find [ class "peek-card" ])
        ( "click", E.object [] )


discardDialogText : String
discardDialogText =
    "移動すると編集は失われます。"



-- テスト本体


suite : Test
suite =
    describe "ダッシュボードのフロー"
        [ test "宣言があると一覧に節が出て、開くと uses 全ファイル(本文+スキーマ)の getFile が飛ぶ" <|
            \() ->
                booted
                    |> ProgramTest.ensureViewHas [ text "ダッシュボード" ]
                    |> ProgramTest.clickButton "モンスター図鑑"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal
                            [ ( "getFile", "assets/enemies.json" )
                            , ( "getFile", "assets/enemies.schema.json" )
                            , ( "getFile", "assets/weapons.json" )
                            , ( "getFile", "assets/weapons.schema.json" )
                            ]
                        )
        , test "エントリを選ぶと ref ののぞき窓(参照先の要約)が出る" <|
            \() ->
                openedBoard
                    |> ProgramTest.clickButton "slime"
                    |> ProgramTest.ensureViewHas [ text "威力: 4" ]
                    |> ProgramTest.expectViewHas [ text "名前: 爪" ]
        , test "ぶら下がり ref は「見つかりません」になる" <|
            \() ->
                openedBoard
                    |> ProgramTest.clickButton "ghost"
                    |> ProgramTest.expectViewHas [ text "\"cursed\" は weapons に見つかりません" ]
        , test "のぞき窓カードのクリックで参照先ファイルが開き、該当エントリが選ばれる" <|
            \() ->
                openedBoard
                    |> ProgramTest.clickButton "slime"
                    |> clickPeekCard
                    |> ProgramTest.ensureOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal
                            [ ( "getFile", "assets/weapons.json" )
                            , ( "getFile", "assets/weapons.schema.json" )
                            ]
                        )
                    |> respondOk 8 "getFile" (fileBody "assets/weapons.json" weaponsText)
                    |> respondOk 9 "getFile" (fileBody "assets/weapons.schema.json" weaponsSchemaText)
                    |> ProgramTest.expectView
                        (Query.find [ class "entry-id" ] >> Query.has [ text "claw" ])
        , test "dirty のままのぞき窓ジャンプ: 関所が出て、破棄を選ぶと開く" <|
            \() ->
                booted
                    |> ProgramTest.clickButton "assets/enemies.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/enemies.json" enemiesText)
                    |> respondOk 5 "getFile" (fileBody "assets/enemies.schema.json" enemiesSchemaText)
                    -- スキーマ確定で横断辞書(weapons の本文+スキーマ)も取りに行く
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 6 "getFile" (fileBody "assets/weapons.json" weaponsText)
                    |> respondOk 7 "getFile" (fileBody "assets/weapons.schema.json" weaponsSchemaText)
                    -- つまみ系 Doc は常時 2 ペイン。右下の JSON を直に汚す
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ class "json-box" ])
                        ( "input", E.object [ ( "target", E.object [ ( "value", E.string "{ }" ) ] ) ] )
                    |> ProgramTest.clickButton "モンスター図鑑"
                    |> ensureKinds [ "getFile", "getFile", "getFile", "getFile" ]
                    |> respondOk 8 "getFile" (fileBody "assets/enemies.json" enemiesText)
                    |> respondOk 9 "getFile" (fileBody "assets/enemies.schema.json" enemiesSchemaText)
                    |> respondOk 10 "getFile" (fileBody "assets/weapons.json" weaponsText)
                    |> respondOk 11 "getFile" (fileBody "assets/weapons.schema.json" weaponsSchemaText)
                    |> ProgramTest.clickButton "slime"
                    |> clickPeekCard
                    -- 答えるまで getFile は飛ばない
                    |> ensureKinds []
                    |> ProgramTest.ensureViewHas [ text discardDialogText ]
                    |> ProgramTest.clickButton "破棄して開く"
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal
                            [ ( "getFile", "assets/weapons.json" )
                            , ( "getFile", "assets/weapons.schema.json" )
                            ]
                        )
        , test "宣言ファイルを開くとスキーマ確定で横断辞書(他の宣言ファイル)を取りに行く" <|
            \() ->
                booted
                    |> ProgramTest.clickButton "assets/enemies.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/enemies.json" enemiesText)
                    |> respondOk 5 "getFile" (fileBody "assets/enemies.schema.json" enemiesSchemaText)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal
                            [ ( "getFile", "assets/weapons.json" )
                            , ( "getFile", "assets/weapons.schema.json" )
                            ]
                        )
        , test "横断辞書が届くと武器ドロップダウンに claw / cannon が並び、実在参照は問題にならない" <|
            \() ->
                booted
                    |> ProgramTest.clickButton "assets/enemies.json"
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/enemies.json" enemiesText)
                    |> respondOk 5 "getFile" (fileBody "assets/enemies.schema.json" enemiesSchemaText)
                    |> ensureKinds [ "getFile", "getFile" ]
                    |> respondOk 6 "getFile" (fileBody "assets/weapons.json" weaponsText)
                    |> respondOk 7 "getFile" (fileBody "assets/weapons.schema.json" weaponsSchemaText)
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "tr", Test.Html.Selector.containing [ text "slime" ] ])
                        ( "click", E.object [] )
                    |> ProgramTest.ensureViewHas
                        [ tag "option", text "cannon" ]
                    -- 問題は ghost の "cursed"(本当のぶら下がり)1 件だけ
                    |> ProgramTest.expectView
                        (Query.find [ class "problem-head" ] >> Query.has [ text "1" ])
        , test "ボードの文書とスキーマが揃うと肖像(uiDoc)のエンジン焼き previewUi が飛ぶ" <|
            \() ->
                let
                    withPortraitSchema =
                        String.replace
                            "\"weapon\": { \"type\": {\"ref\": \"weapons\"}, \"label\": \"武器\", \"order\": 4, \"required\": true }"
                            "\"weapon\": { \"type\": {\"ref\": \"weapons\"}, \"label\": \"武器\", \"order\": 4, \"required\": true },\n        \"portrait\": { \"type\": \"text\", \"widget\": \"uiDoc\", \"label\": \"肖像\", \"order\": 5 }"
                            enemiesSchemaText

                    withPortraitDoc =
                        String.replace "\"weapon\": \"claw\" }"
                            "\"weapon\": \"claw\", \"portrait\": \"assets/portraits/slime.ui.json\" }"
                            enemiesText
                in
                booted
                    |> ProgramTest.clickButton "モンスター図鑑"
                    |> ensureKinds [ "getFile", "getFile", "getFile", "getFile" ]
                    |> respondOk 4 "getFile" (fileBody "assets/enemies.json" withPortraitDoc)
                    |> respondOk 5 "getFile" (fileBody "assets/enemies.schema.json" withPortraitSchema)
                    |> ProgramTest.expectOutgoingPortValues "apiRequest"
                        kindAndPath
                        (Expect.equal [ ( "previewUi", "assets/portraits/slime.ui.json" ) ])
        ]
