module TalkViewTest exposing (suite)

{-| 会話の画面の規則のテスト。

守るのは規則だけ: カーソルの行からノードを引く決まり(engine の Yarn.nodeAtLine と同じ)、
頼んでいる間に動いたカーソルは最後の 1 つだけ頼むこと、今の頼みと違う答えは捨てること、
壊れた台本・起こしている最中は前の絵のまま理由を出すこと、窓なしのゲームが落ちたら頼み直さず
「もう一度起こす」で 1 回だけ頼むこと。絵の見た目は焼かない。

-}

import Expect
import Test exposing (Test, describe, test)
import Json.Decode as D
import Dict
import TalkTags
import TalkTree
import TalkView exposing (Boot(..), Headless(..), Out(..), Picker(..), Request(..), ShotAnswer(..), Shown(..))


path : String
path =
    "assets/talk/novel.yarn"


nodes : List TalkView.Node
nodes =
    [ { title = "arrival", file = path, line = 7 }
    , { title = "clock", file = path, line = 36 }
    , { title = "other", file = "assets/talk/x.yarn", line = 30 }
    ]


{-| novel.yarn を開いてノードが届いた所(何も頼んでいない)。 -}
opened : TalkView.Model
opened =
    let
        init =
            TalkView.init
    in
    { init | path = path, text = String.repeat 60 "x\n", saved = String.repeat 60 "x\n", nodes = nodes }


{-| 本文の n 行目の頭の文字位置。 -}
offsetOfLine : Int -> Int
offsetOfLine n =
    (n - 1) * 2


caretTo : Int -> TalkView.Model -> ( TalkView.Model, Out )
caretTo n model =
    TalkView.update (TalkView.CaretMoved model.text (offsetOfLine n)) model


clock : Int -> TalkView.Target
clock line =
    { node = "clock", line = line }


{-| タグを選ぶダイアログの下ごしらえ: 3 行目が girl の台詞(#face:happy)・語彙は表情 3 つと背景 1 つ。 -}
tagged : TalkView.Model
tagged =
    let
        body =
            "title: op\n---\ngirl: できた！ #face:happy\n<<wait 1>>\n==="

        init =
            TalkView.init
    in
    { init
        | path = path
        , text = body
        , saved = body
        , nodes = [ { title = "op", file = path, line = 1 } ]
        , cursor = 3
        , vocab = Just { tags = [ ( "face", [ "neutral", "happy", "sad" ] ), ( "backdrop", [ "fight" ] ) ], speakerTags = [] }
    }


press : List TalkView.Msg -> TalkView.Model -> ( TalkView.Model, Out )
press msgs model =
    List.foldl (\msg ( m, _ ) -> TalkView.update msg m) ( model, OutNone ) msgs


pickerSuite : Test
pickerSuite =
    describe "表情・背景を選ぶダイアログ"
        [ test "開くと語彙の値を並べ、今の値を選んでおく" <|
            \_ ->
                press [ TalkView.TagPickRequested "face" ] tagged
                    |> Tuple.first
                    |> .picker
                    |> Expect.equal (PickerOpen { line = 3, key = "face" } [ "neutral", "happy", "sad" ] 1)
        , test "↑↓ は端で回り、Esc で閉じる(本文は変えない)" <|
            \_ ->
                let
                    ( m1, _ ) =
                        press [ TalkView.TagPickRequested "face", TalkView.PickerKeyed "ArrowDown", TalkView.PickerKeyed "ArrowDown" ] tagged

                    ( m2, out2 ) =
                        press [ TalkView.PickerKeyed "Escape" ] m1
                in
                ( m1.picker, ( m2.picker, m2.text == tagged.text, out2 ) )
                    |> Expect.equal ( PickerOpen { line = 3, key = "face" } [ "neutral", "happy", "sad" ] 0, ( PickerClosed, True, OutNone ) )
        , test "Enter でその行のタグを書き換えて保存する(保存の答えで撮り直す)" <|
            \_ ->
                let
                    ( m1, out ) =
                        press [ TalkView.TagPickRequested "face", TalkView.PickerKeyed "ArrowDown", TalkView.PickerKeyed "Enter" ] tagged

                    expected =
                        "title: op\n---\ngirl: できた！ #face:sad\n<<wait 1>>\n==="
                in
                ( m1.picker, m1.text, out ) |> Expect.equal ( PickerClosed, expected, OutBuild path (Just expected) )
        , test "背景が無い行には語彙の順で足す(押して選ぶのも同じ)" <|
            \_ ->
                press [ TalkView.TagPickRequested "backdrop", TalkView.PickerChosen "fight" ] tagged
                    |> Tuple.first
                    |> .text
                    |> String.lines
                    |> List.drop 2
                    |> List.head
                    |> Expect.equal (Just "girl: できた！ #face:happy #backdrop:fight")
        , test "語彙が無い・タグを付けられない行では開かずに理由を出す" <|
            \_ ->
                let
                    noVocab =
                        press [ TalkView.TagPickRequested "face" ] { tagged | vocab = Nothing } |> Tuple.first

                    command =
                        press [ TalkView.TagPickRequested "face" ] { tagged | cursor = 4 } |> Tuple.first
                in
                ( ( noVocab.picker, noVocab.status /= "" ), ( command.picker, command.status /= "" ) )
                    |> Expect.equal ( ( PickerClosed, True ), ( PickerClosed, True ) )
        , test "別の行へ動いたら閉じる(選んだ値を前の行に書かない)" <|
            \_ ->
                press [ TalkView.TagPickRequested "face", TalkView.CaretMoved tagged.text 0 ] tagged
                    |> Tuple.first
                    |> .picker
                    |> Expect.equal PickerClosed
        , test "語彙はシナリオと同じフォルダの vocab.json" <|
            \_ ->
                ( TalkView.vocabPathOf "assets/talk/opening.yarn", TalkView.vocabPathOf "a.yarn" )
                    |> Expect.equal ( "assets/talk/vocab.json", "vocab.json" )
        ]


{-| 変数が 2 つ宣言された木が届いた所(カーソルは clock の 40 行目・何も頼んでいない)。 -}
withVars : TalkView.Model
withVars =
    TalkView.gotTree
        (Ok
            { variables =
                [ { name = "$gold", value = TalkTree.Number 0, file = path, line = 2 }
                , { name = "$met", value = TalkTree.Flag False, file = path, line = 3 }
                ]
            , nodes = []
            }
        )
        { opened | cursor = 40 }


varsSuite : Test
varsSuite =
    describe "変数の一覧"
        [ test "値を変えると覚えて、今の行を撮り直す" <|
            \_ ->
                TalkView.update (TalkView.VarEdited "$gold" "5") withVars
                    |> Tuple.mapFirst TalkView.varsOf
                    |> Expect.equal ( [ ( "$gold", "5" ) ], OutShot (clock 40) )
        , test "初めの値に戻したら持たない(渡さない)" <|
            \_ ->
                press [ TalkView.VarEdited "$met" "true", TalkView.VarEdited "$met" "false" ] withVars
                    |> Tuple.first
                    |> TalkView.varsOf
                    |> Expect.equal []
        , test "全部を初めの値に戻す(変えていなければ撮り直さない)" <|
            \_ ->
                ( press [ TalkView.VarEdited "$gold" "5", TalkView.VarsResetAll ] withVars |> Tuple.first |> .overrides |> Dict.isEmpty
                , TalkView.update TalkView.VarsResetAll withVars |> Tuple.second
                )
                    |> Expect.equal ( True, OutNone )
        , test "組み直して宣言から消えた変数の値は捨てる" <|
            \_ ->
                press [ TalkView.VarEdited "$gold" "5" ] withVars
                    |> Tuple.first
                    |> TalkView.gotTree (Ok { variables = [], nodes = [] })
                    |> TalkView.varsOf
                    |> Expect.equal []
        ]


graphSuite : Test
graphSuite =
    describe "ノードの図"
        [ test "箱を押すとそのノードの行へカーソルを置いて台本に戻り、欄をその行まで送って撮る" <|
            \_ ->
                press [ TalkView.PaneChosen TalkView.GraphPane, TalkView.GraphNodePicked "clock" ] opened
                    |> Tuple.mapFirst (\m -> ( m.pane, m.cursor ))
                    |> Expect.equal ( ( TalkView.ScriptPane, 36 ), OutBatch [ OutShot (clock 36), OutScrollToLine 36 ] )
        , test "読み進める欄の台詞を押すと、その行をその時の変数の値で撮る" <|
            \_ ->
                let
                    program =
                        { nodes =
                            Dict.fromList
                                [ ( "clock"
                                  , []
                                  )
                                ]
                        , declared = Dict.fromList [ ( "$gold", TalkTree.Number 0 ), ( "$met", TalkTree.Flag False ) ]
                        }

                    m0 =
                        TalkView.gotProgram program withVars

                    ( m1, _ ) =
                        TalkView.update (TalkView.ReadStarted "clock") m0

                    -- 流した後に $gold が 3 になった所を押した、と置く
                    m2 =
                        { m1 | reading = Maybe.map (\r -> { r | vars = Dict.insert "$gold" (TalkTree.Number 3) r.vars }) m1.reading }
                in
                TalkView.update (TalkView.ReadLinePicked 41) m2
                    |> Tuple.mapFirst TalkView.varsOf
                    |> Expect.equal ( [ ( "$gold", "3" ) ], OutShot (clock 41) )
        , test "図を見ている間は ⌘E でダイアログを開かない" <|
            \_ ->
                press [ TalkView.PaneChosen TalkView.GraphPane, TalkView.TagPickRequested "face" ] tagged
                    |> Tuple.first
                    |> .picker
                    |> Expect.equal PickerClosed
        ]


suite : Test
suite =
    describe "TalkView"
        [ pickerSuite
        , varsSuite
        , graphSuite
        , test "行のノードは、このファイルで title: の行が line 以下のいちばん後の物" <|
            \_ ->
                ( TalkView.targetAt path nodes 40, TalkView.targetAt path nodes 7, TalkView.targetAt path nodes 3 )
                    |> Expect.equal ( Just (clock 40), Just { node = "arrival", line = 7 }, Nothing )
        , test "文字位置から行(1 始まり)" <|
            \_ ->
                ( TalkView.lineAt "a\nb\nc" 0, TalkView.lineAt "a\nb\nc" 2, TalkView.lineAt "a\nb\nc" 4 )
                    |> Expect.equal ( 1, 2, 3 )
        , test "何も頼んでいなければ、カーソルの行をすぐ頼む" <|
            \_ ->
                caretTo 40 opened |> Tuple.second |> Expect.equal (OutShot (clock 40))
        , test "頼んでいる間に動いたカーソルは、答えの後に最後の 1 つだけ頼む" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, out2 ) =
                        caretTo 41 m1

                    ( m3, out3 ) =
                        caretTo 42 m2

                    ( m4, out4 ) =
                        TalkView.gotShot (clock 40) (ShotOk "png40") m3
                in
                ( ( out2, out3 ), ( m4.request, out4 ) )
                    |> Expect.equal ( ( OutNone, OutNone ), ( InFlight (clock 42), OutShot (clock 42) ) )
        , test "今の頼みと違う Target の答えは捨てる(絵も頼みも変えない)" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, out ) =
                        TalkView.gotShot (clock 38) (ShotOk "stale") m1
                in
                ( m2.request, m2.shown, out ) |> Expect.equal ( InFlight (clock 40), Blank, OutNone )
        , test "撮れたら今の行の絵を出し、頼みは空く" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, _ ) =
                        TalkView.gotShot (clock 40) (ShotOk "png40") m1
                in
                ( m2.request, m2.shown ) |> Expect.equal ( Idle, Shown { target = clock 40, png = "png40" } )
        , test "起こしている最中は前の絵のまま理由を出し、少し後に頼み直す予約をする" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, _ ) =
                        TalkView.gotShot (clock 40) (ShotOk "png40") m1

                    ( m3, _ ) =
                        caretTo 41 m2

                    ( m4, out ) =
                        TalkView.gotShot (clock 41) (ShotStarting "起こしています" (BootStartingGame 2)) m3
                in
                ( m4.shown, out )
                    |> Expect.equal ( Held { target = clock 40, png = "png40" } "起こしています", OutRetryLater m4.retrySeq )
        , test "起こしている段は答えのまま持つ(jar を作っている・経過秒)" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, _ ) =
                        TalkView.gotShot (clock 40) (ShotStarting "jar を作っています" (BootBuildingJar { secs = 37, round = 1, last = Nothing })) m1
                in
                m2.headless |> Expect.equal (HeadlessBooting (BootBuildingJar { secs = 37, round = 1, last = Nothing }))
        , test "starting の答えの stage と elapsed を段に読む(無ければ段の分からない待ち)" <|
            \_ ->
                let
                    decode json =
                        D.decodeString TalkView.shotAnswerDecoder json
                in
                ( decode """{"ok":false,"headless":"starting","stage":"jar","elapsed":37,"round":2,"lastBuildSeconds":95,"error":{"message":"m"}}"""
                , decode """{"ok":false,"headless":"starting","stage":"boot","elapsed":3,"error":{"message":"m"}}"""
                , decode """{"ok":false,"headless":"starting","error":{"message":"m"}}"""
                )
                    |> Expect.equal
                        ( Ok (ShotStarting "m" (BootBuildingJar { secs = 37, round = 2, last = Just 95 }))
                        , Ok (ShotStarting "m" (BootStartingGame 3))
                        , Ok (ShotStarting "m" BootWaiting)
                        )
        , test "見出しは目安を決め打ちしない(前回の秒数が分かる時だけ言う)・2 人目の作り手は「作り直しています(2 回目)」" <|
            \_ ->
                ( ( TalkView.busyLabel False (HeadlessBooting (BootBuildingJar { secs = 37, round = 1, last = Just 95 }))
                  , TalkView.busyLabel False (HeadlessBooting (BootBuildingJar { secs = 37, round = 1, last = Nothing }))
                  , TalkView.busyLabel False (HeadlessBooting (BootBuildingJar { secs = 4, round = 2, last = Nothing }))
                  )
                , TalkView.busyLabel False (HeadlessBooting (BootStartingGame 3))
                , ( TalkView.busyLabel True HeadlessOk, TalkView.busyLabel False HeadlessOk )
                )
                    |> Expect.equal
                        ( ( "⏳ jar を作っています… 37 秒(前回は 95 秒)"
                          , "⏳ jar を作っています… 37 秒"
                          , "⏳ jar を作り直しています(2 回目)… 4 秒"
                          )
                        , "⏳ 起こしています… 3 秒"
                        , ( "⏳ 撮っています…", "" )
                        )
        , test "長く起きない間は、ログの末尾を持って頼み直しを続ける" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, out ) =
                        TalkView.gotShot (clock 40) (ShotStalled "3 分たっても ready になりません" [ "boom" ]) m1
                in
                ( m2.headless, out ) |> Expect.equal ( HeadlessStalled [ "boom" ], OutRetryLater m2.retrySeq )
        , test "落ちたら頼み直さず、理由とログの末尾を持つ" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, out ) =
                        TalkView.gotShot (clock 40) (ShotCrashed "終了コード 1" [ "boom" ]) m1
                in
                ( ( m2.headless, m2.note ), out ) |> Expect.equal ( ( HeadlessCrashed [ "boom" ], "終了コード 1" ), OutNone )
        , test "「もう一度起こす」は今の行を起こし直しつきで 1 回だけ頼む(頼んでいる間の連打は何もしない)" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, _ ) =
                        TalkView.gotShot (clock 40) (ShotCrashed "終了コード 1" []) m1

                    ( m3, out3 ) =
                        TalkView.update TalkView.RestartClicked m2

                    ( _, out4 ) =
                        TalkView.update TalkView.RestartClicked m3
                in
                ( ( m3.headless, m3.request ), ( out3, out4 ) )
                    |> Expect.equal ( ( HeadlessBooting BootWaiting, InFlight (clock 40) ), ( OutRestart (clock 40), OutNone ) )
        , test "頼み直しの時にカーソルがノードの外なら、頼まずに予約だけ続ける" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 3 opened

                    ( m2, out ) =
                        TalkView.retryFired m1.retrySeq m1
                in
                out |> Expect.equal (OutRetryLater m2.retrySeq)
        , test "古い頼み直しの予約は何も頼まない" <|
            \_ ->
                TalkView.retryFired (opened.retrySeq + 5) opened |> Tuple.second |> Expect.equal OutNone
        , test "壊れた台本を保存したら、前の絵のまま誤りの行を理由に出す" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, _ ) =
                        TalkView.gotShot (clock 40) (ShotOk "png40") m1

                    ( m3, _ ) =
                        TalkView.update TalkView.SaveClicked m2

                    ( m4, out ) =
                        TalkView.gotBuild
                            { ok = False
                            , saved = True
                            , nodes = nodes
                            , problems = [ { file = path, line = 41, msg = "知らない話し手 'mee'（もしかして: me）" } ]
                            , notes = []
                            }
                            m3
                in
                ( m4.shown, out )
                    |> Expect.equal
                        ( Held { target = clock 40, png = "png40" } "台本に誤りがあるので前の絵のまま(41 行目: 知らない話し手 'mee'（もしかして: me）)"
                        , OutNone
                        )
        , test "保存して組めたら、同じ行でも撮り直す" <|
            \_ ->
                let
                    ( m1, _ ) =
                        caretTo 40 opened

                    ( m2, _ ) =
                        TalkView.gotShot (clock 40) (ShotOk "png40") m1

                    ( m3, _ ) =
                        TalkView.update TalkView.SaveClicked m2
                in
                TalkView.gotBuild { ok = True, saved = True, nodes = nodes, problems = [], notes = [] } m3
                    |> Tuple.second
                    |> Expect.equal (OutShot (clock 40))
        ]
