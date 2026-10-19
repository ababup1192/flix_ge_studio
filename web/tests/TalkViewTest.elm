module TalkViewTest exposing (suite)

{-| 会話の画面の規則のテスト。

守るのは規則だけ: カーソルの行からノードを引く決まり(engine の Yarn.nodeAtLine と同じ)、
頼んでいる間に動いたカーソルは最後の 1 つだけ頼むこと、今の頼みと違う答えは捨てること、
壊れた台本・起こしている最中は前の絵のまま理由を出すこと、窓なしのゲームが落ちたら頼み直さず
「もう一度起こす」で 1 回だけ頼むこと。絵の見た目は焼かない。

-}

import Expect
import Test exposing (Test, describe, test)
import TalkView exposing (Headless(..), Out(..), Request(..), ShotAnswer(..), Shown(..))


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


suite : Test
suite =
    describe "TalkView"
        [ test "行のノードは、このファイルで title: の行が line 以下のいちばん後の物" <|
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
                        TalkView.gotShot (clock 41) (ShotStarting "起こしています") m3
                in
                ( m4.shown, out )
                    |> Expect.equal ( Held { target = clock 40, png = "png40" } "起こしています", OutRetryLater m4.retrySeq )
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
                    |> Expect.equal ( ( HeadlessBooting, InFlight (clock 40) ), ( OutRestart (clock 40), OutNone ) )
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
