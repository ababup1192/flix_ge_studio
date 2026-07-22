module AtelierFlowTest exposing (suite)

{-| アトリエの装着(swap)の「規則」のテスト。

見た目(カードの並び・オーバーレイの文言送り)は焼かない — ここで守るのは、
何を選べたら装着でき、成功・失敗・未起動で何が起きるか、という規則だけ。

-}

import Atelier
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


{-| 候補が届いた初期状態(スロット 1 つ: 新候補 a と退避版 prev)。 -}
withCandidates : Atelier.Model
withCandidates =
    Atelier.init
        |> Atelier.gotCandidates (candidatesWith { baking = False })


{-| 焼きの有無だけ違う候補一式(previewReady はどちらも真とする)。 -}
candidatesWith : { baking : Bool } -> Atelier.Candidates
candidatesWith { baking } =
    { slots =
        [ { slot = "assets/prologue.sprite.json"
          , entityId = Just "villager"
          , candidates =
                [ { file = "atelier/villager.a.sprite.json", note = Just "丸み", isPrev = False, mtime = 1, previewReady = True }
                , { file = "atelier/prev-1.villager.sprite.json", note = Nothing, isPrev = True, mtime = 0, previewReady = True }
                ]
          , currentPreviewReady = True
          }
        ]
    , loose = []
    , baking = baking
    }


running : Atelier.Model -> Atelier.Model
running =
    Atelier.gotGameStatus True


stopped : Atelier.Model -> Atelier.Model
stopped =
    Atelier.gotGameStatus False


step : Atelier.Msg -> ( Atelier.Model, Atelier.Out ) -> ( Atelier.Model, Atelier.Out )
step msg ( model, _ ) =
    Atelier.update msg model


begin : Atelier.Model -> ( Atelier.Model, Atelier.Out )
begin model =
    ( model, Atelier.OutNone )


select : ( Atelier.Model, Atelier.Out ) -> ( Atelier.Model, Atelier.Out )
select =
    step (Atelier.CandidateClicked "assets/prologue.sprite.json" "atelier/villager.a.sprite.json")


{-| 候補 a のプレビューを押した時の中身(compareWith = スロットのいまの見た目)。 -}
candidatePreview : { file : String, note : Maybe String, compareWith : Maybe String }
candidatePreview =
    { file = "atelier/villager.a.sprite.json"
    , note = Just "丸み"
    , compareWith = Just "assets/prologue.sprite.json"
    }


suite : Test
suite =
    describe "アトリエの装着(swap)"
        [ describe "選びと装着ボタン"
            [ test "選ばずに装着を押しても何も送らない" <|
                \_ ->
                    begin (running withCandidates)
                        |> step Atelier.SwapClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            , test "選んで装着(ゲーム起動中)で promote が送られる" <|
                \_ ->
                    begin (running withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutPromote
                                { candidate = "atelier/villager.a.sprite.json"
                                , slot = "assets/prologue.sprite.json"
                                }
                            )
            , test "promote 飛行中の二度押しは送らない" <|
                \_ ->
                    begin (running withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> step Atelier.SwapClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            ]
        , describe "成功と失敗"
            [ test "成功で選択が畳まれ、閉じると取り直しの合図が返る" <|
                \_ ->
                    let
                        model =
                            begin (running withCandidates)
                                |> select
                                |> step Atelier.SwapClicked
                                |> Tuple.first
                                |> Atelier.promoted (Just "atelier/prev-2.villager.sprite.json")
                    in
                    ( model.selected
                    , Atelier.update Atelier.OverlayClosed model |> Tuple.second
                    )
                        |> Expect.equal ( Nothing, Atelier.OutClosed )
            , test "400 はサーバの日本語の理由だけを見せる(生のエラー行は見せない)" <|
                \_ ->
                    begin (running withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> Tuple.first
                        |> Atelier.promoteFailed "Error: HTTP 400: /atelier/promote — スロットが見つかりません"
                        |> .promoteError
                        |> Expect.equal (Just "スロットが見つかりません")
            ]
        , describe "巻き戻し(rollback)"
            [ test "戻すも同じ promote(candidate = 退避ファイル)で、選択も案内も挟まない" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> step (Atelier.RollbackClicked "assets/prologue.sprite.json" "atelier/prev-1.villager.sprite.json")
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutPromote
                                { candidate = "atelier/prev-1.villager.sprite.json"
                                , slot = "assets/prologue.sprite.json"
                                }
                            )
            ]
        , describe "ゲームが起きていない時の装着"
            [ test "装着を押すと案内が開き、まだ promote は送らない" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin (stopped withCandidates)
                                |> select
                                |> step Atelier.SwapClicked
                    in
                    ( model.gate, out )
                        |> Expect.equal ( True, Atelier.OutNone )
            , test "「起動する」で start が送られる" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> step Atelier.StartGameClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutStartGame
            , test "「そのまま装着」で promote が送られる" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> step Atelier.PromoteAnywayClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutPromote
                                { candidate = "atelier/villager.a.sprite.json"
                                , slot = "assets/prologue.sprite.json"
                                }
                            )
            ]
        , describe "モード分け(候補えらび / そうこ)"
            [ test "候補が 1 件以上あれば候補えらびが既定" <|
                \_ ->
                    Atelier.showPicks withCandidates
                        |> Expect.equal True
            , test "候補ゼロのスロットは捨てられ、そうこが既定になる(防御)" <|
                \_ ->
                    Atelier.init
                        |> Atelier.gotCandidates
                            { slots = [ { slot = "assets/x.schema.json", entityId = Nothing, candidates = [], currentPreviewReady = False } ]
                            , loose = []
                            , baking = False
                            }
                        |> Atelier.showPicks
                        |> Expect.equal False
            , test "「そうこを開く」と「候補えらびに戻る」で行き来できる" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step Atelier.OpenStorehouse
                                |> Tuple.first

                        back =
                            Atelier.update Atelier.OpenPicks opened |> Tuple.first
                    in
                    ( Atelier.showPicks opened, Atelier.showPicks back )
                        |> Expect.equal ( False, True )
            ]
        , describe "ゲーム起動の進み(ポーリングとログ)"
            [ test "起動を押した瞬間からポーリングが回り、二度押しは送らない(ボタン無効の規則)" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin (stopped withCandidates)
                                |> select
                                |> step Atelier.SwapClicked
                                |> step Atelier.StartGameClicked
                                |> step Atelier.StartGameClicked
                    in
                    ( Atelier.isLaunchPolling model, out )
                        |> Expect.equal ( True, Atelier.OutNone )
            , test "running=true が届いたらポーリングは止まる(起動完了)" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> step Atelier.StartGameClicked
                        |> Tuple.first
                        |> Atelier.gotGameStatus True
                        |> Atelier.isLaunchPolling
                        |> Expect.equal False
            , test "make debug の異常終了で止まり、失敗のログは自動で全文展開" <|
                \_ ->
                    let
                        model =
                            begin (stopped withCandidates)
                                |> select
                                |> step Atelier.SwapClicked
                                |> step Atelier.StartGameClicked
                                |> Tuple.first
                                |> Atelier.gotGameLog
                                    { running = False, exitCode = Just 2, lines = [ "boom" ] }
                    in
                    ( Atelier.isLaunchPolling model, model.launchLogExpanded )
                        |> Expect.equal ( False, True )
            , test "既定ではログ全文は開かず、⤢ で開く" <|
                \_ ->
                    let
                        starting =
                            begin (stopped withCandidates)
                                |> select
                                |> step Atelier.SwapClicked
                                |> step Atelier.StartGameClicked

                        expanded =
                            starting |> step Atelier.LaunchLogToggled
                    in
                    ( (Tuple.first starting).launchLogExpanded
                    , (Tuple.first expanded).launchLogExpanded
                    )
                        |> Expect.equal ( False, True )
            ]
        , describe "プレビュー焼き(自動 bake)"
            [ test "baking=true が届いたらポーリングが回り、false で止まる" <|
                \_ ->
                    let
                        during =
                            Atelier.init |> Atelier.gotCandidates (candidatesWith { baking = True })

                        after =
                            during |> Atelier.gotCandidates (candidatesWith { baking = False })
                    in
                    ( Atelier.isBaking Atelier.init, Atelier.isBaking during, Atelier.isBaking after )
                        |> Expect.equal ( False, True, False )
            , test "プレビュー領域の決め: ready なら画像、未完は焼き中、焼きが止まってもまだ無ければ失敗" <|
                \_ ->
                    ( Atelier.previewState { ready = True, baking = True }
                    , Atelier.previewState { ready = False, baking = True }
                    , Atelier.previewState { ready = False, baking = False }
                    )
                        |> Expect.equal ( Atelier.PreviewImage, Atelier.PreviewBaking, Atelier.PreviewFailed )
            , test "焼き待ちの取り直しでも、同じファイルが居るなら選択は保たれる" <|
                \_ ->
                    begin (running withCandidates)
                        |> select
                        |> Tuple.first
                        |> Atelier.gotCandidates (candidatesWith { baking = True })
                        |> .selected
                        |> Expect.equal
                            (Just { slot = "assets/prologue.sprite.json", file = "atelier/villager.a.sprite.json" })
            ]
        , describe "プレビューの拡大(lightbox)"
            [ test "プレビューを押すと拡大が開く(候補の絵から)" <|
                \_ ->
                    begin withCandidates
                        |> step (Atelier.PreviewClicked candidatePreview)
                        |> Tuple.first
                        |> (\m -> ( Atelier.lightboxOpen m, Atelier.lightboxShownFile m ))
                        |> Expect.equal ( True, Just "atelier/villager.a.sprite.json" )
            , test "プレビューを押してもカードの選択は変わらない(未選択のまま装着は送れない)" <|
                \_ ->
                    begin (running withCandidates)
                        |> step (Atelier.PreviewClicked candidatePreview)
                        |> step Atelier.SwapClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            , test "閉じるで拡大が畳まれる" <|
                \_ ->
                    begin withCandidates
                        |> step (Atelier.PreviewClicked candidatePreview)
                        |> step Atelier.LightboxClosed
                        |> Tuple.first
                        |> Atelier.lightboxOpen
                        |> Expect.equal False
            , test "見比べるトグルで「いまの見た目」(file=slot)へ、もう一度で候補へ戻る" <|
                \_ ->
                    let
                        once =
                            begin withCandidates
                                |> step (Atelier.PreviewClicked candidatePreview)
                                |> step Atelier.CompareToggled

                        twice =
                            once |> step Atelier.CompareToggled
                    in
                    ( Atelier.lightboxShownFile (Tuple.first once)
                    , Atelier.lightboxShownFile (Tuple.first twice)
                    )
                        |> Expect.equal
                            ( Just "assets/prologue.sprite.json"
                            , Just "atelier/villager.a.sprite.json"
                            )
            , test "「いまの見た目」の拡大(比べる相手なし)はトグルしても同じ絵のまま" <|
                \_ ->
                    begin withCandidates
                        |> step
                            (Atelier.PreviewClicked
                                { file = "assets/prologue.sprite.json", note = Nothing, compareWith = Nothing }
                            )
                        |> step Atelier.CompareToggled
                        |> Tuple.first
                        |> Atelier.lightboxShownFile
                        |> Expect.equal (Just "assets/prologue.sprite.json")
            ]
        , describe "JSON 橋渡し(candidates)"
            [ test "壊れた JSON でも既定値に倒れる" <|
                \_ ->
                    D.decodeString Atelier.candidatesDecoder "{}"
                        |> Expect.equal (Ok { slots = [], loose = [], baking = False })
            , test "候補の欠けた項目は既定値で埋まる(旧サーバ — preview 系の欠けも fail-open)" <|
                \_ ->
                    D.decodeString Atelier.candidatesDecoder
                        """{"slots":[{"slot":"s","entityId":null,"candidates":[{"file":"f","note":null}]}],"loose":[{"file":"x","reason":"r"}]}"""
                        |> Expect.equal
                            (Ok
                                { slots =
                                    [ { slot = "s"
                                      , entityId = Nothing
                                      , candidates = [ { file = "f", note = Nothing, isPrev = False, mtime = 0, previewReady = False } ]
                                      , currentPreviewReady = False
                                      }
                                    ]
                                , loose = [ { file = "x", reason = "r" } ]
                                , baking = False
                                }
                            )
            , test "game/status の壊れた応答は「走っていない」に倒れる" <|
                \_ ->
                    D.decodeString Atelier.statusDecoder "{}"
                        |> Expect.equal (Ok False)
            ]
        ]
