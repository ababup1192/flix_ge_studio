module AtelierFlowTest exposing (suite)

{-| アトリエの採用(swap)の「規則」のテスト。

見た目(カードの並び・オーバーレイの文言送り)は焼かない — ここで守るのは、
何を選べたら採用でき、成功・失敗・未起動で何が起きるか、という規則だけ。

-}

import Atelier
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


{-| 候補が届いた初期状態(スロット 1 つ: 新候補 a と過去バージョン prev)。 -}
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
candidatePreview : { file : String, note : Maybe String, compareWith : Maybe String, isPrev : Bool }
candidatePreview =
    { file = "atelier/villager.a.sprite.json"
    , note = Just "丸み"
    , compareWith = Just "assets/prologue.sprite.json"
    , isPrev = False
    }


{-| 過去バージョン(prev)のプレビューを押した時の中身。 -}
prevPreview : { file : String, note : Maybe String, compareWith : Maybe String, isPrev : Bool }
prevPreview =
    { file = "atelier/prev-1.villager.sprite.json"
    , note = Nothing
    , compareWith = Just "assets/prologue.sprite.json"
    , isPrev = True
    }


suite : Test
suite =
    describe "アトリエの切り替え(swap)"
        [ describe "選びと切り替えボタン"
            [ test "選ばずに切り替えを押しても何も送らない" <|
                \_ ->
                    begin (running withCandidates)
                        |> step Atelier.SwapClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            , test "選んで切り替え(ゲーム起動中)で promote が送られる" <|
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
            [ test "戻すも同じ promote(candidate = 前のバージョンのファイル)で、選択も案内も挟まない" <|
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
        , describe "ゲームが起きていない時の切り替え"
            [ test "切り替えを押すと案内が開き、まだ promote は送らない" <|
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
            , test "「そのまま切り替える」で promote が送られる" <|
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
        , describe "セクション分け(入口 / 素材 / 調整 / アーカイブ)"
            [ test "最初は入口(候補があっても勝手に素材へ飛ばない)" <|
                \_ ->
                    ( Atelier.showLanding withCandidates, Atelier.showPicks withCandidates )
                        |> Expect.equal ( True, False )
            , test "素材を開いて「← アトリエ」で入口へ戻れる" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step Atelier.OpenPicks
                                |> Tuple.first

                        back =
                            Atelier.update Atelier.OpenLanding opened |> Tuple.first
                    in
                    ( Atelier.showPicks opened, Atelier.showLanding back )
                        |> Expect.equal ( True, True )
            , test "調整を開くと入口でも素材でもアーカイブでもない" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step Atelier.OpenStorehouse
                                |> Tuple.first
                    in
                    ( Atelier.showLanding opened, Atelier.showPicks opened, Atelier.showArchiver opened )
                        |> Expect.equal ( False, False, False )
            , test "アーカイブを開くと第 3 のセクション(入口でも素材でもない)" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step Atelier.OpenArchiver
                                |> Tuple.first
                    in
                    ( Atelier.showArchiver opened, Atelier.showLanding opened, Atelier.showPicks opened )
                        |> Expect.equal ( True, False, False )
            , test "候補ゼロのスロットは捨てられる(防御 — ファイル名の羅列を出さない)" <|
                \_ ->
                    Atelier.init
                        |> Atelier.gotCandidates
                            { slots = [ { slot = "assets/x.schema.json", entityId = Nothing, candidates = [], currentPreviewReady = False } ]
                            , loose = []
                            , baking = False
                            }
                        |> Atelier.hasCandidates
                        |> Expect.equal False
            ]
        , describe "素材スロットの行(素材を切り替えるの一覧)"
            [ test "宣言された素材が全部行になり、候補はスロット別に付き、いまの素材の vN はアーカイブ履歴から導く" <|
                \_ ->
                    let
                        model =
                            withCandidates
                                |> Atelier.gotSlots
                                    [ { file = "assets/prologue.sprite.json", entityId = Just "villager", kind = "sprite", title = "村人\u{FF08}主役\u{FF09}" }
                                    , { file = "assets/theme.json", entityId = Nothing, kind = "theme", title = "色" }
                                    ]
                                |> Atelier.gotArchive
                                    { history =
                                        [ { file = "atelier/archive/prologue.v2.sprite.json", slot = "assets/prologue.sprite.json", ver = 2, note = Nothing, mtime = 2 }
                                        , { file = "atelier/archive/prologue.v1.sprite.json", slot = "assets/prologue.sprite.json", ver = 1, note = Nothing, mtime = 1 }
                                        ]
                                    , candidates = []
                                    }
                    in
                    Atelier.pickRows model
                        |> List.map
                            (\row ->
                                ( row.title
                                , row.slot |> Maybe.map (\s -> List.length s.candidates) |> Maybe.withDefault 0
                                , Atelier.slotVersion model row.file
                                )
                            )
                        -- ヘッダの題は宣言題そのまま(全角括弧も切らない)。
                        -- 候補ゼロのスロットも行として残り、履歴なしは v1
                        |> Expect.equal [ ( "村人\u{FF08}主役\u{FF09}", 2, 3 ), ( "色", 0, 1 ) ]
            , test "文章に織り込む題(titleBeforeParen)は全角・半角どちらの括弧でも前だけ残す" <|
                \_ ->
                    [ "ドット絵\u{FF08}額縁\u{FF09}", "レベル(調整卓)", "色" ]
                        |> List.map Atelier.titleBeforeParen
                        |> Expect.equal [ "ドット絵", "レベル", "色" ]
            ]
        , describe "アーカイバ(何も捨てない置き場)"
            [ test "🗃 でアーカイブ送りの便りが飛び、飛行中の二度押しは送らない" <|
                \_ ->
                    let
                        first =
                            begin withCandidates
                                |> step (Atelier.ArchiveClicked "atelier/villager.a.sprite.json")

                        second =
                            first |> step (Atelier.ArchiveClicked "atelier/villager.a.sprite.json")
                    in
                    ( Tuple.second first, Tuple.second second )
                        |> Expect.equal
                            ( Atelier.OutArchive "atelier/villager.a.sprite.json", Atelier.OutNone )
            , test "「↩ 復活」で restore の便りが飛び、応答(成功も失敗も)で ⏳ が畳まれて次を送れる" <|
                \_ ->
                    let
                        first =
                            begin withCandidates
                                |> step (Atelier.RestoreClicked "atelier/archive/villager.b.sprite.json")

                        settled =
                            Atelier.archiveSettled (Tuple.first first)
                    in
                    ( Tuple.second first
                    , first |> step (Atelier.RestoreClicked "atelier/archive/x.json") |> Tuple.second
                    , Atelier.update (Atelier.RestoreClicked "atelier/archive/x.json") settled |> Tuple.second
                    )
                        |> Expect.equal
                            ( Atelier.OutRestore "atelier/archive/villager.b.sprite.json"
                            , Atelier.OutNone
                            , Atelier.OutRestore "atelier/archive/x.json"
                            )
            , test "「この版に戻す」は既存の promote(candidate = 版札、アーカイブの札は消えない約束)" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> step (Atelier.RollbackClicked "assets/block2.sprite.json" "atelier/archive/block2.v3.sprite.json")
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutPromote
                                { candidate = "atelier/archive/block2.v3.sprite.json"
                                , slot = "assets/block2.sprite.json"
                                }
                            )
            , test "アーカイバ一覧のデコーダ(JSON→型、null の note は Nothing)" <|
                \_ ->
                    D.decodeString Atelier.archiveDecoder
                        """{"history":[{"file":"atelier/archive/block2.v3.sprite.json","slot":"assets/block2.sprite.json","ver":3,"note":null,"mtime":5}],"candidates":[{"file":"atelier/archive/villager.b.sprite.json","entityId":"villager","note":"丸み","mtime":4}]}"""
                        |> Expect.equal
                            (Ok
                                { history =
                                    [ { file = "atelier/archive/block2.v3.sprite.json"
                                      , slot = "assets/block2.sprite.json"
                                      , ver = 3
                                      , note = Nothing
                                      , mtime = 5
                                      }
                                    ]
                                , candidates =
                                    [ { file = "atelier/archive/villager.b.sprite.json"
                                      , entityId = Just "villager"
                                      , note = Just "丸み"
                                      , mtime = 4
                                      }
                                    ]
                                }
                            )
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
            , test "しくじった後も実況パネルの材料は残る(failed で出る)" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> step Atelier.StartGameClicked
                        |> Tuple.first
                        |> Atelier.gotGameLog
                            { running = False, exitCode = Just 2, lines = [ "boom" ] }
                        |> Atelier.launchLine
                        |> Expect.equal
                            (Just { lines = [ "boom" ], expanded = True, failed = True })
            , test "しくじりへ落ちた 1 手だけ launchJustFailed が True" <|
                \_ ->
                    let
                        starting =
                            begin (stopped withCandidates)
                                |> select
                                |> step Atelier.SwapClicked
                                |> step Atelier.StartGameClicked
                                |> Tuple.first

                        failed =
                            starting
                                |> Atelier.gotGameLog
                                    { running = False, exitCode = Just 2, lines = [ "boom" ] }

                        again =
                            failed
                                |> Atelier.gotGameLog
                                    { running = False, exitCode = Just 2, lines = [ "boom" ] }
                    in
                    ( Atelier.launchJustFailed starting failed
                    , Atelier.launchJustFailed failed again
                    )
                        |> Expect.equal ( True, False )
            , test "静かに終わった(exitCode 0)のはしくじりにしない" <|
                \_ ->
                    begin (stopped withCandidates)
                        |> select
                        |> step Atelier.SwapClicked
                        |> step Atelier.StartGameClicked
                        |> Tuple.first
                        |> Atelier.gotGameLog
                            { running = False, exitCode = Just 0, lines = [] }
                        |> Atelier.launchHasFailed
                        |> Expect.equal False
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
            , test "プレビューを押してもカードの選択は変わらない(未選択のまま切り替えは送れない)" <|
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
                                { file = "assets/prologue.sprite.json", note = Nothing, compareWith = Nothing, isPrev = False }
                            )
                        |> step Atelier.CompareToggled
                        |> Tuple.first
                        |> Atelier.lightboxShownFile
                        |> Expect.equal (Just "assets/prologue.sprite.json")
            , test "候補のライトボックスから切り替え — 拡大が畳まれ、既存フローで promote が送られる" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin (running withCandidates)
                                |> step (Atelier.PreviewClicked candidatePreview)
                                |> step Atelier.LightboxSwapClicked
                    in
                    ( Atelier.lightboxOpen model, out )
                        |> Expect.equal
                            ( False
                            , Atelier.OutPromote
                                { candidate = "atelier/villager.a.sprite.json"
                                , slot = "assets/prologue.sprite.json"
                                }
                            )
            , test "候補のライトボックスから切り替え(ゲーム未起動)— 既存の案内(gate)がそのまま効く" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin (stopped withCandidates)
                                |> step (Atelier.PreviewClicked candidatePreview)
                                |> step Atelier.LightboxSwapClicked
                    in
                    ( model.gate, out )
                        |> Expect.equal ( True, Atelier.OutNone )
            , test "prev のライトボックスは「戻す」— 既存のロールバック経路で promote が送られる" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin (stopped withCandidates)
                                |> step (Atelier.PreviewClicked prevPreview)
                                |> step Atelier.LightboxRollbackClicked
                    in
                    ( Atelier.lightboxOpen model, out )
                        |> Expect.equal
                            ( False
                            , Atelier.OutPromote
                                { candidate = "atelier/prev-1.villager.sprite.json"
                                , slot = "assets/prologue.sprite.json"
                                }
                            )
            ]
        , describe "カード内の切り替え導線(選んだカードにだけ決めボタン)"
            [ test "未選択のカードには何も出ない" <|
                \_ ->
                    Atelier.cardAction withCandidates
                        { slot = "assets/prologue.sprite.json"
                        , file = "atelier/villager.a.sprite.json"
                        , isPrev = False
                        }
                        |> Expect.equal Nothing
            , test "選んだカードにだけ「切り替え」が出る(隣のカードには出ない)" <|
                \_ ->
                    let
                        model =
                            begin withCandidates |> select |> Tuple.first
                    in
                    ( Atelier.cardAction model
                        { slot = "assets/prologue.sprite.json"
                        , file = "atelier/villager.a.sprite.json"
                        , isPrev = False
                        }
                    , Atelier.cardAction model
                        { slot = "assets/prologue.sprite.json"
                        , file = "atelier/prev-1.villager.sprite.json"
                        , isPrev = True
                        }
                    )
                        |> Expect.equal ( Just Atelier.Swap, Nothing )
            , test "前のバージョン(prev)を選ぶとボタンは「戻す」になる" <|
                \_ ->
                    begin withCandidates
                        |> step (Atelier.CandidateClicked "assets/prologue.sprite.json" "atelier/prev-1.villager.sprite.json")
                        |> Tuple.first
                        |> (\model ->
                                Atelier.cardAction model
                                    { slot = "assets/prologue.sprite.json"
                                    , file = "atelier/prev-1.villager.sprite.json"
                                    , isPrev = True
                                    }
                           )
                        |> Expect.equal (Just Atelier.Rollback)
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
