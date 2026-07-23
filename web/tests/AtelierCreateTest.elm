module AtelierCreateTest exposing (suite)

{-| 「つくる」(創作の第一幕)の規則のテスト。

守るのは規則だけ: パネルの既定の開閉(既定は畳む)、カードを開いたら
AI 候補づくりフォームに直行すること、プロンプトの取り寄せと表示、
複製の依頼と成功後の行き先。カードの見た目やコピー札の戻りの
タイミング(2 秒)の演出は焼かない。

-}

import Atelier
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


slots : List Atelier.CreateSlot
slots =
    [ { file = "assets/prologue.sprite.json", entityId = Just "villager", kind = "sprite", title = "村人の見た目" }
    , { file = "assets/prologue.sfx.json", entityId = Nothing, kind = "sound", title = "効果音" }
    ]


{-| スロットが届いた素の状態(候補なし)。育ったゲーム扱い(starterFresh = False)。 -}
fresh : Atelier.Model
fresh =
    Atelier.init
        |> Atelier.gotSlots slots


{-| 候補が 1 件ある状態(えらぶが主役になる側)。 -}
withCandidates : Atelier.Model
withCandidates =
    fresh
        |> Atelier.gotCandidates
            { slots =
                [ { slot = "assets/prologue.sprite.json"
                  , entityId = Just "villager"
                  , candidates =
                        [ { file = "atelier/villager.a.sprite.json", note = Nothing, isPrev = False, mtime = 1, previewReady = True } ]
                  , currentPreviewReady = True
                  }
                ]
            , loose = []
            , baking = False
            }


step : Atelier.Msg -> ( Atelier.Model, Atelier.Out ) -> ( Atelier.Model, Atelier.Out )
step msg ( model, _ ) =
    Atelier.update msg model


begin : Atelier.Model -> ( Atelier.Model, Atelier.Out )
begin model =
    ( model, Atelier.OutNone )


suite : Test
suite =
    describe "アトリエの「つくる」"
        [ describe "パネルの開閉"
            [ test "候補ゼロでも畳まれている(開くのは手だけ)" <|
                \_ ->
                    Atelier.createOpen fresh
                        |> Expect.equal False
            , test "候補があっても畳まれている" <|
                \_ ->
                    Atelier.createOpen withCandidates
                        |> Expect.equal False
            , test "境界リンク(CreateForSlotClicked)で開くと押したカードにだけ繋留される(候補があっても開いたまま)" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                                |> Tuple.first
                    in
                    ( Atelier.createAnchoredTo opened "assets/prologue.sprite.json"
                    , Atelier.createAnchoredTo opened "assets/prologue.sfx.json"
                    , Atelier.createAnchored opened
                    )
                        |> Expect.equal ( True, False, True )
            , test "スロットカード: ヘッダで開くと候補づくりがそのカードに繋留され、再クリックで閉じる" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step (Atelier.SlotRowToggled "assets/prologue.sprite.json")
                                |> Tuple.first

                        closed =
                            begin opened
                                |> step (Atelier.SlotRowToggled "assets/prologue.sprite.json")
                                |> Tuple.first
                    in
                    ( Atelier.createAnchoredTo opened "assets/prologue.sprite.json"
                    , ( Atelier.createOpen closed, Atelier.createAnchored closed )
                    )
                        |> Expect.equal ( True, ( False, False ) )
            , test "パネルの開閉バー(とじる)で閉じ、カードへの繋留も解ける" <|
                \_ ->
                    begin withCandidates
                        |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                        |> step Atelier.CreateToggled
                        |> Tuple.first
                        |> (\m -> ( Atelier.createOpen m, Atelier.createAnchored m ))
                        |> Expect.equal ( False, False )
            , test "カードを開くと AI 候補づくりフォームに直行する(道えらびは挟まない)" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                                |> Tuple.first
                    in
                    Atelier.viewCreate opened
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "案をいくつ作るか" ]
                            , Query.has [ Selector.text "プロンプトを作る" ]
                            , Query.hasNot [ Selector.text "なにをつくる?" ]
                            ]
            , test "カードから開いたフォームに対象セレクタは無い(対象はカードで確定 — 選び直させない)" <|
                \_ ->
                    let
                        opened =
                            begin withCandidates
                                |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                                |> Tuple.first
                    in
                    Atelier.viewCreate opened
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.tag "select" ]
            ]
        , describe "プロンプトを作る(対象はカードで確定)"
            [ test "「プロンプトを作る」でカードのスロット・案数・方向性が飛ぶ" <|
                \_ ->
                    begin fresh
                        |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                        |> step (Atelier.CreateCountChosen 5)
                        |> step (Atelier.CreateDirectionEdited " 冬らしく ")
                        |> step Atelier.MakePromptClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutFetchPrompt
                                { slot = "assets/prologue.sprite.json"
                                , count = 5
                                , direction = "冬らしく"
                                }
                            )
            , test "届いたプロンプトが箱に映り、コピーで中身が飛ぶ" <|
                \_ ->
                    let
                        model =
                            begin fresh
                                |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                                |> step Atelier.MakePromptClicked
                                |> Tuple.first
                                |> Atelier.gotPrompt "描いてください"
                    in
                    ( Atelier.shownPrompt model
                    , Atelier.update Atelier.CopyPromptClicked model |> Tuple.second
                    )
                        |> Expect.equal ( Just "描いてください", Atelier.OutCopyPrompt "描いてください" )
            , test "取り寄せ中の二度押しは送らない" <|
                \_ ->
                    begin fresh
                        |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                        |> step Atelier.MakePromptClicked
                        |> step Atelier.MakePromptClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            , test "プロンプト表示中でも「とじる」で閉じられ、プロンプトは保持される" <|
                \_ ->
                    let
                        closed =
                            begin fresh
                                |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                                |> step Atelier.MakePromptClicked
                                |> Tuple.first
                                |> Atelier.gotPrompt "描いてください"
                                |> Atelier.update Atelier.CreateToggled
                                |> Tuple.first
                    in
                    ( Atelier.createOpen closed, Atelier.shownPrompt closed )
                        |> Expect.equal ( False, Just "描いてください" )
            ]
        , describe "手直し(えらぶ側から入る)"
            [ test "候補カードの「手直し」でそのファイルを開く便りが飛び、そうこ側へ切り替わる" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin withCandidates
                                |> step (Atelier.EditCandidateClicked "atelier/villager.a.sprite.json")
                    in
                    ( out, Atelier.showPicks model )
                        |> Expect.equal ( Atelier.OutEditFile "atelier/villager.a.sprite.json", False )
            , test "「写しを作って直す」で自動採番の名前が /atelier/copy へ飛ぶ" <|
                \_ ->
                    begin withCandidates
                        |> step (Atelier.CopyCurrentClicked "assets/prologue.sprite.json")
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutCopyFile { slot = "assets/prologue.sprite.json", name = "prologue.a" })
            , test "自動採番: 既存の a・b を飛ばして c を選ぶ" <|
                \_ ->
                    Atelier.autoCopyName "assets/prologue.sprite.json"
                        [ "atelier/prologue.a.sprite.json", "atelier/prologue.b.sprite.json" ]
                        |> Expect.equal "prologue.c"
            , test "409(衝突)は次の空き番でもう一度、成功でそうこ側へ切り替わる" <|
                \_ ->
                    let
                        sent =
                            begin withCandidates
                                |> step (Atelier.CopyCurrentClicked "assets/prologue.sprite.json")
                                |> Tuple.first

                        ( retried, retry ) =
                            Atelier.copyRetry sent
                    in
                    ( retry
                    , retried |> Atelier.copyDone |> Atelier.showPicks
                    )
                        |> Expect.equal
                            ( Just { slot = "assets/prologue.sprite.json", name = "prologue.b" }
                            , False
                            )
            ]
        , describe "/atelier/slots の橋渡し"
            [ test "壊れた JSON は空(fail-open — カードが準備中になるだけ)" <|
                \_ ->
                    D.decodeString Atelier.createSlotsDecoder "{\"slots\":42}"
                        |> Result.withDefault []
                        |> Expect.equal []
            , test "title 等の欠けは既定に倒し、rows の長さに追随する" <|
                \_ ->
                    D.decodeString Atelier.createSlotsDecoder
                        "{\"slots\":[{\"file\":\"a.json\"},{\"file\":\"b.json\",\"title\":\"音\",\"kind\":\"sfx\",\"entityId\":\"e\"}]}"
                        |> Expect.equal
                            (Ok
                                [ { file = "a.json", entityId = Nothing, kind = "", title = "" }
                                , { file = "b.json", entityId = Just "e", kind = "sfx", title = "音" }
                                ]
                            )
            ]
        ]
