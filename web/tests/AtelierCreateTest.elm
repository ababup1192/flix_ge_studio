module AtelierCreateTest exposing (suite)

{-| 「つくる」(創作の第一幕)の規則のテスト。

守るのは規則だけ: パネルの既定の開閉(既定は畳む)、プロンプトの
取り寄せと表示、名前の検証、複製の依頼と成功後の行き先。カードの見た目や
コピー札の戻りのタイミング(2 秒)の演出は焼かない。

-}

import Atelier
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


slots : List Atelier.CreateSlot
slots =
    [ { file = "assets/prologue.sprite.json", entityId = Just "villager", kind = "sprite", title = "村人の見た目", hint = "村人の歩き・持ち物の見た目" }
    , { file = "assets/prologue.sfx.json", entityId = Nothing, kind = "sound", title = "効果音", hint = "" }
    ]


{-| スロットが届いた素の状態(候補なし)。育ったゲーム扱い(starterFresh = False)。 -}
fresh : Atelier.Model
fresh =
    Atelier.init
        |> Atelier.gotSlots slots


{-| 生まれたての見本プロジェクト(journey の hasStarterDoc が真)。
インタビューの「見本のまま」チップはこの時だけ意味を持つ。
-}
starterborn : Atelier.Model
starterborn =
    fresh
        |> Atelier.setStarterFresh True


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
            , test "行の「✨ 候補を作る」で開くと押した行にだけ繋留される(候補があっても開いたまま)" <|
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
            , test "パネルの開閉バー(とじる)で閉じ、行への繋留も解ける" <|
                \_ ->
                    begin withCandidates
                        |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                        |> step Atelier.CreateToggled
                        |> Tuple.first
                        |> (\m -> ( Atelier.createOpen m, Atelier.createAnchored m ))
                        |> Expect.equal ( False, False )
            ]
        , describe "AIに作らせる(プロンプト)"
            [ test "「プロンプトを作る」で slot・案数・方向性が飛ぶ" <|
                \_ ->
                    begin fresh
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
                        |> step Atelier.MakePromptClicked
                        |> step Atelier.MakePromptClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            ]
        , describe "あそびを作らせる(ゲームのプロンプト)"
            [ test "生まれたての見本: 何も触らなくても、2問の答え(見本のまま)が direction に載って飛ぶ" <|
                \_ ->
                    begin starterborn
                        |> step Atelier.MakeGamePromptClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutFetchGamePrompt
                                "動かすもの: パドルのまま/終わり方: ぜんぶ壊したら勝ち"
                            )
            , test "インタビューの答え(選択肢・言葉で・エッセンス)と自由記述が 1 本に合成される" <|
                \_ ->
                    begin starterborn
                        |> step (Atelier.GameMoverPicked "猫(しっぽで打ち返す)")
                        |> step (Atelier.GameEndEdited " 3回落としたら終わり ")
                        |> step (Atelier.GameEssenceEdited "夜だけの世界")
                        |> step (Atelier.GameDirectionEdited " 星をバケツで受け止める ")
                        |> step Atelier.MakeGamePromptClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutFetchGamePrompt
                                "星をバケツで受け止める/動かすもの: 猫(しっぽで打ち返す)/終わり方: 3回落としたら終わり/エッセンス: 夜だけの世界"
                            )
            , test "「言葉で」を消すと選択肢の答えに戻る" <|
                \_ ->
                    begin starterborn
                        |> step (Atelier.GameMoverEdited "傘をさしたおじいさん")
                        |> step (Atelier.GameMoverEdited "")
                        |> step Atelier.MakeGamePromptClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutFetchGamePrompt
                                "動かすもの: パドルのまま/終わり方: ぜんぶ壊したら勝ち"
                            )
            , test "育ったゲーム: 見本の 2 問は載らない(エッセンスと自由記述だけが答え)" <|
                \_ ->
                    begin fresh
                        |> step (Atelier.GameEssenceEdited "夜だけの世界")
                        |> step (Atelier.GameDirectionEdited "冬の行商人を足す")
                        |> step Atelier.MakeGamePromptClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutFetchGamePrompt
                                "冬の行商人を足す/エッセンス: 夜だけの世界"
                            )
            , test "届いたプロンプトが箱に映り、コピーで中身が飛ぶ" <|
                \_ ->
                    let
                        model =
                            begin fresh
                                |> step (Atelier.GameDirectionEdited "星をバケツで")
                                |> step Atelier.MakeGamePromptClicked
                                |> Tuple.first
                                |> Atelier.gotGamePrompt "作ってください"
                    in
                    ( Atelier.shownGamePrompt model
                    , Atelier.update Atelier.CopyGamePromptClicked model |> Tuple.second
                    )
                        |> Expect.equal ( Just "作ってください", Atelier.OutCopyPrompt "作ってください" )
            , test "取り寄せ中の二度押しは送らない" <|
                \_ ->
                    begin fresh
                        |> step (Atelier.GameDirectionEdited "星をバケツで")
                        |> step Atelier.MakeGamePromptClicked
                        |> step Atelier.MakeGamePromptClicked
                        |> Tuple.second
                        |> Expect.equal Atelier.OutNone
            , test "プロンプト表示中でも「とじる」で閉じられ、プロンプトは保持される" <|
                \_ ->
                    let
                        closed =
                            begin fresh
                                |> step (Atelier.CreateForSlotClicked "assets/prologue.sprite.json")
                                |> step (Atelier.GameDirectionEdited "星をバケツで")
                                |> step Atelier.MakeGamePromptClicked
                                |> Tuple.first
                                |> Atelier.gotGamePrompt "作ってください"
                                |> Atelier.update Atelier.CreateToggled
                                |> Tuple.first
                    in
                    ( Atelier.createOpen closed, Atelier.shownGamePrompt closed )
                        |> Expect.equal ( False, Just "作ってください" )
            ]
        , describe "なにをつくる?(選択式 1 問)"
            [ test "選ぶとその道だけが出て、「← ほかのつくり方」で選択リストへ戻る" <|
                \_ ->
                    let
                        chosen =
                            begin fresh
                                |> step (Atelier.PathChosen Atelier.PathScaffold)
                                |> Tuple.first

                        back =
                            Atelier.update Atelier.PathCleared chosen |> Tuple.first
                    in
                    ( Atelier.chosenPath fresh, Atelier.chosenPath chosen, Atelier.chosenPath back )
                        |> Expect.equal ( Nothing, Just Atelier.PathScaffold, Nothing )
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
        , describe "スロットの案内(hint)と方向性の例"
            [ test "hint はスロットに追随し、空なら出さない(fail-open)" <|
                \_ ->
                    ( Atelier.selectedSlotHint fresh
                    , begin fresh
                        |> step (Atelier.CreateSlotChosen "assets/prologue.sfx.json")
                        |> Tuple.first
                        |> Atelier.selectedSlotHint
                    )
                        |> Expect.equal ( Just "村人の歩き・持ち物の見た目", Nothing )
            , test "方向性の例はスロットの kind で切り替わり、不明は汎用に倒れる" <|
                \_ ->
                    List.map Atelier.directionPlaceholder [ "sprite", "sound", "nazo" ]
                        |> Expect.equal
                            [ "例: 冬支度の村人。網で虫を追いかける子供"
                            , "例: 短く鋭い斧の音。夕暮れのひぐらし"
                            , "例: イナゴやトンボが飛び、網で虫を追いかける子供がいる晩夏の情景"
                            ]
            ]
        , describe "新しい種類の素材/設定を足す(scaffold)"
            [ test "名前が規則(^[a-z][a-z0-9_]*$)に合わないと送らず、理由がその場に出る" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin fresh
                                |> step (Atelier.ScaffoldKindEdited "Enemy-Wave")
                                |> step Atelier.ScaffoldClicked
                    in
                    ( out, Atelier.scaffoldErrorShown model /= Nothing )
                        |> Expect.equal ( Atelier.OutNone, True )
            , test "規則に合う名前なら kind・表示名・役割が飛ぶ" <|
                \_ ->
                    begin fresh
                        |> step (Atelier.ScaffoldKindEdited " enemy_wave ")
                        |> step (Atelier.ScaffoldTitleEdited "敵の波")
                        |> step (Atelier.ScaffoldRoleChosen "tuning")
                        |> step Atelier.ScaffoldClicked
                        |> Tuple.second
                        |> Expect.equal
                            (Atelier.OutScaffold { kind = "enemy_wave", title = "敵の波", role = "tuning" })
            , test "成功で配線プロンプトが箱に映り、コピーで中身が飛ぶ" <|
                \_ ->
                    let
                        model =
                            begin fresh
                                |> step (Atelier.ScaffoldKindEdited "enemy_wave")
                                |> step Atelier.ScaffoldClicked
                                |> Tuple.first
                                |> Atelier.scaffoldDone { files = [ "assets/enemy_wave.json" ], wirePrompt = "配線して" }
                    in
                    ( Atelier.shownWirePrompt model
                    , Atelier.update Atelier.ScaffoldCopyClicked model |> Tuple.second
                    )
                        |> Expect.equal ( Just "配線して", Atelier.OutCopyPrompt "配線して" )
            , test "サーバの 409 の理由がその場に出て、ボタンが戻る" <|
                \_ ->
                    begin fresh
                        |> step (Atelier.ScaffoldKindEdited "enemy_wave")
                        |> step Atelier.ScaffoldClicked
                        |> Tuple.first
                        |> Atelier.scaffoldFailed "HTTP 409: /scaffold/doc — その種類は既にあります"
                        |> Atelier.scaffoldErrorShown
                        |> Expect.equal (Just "その種類は既にあります")
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
                                [ { file = "a.json", entityId = Nothing, kind = "", title = "", hint = "" }
                                , { file = "b.json", entityId = Just "e", kind = "sfx", title = "音", hint = "" }
                                ]
                            )
            ]
        ]
