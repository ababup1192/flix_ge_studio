module NewGameTest exposing (suite)

{-| 「＋ 新しいゲームをはじめる」(まっさらから)の規則のテスト。

守るのは規則だけ: 名前の検証ゲート、ポーリングの始まりと止まり、
成功で /projects を取り直して選択フローへ乗ること、404 の fail-open(準備中)。
チップの見た目やパネルの文言の演出は焼かない。

-}

import Effect exposing (Effect)
import Expect
import Json.Decode as D
import Json.Encode as E
import Main
import NewGame
import SketchPad
import Test exposing (Test, describe, test)


step : NewGame.Msg -> ( NewGame.Model, NewGame.Out ) -> ( NewGame.Model, NewGame.Out )
step msg ( model, _ ) =
    NewGame.update msg model


begin : ( NewGame.Model, NewGame.Out )
begin =
    ( NewGame.init, NewGame.OutNone )


{-| 規則に合う入力を済ませた状態。 -}
filled : ( NewGame.Model, NewGame.Out )
filled =
    begin
        |> step (NewGame.NameEdited "block_breaker2")
        |> step (NewGame.TitleEdited "くずしブロック2")
        |> step (NewGame.PresetChosen 240 320)


creating : NewGame.Model
creating =
    filled |> step NewGame.CreateClicked |> Tuple.first



-- Main の配線(封筒の kind だけ検査する)


kindsOf : Effect -> List String
kindsOf effect =
    case effect of
        Effect.SendApi req ->
            [ req.kind ]

        Effect.Batch effects ->
            List.concatMap kindsOf effects

        _ ->
            []


envelope : String -> Bool -> E.Value -> E.Value
envelope kind ok body =
    E.object
        [ ( "id", E.int 0 )
        , ( "kind", E.string kind )
        , ( "ok", E.bool ok )
        , ( "body", body )
        ]


updateM : Main.Msg -> Main.Model -> Main.Model
updateM msg model =
    Main.update msg model |> Tuple.first


{-| ピッカー画面にいる Main(health が「プロジェクト未選択」を返した後)。 -}
pickerModel : Main.Model
pickerModel =
    Main.init ()
        |> Tuple.first
        |> updateM (Main.GotApiResponse (envelope "health" True (E.object [ ( "ok", E.bool False ) ])))


suite : Test
suite =
    describe "まっさらから(新しいゲーム)"
        [ describe "名前の検証ゲート"
            [ test "規則に合わない名前は生きた検証が言葉で教える" <|
                \_ ->
                    List.map NewGame.nameError [ "", "block_breaker2", "Abc", "2abc", "a-b" ]
                        |> List.map ((/=) Nothing)
                        |> Expect.equal [ False, False, True, True, True ]
            , test "規則に合わない名前では封筒が飛ばず、理由がその場に出る" <|
                \_ ->
                    let
                        ( model, out ) =
                            begin
                                |> step (NewGame.NameEdited "Abc")
                                |> step NewGame.CreateClicked
                    in
                    ( out, NewGame.shownError model /= Nothing )
                        |> Expect.equal ( NewGame.OutNone, True )
            , test "規則に合えば name・題名・サイズが飛ぶ" <|
                \_ ->
                    filled
                        |> step NewGame.CreateClicked
                        |> Tuple.second
                        |> Expect.equal
                            (NewGame.OutCreate
                                { name = "block_breaker2", title = "くずしブロック2", w = 240, h = 320, starter = "" }
                            )
            ]
        , describe "ポーリングの始まりと止まり"
            [ test "つくるでポーリングが始まる" <|
                \_ ->
                    NewGame.isPolling creating
                        |> Expect.equal True
            , test "running の間は回り続け、exitCode 0 で止まって 202 の dir で誕生を告げる" <|
                \_ ->
                    let
                        ( m1, r1 ) =
                            NewGame.gotLog { running = True, exitCode = Nothing, lines = [ "copy" ] }
                                (NewGame.accepted (Just "/games/block_breaker2") creating)

                        ( m2, r2 ) =
                            NewGame.gotLog { running = False, exitCode = Just 0, lines = [] } m1
                    in
                    ( ( NewGame.isPolling m1, r1 )
                    , ( NewGame.isPolling m2, r2 )
                    )
                        |> Expect.equal
                            ( ( True, NewGame.LogContinue )
                            , ( False, NewGame.LogSuccess { dir = Just "/games/block_breaker2" } )
                            )
            , test "exitCode が 0 以外なら止まって失敗(ログは隠さない)" <|
                \_ ->
                    let
                        ( model, result ) =
                            NewGame.gotLog { running = False, exitCode = Just 2, lines = [ "boom" ] } creating
                    in
                    ( NewGame.isPolling model, result )
                        |> Expect.equal ( False, NewGame.LogFailure )
            , test "400/409 の理由がその場に出て、ポーリングは止まる" <|
                \_ ->
                    let
                        model =
                            NewGame.createFailed "HTTP 409: /projects/new — その名前は既にあります" creating
                    in
                    ( NewGame.isPolling model, NewGame.shownError model )
                        |> Expect.equal ( False, Just "その名前は既にあります" )
            ]
        , describe "Main の配線"
            [ test "つくるで projectNew の封筒が飛ぶ" <|
                \_ ->
                    pickerModel
                        |> updateM (Main.NewGameMsg (NewGame.NameEdited "poyo"))
                        |> Main.update (Main.NewGameMsg NewGame.CreateClicked)
                        |> Tuple.second
                        |> kindsOf
                        |> Expect.equal [ "projectNew" ]
            , test "誕生(exitCode 0)で /projects を取り直し、202 の dir で選択フローに乗る" <|
                \_ ->
                    pickerModel
                        |> updateM (Main.NewGameMsg (NewGame.NameEdited "poyo"))
                        |> updateM (Main.NewGameMsg NewGame.CreateClicked)
                        -- 202 応答が dir(産まれるゲームの絶対パス)を教える
                        |> updateM
                            (Main.GotApiResponse
                                (envelope "projectNew"
                                    True
                                    (E.object [ ( "ok", E.bool True ), ( "dir", E.string "/games/poyo" ) ])
                                )
                            )
                        |> Main.update
                            (Main.GotApiResponse
                                (envelope "projectNewLog"
                                    True
                                    (E.object
                                        [ ( "running", E.bool False )
                                        , ( "target", E.string "new-game" )
                                        , ( "exitCode", E.int 0 )
                                        , ( "lines", E.list E.string [] )
                                        ]
                                    )
                                )
                            )
                        |> Tuple.mapSecond kindsOf
                        |> Tuple.mapFirst (\m -> m.picker.busy)
                        |> Expect.equal ( Just "/games/poyo", [ "projects", "selectProject" ] )
            , test "dir の無い 202(旧サーバ)なら /projects 再取得だけに倒す(fail-open)" <|
                \_ ->
                    pickerModel
                        |> updateM (Main.NewGameMsg (NewGame.NameEdited "poyo"))
                        |> updateM (Main.NewGameMsg NewGame.CreateClicked)
                        |> updateM
                            (Main.GotApiResponse
                                (envelope "projectNew" True (E.object [ ( "ok", E.bool True ) ]))
                            )
                        |> Main.update
                            (Main.GotApiResponse
                                (envelope "projectNewLog"
                                    True
                                    (E.object [ ( "running", E.bool False ), ( "exitCode", E.int 0 ) ])
                                )
                            )
                        |> Tuple.second
                        |> kindsOf
                        |> Expect.equal [ "projects" ]
            , test "404(旧サーバ)は準備中に倒れ、ポーリングは止まる(fail-open)" <|
                \_ ->
                    let
                        model =
                            pickerModel
                                |> updateM (Main.NewGameMsg (NewGame.NameEdited "poyo"))
                                |> updateM (Main.NewGameMsg NewGame.CreateClicked)
                                |> updateM
                                    (Main.GotApiResponse
                                        (envelope "projectNew"
                                            False
                                            (E.object [ ( "message", E.string "HTTP 404: /projects/new" ) ])
                                        )
                                    )
                    in
                    ( NewGame.isPolling model.newGame, NewGame.shownError model.newGame /= Nothing )
                        |> Expect.equal ( False, True )
            ]
        , describe "/projects/new/log の橋渡し"
            [ test "欠けたフィールドは既定に倒す(fail-open)" <|
                \_ ->
                    D.decodeString NewGame.logDecoder "{}"
                        |> Expect.equal
                            (Ok { running = False, exitCode = Nothing, lines = [] })
            ]
        , describe "ジャンルえらび(genesis)"
            [ test "/genesis/families の橋渡し: 並びはそのまま、id 以外の欠けは空文字に倒す" <|
                \_ ->
                    D.decodeString NewGame.familiesDecoder
                        """{"families":[
                             {"id":"action","name":"アクション","verb":"走って、跳んで、壊す","includes":"ブロック崩し","controls":"←→ + スペース","starter":"templates/game-starter"},
                             {"id":"free"}
                           ]}"""
                        |> Expect.equal
                            (Ok
                                [ { id = "action", name = "アクション", verb = "走って、跳んで、壊す", includes = "ブロック崩し", controls = "←→ + スペース", starter = "templates/game-starter" }
                                , { id = "free", name = "", verb = "", includes = "", controls = "", starter = "" }
                                ]
                            )
            , test "starter 無しのジャンルを選ぶと、その場で公式プロンプトを取りに行く" <|
                \_ ->
                    begin
                        |> Tuple.mapFirst (NewGame.gotFamilies genesisFamilies)
                        |> step (NewGame.FamilyChosen "rpg")
                        |> Tuple.second
                        |> Expect.equal
                            (NewGame.OutFetchGenesisPrompt { family = "rpg", direction = "" })
            , test "free は言葉(direction)が必須 — 空では飛ばず、書けば飛ぶ" <|
                \_ ->
                    let
                        chosen =
                            begin
                                |> Tuple.mapFirst (NewGame.gotFamilies genesisFamilies)
                                |> step (NewGame.FamilyChosen "free")

                        empty =
                            chosen
                                |> step NewGame.FreePromptRequested
                                |> Tuple.second

                        written =
                            chosen
                                |> step (NewGame.FreeDirectionEdited " 猫が屋根を跳びわたる ")
                                |> step NewGame.FreePromptRequested
                                |> Tuple.second
                    in
                    ( empty, written )
                        |> Expect.equal
                            ( NewGame.OutNone
                            , NewGame.OutFetchGenesisPrompt { family = "free", direction = "猫が屋根を跳びわたる" }
                            )
            ]
        , describe "画面のラフから(ラフ札)"
            [ test "ラフ札を選ぶと、依頼文の土台(blank の公式文)を裏で取りに行く" <|
                \_ ->
                    begin
                        |> step (NewGame.FamilyChosen NewGame.sketchFamilyId)
                        |> Tuple.second
                        |> Expect.equal
                            (NewGame.OutFetchGenesisPrompt { family = "blank", direction = "" })
            , test "名前が規則に合えば game-starter の複製が飛ぶ(w/h は使わない)" <|
                \_ ->
                    sketchFilled
                        |> step NewGame.CreateClicked
                        |> Tuple.second
                        |> Expect.equal
                            (NewGame.OutCreate
                                { name = "rough_game", title = "ラフのゲーム", w = 0, h = 0, starter = "templates/game-starter" }
                            )
            , test "規則に合わない名前では封筒が飛ばない(ラフ札も同じ関所)" <|
                \_ ->
                    sketchChosen
                        |> step (NewGame.NameEdited "Abc")
                        |> step NewGame.CreateClicked
                        |> Tuple.second
                        |> Expect.equal NewGame.OutNone
            , test "依頼文 = 公式文 + ラフの節 + 催促とカメラの一文" <|
                \_ ->
                    let
                        prompt =
                            sketchPainted
                                |> Tuple.first
                                |> NewGame.buildSketchPrompt
                    in
                    ( String.startsWith "公式文のつもり" prompt
                    , String.contains "## 画面のラフ（カメラ: 見下ろし" prompt
                    , String.endsWith
                        (interviewAsk ++ "\nカメラ視点はこのラフの『見下ろし』で決まっています。視点の質問は飛ばしてください。")
                        prompt
                    )
                        |> Expect.equal ( True, True, True )
            , test "何も塗っていなければ公式文と催促だけ(節もカメラの一文も足さない)" <|
                \_ ->
                    sketchFilled
                        |> Tuple.first
                        |> NewGame.buildSketchPrompt
                        |> Expect.equal ("公式文のつもり\n\n" ++ interviewAsk)
            , test "公式文の「あなたの言葉」節(--- 行と世界・主人公・エッセンス行)は依頼文から消える" <|
                \_ ->
                    let
                        prompt =
                            sketchChosen
                                |> Tuple.mapFirst (NewGame.gotGenesisPrompt officialWithYourWords)
                                |> Tuple.first
                                |> NewGame.buildSketchPrompt
                    in
                    ( String.startsWith "土台: blank の骨組み\n守ること: 完了条件を満たす" prompt
                    , String.contains "あなたの言葉" prompt
                    , ( String.contains "世界:" prompt
                      , String.contains "主人公:" prompt
                      , String.contains "エッセンス:" prompt
                      )
                    )
                        |> Expect.equal ( True, False, ( False, False, False ) )
            , test "「あなたの言葉」節の無い公式文はそのまま(消す行が無くても壊れない)" <|
                \_ ->
                    sketchChosen
                        |> Tuple.mapFirst (NewGame.gotGenesisPrompt "節の無い公式文")
                        |> Tuple.first
                        |> NewGame.buildSketchPrompt
                        |> Expect.equal ("節の無い公式文\n\n" ++ interviewAsk)
            , test "「伝えたいこと」を書いて誕生すると「あなたから:」の節が入る(複数行もそのまま)" <|
                \_ ->
                    let
                        prompt =
                            sketchPainted
                                |> step (NewGame.SketchNoteEdited "夜の温泉街が舞台。\nちょうちんの光で照らしたい。")
                                |> Tuple.first
                                |> NewGame.buildSketchPrompt
                    in
                    String.contains "あなたから: 夜の温泉街が舞台。\nちょうちんの光で照らしたい。" prompt
                        |> Expect.equal True
            , test "「伝えたいこと」が空(空白だけ)なら「あなたから:」の節ごと無い" <|
                \_ ->
                    sketchPainted
                        |> step (NewGame.SketchNoteEdited "  \n  ")
                        |> Tuple.first
                        |> NewGame.buildSketchPrompt
                        |> String.contains "あなたから"
                        |> Expect.equal False
            , test "「あなたから:」の節はラフの節の後・催促の前に入る" <|
                \_ ->
                    sketchPainted
                        |> step (NewGame.SketchNoteEdited "夜の温泉街")
                        |> Tuple.first
                        |> NewGame.buildSketchPrompt
                        |> String.split "\n\n"
                        |> List.filter
                            (\block ->
                                String.startsWith "## 画面のラフ" block
                                    || String.startsWith "あなたから:" block
                                    || String.startsWith interviewAsk block
                            )
                        |> List.map (String.left 4)
                        |> Expect.equal [ "## 画", "あなたか", "実装に入" ]
            , test "誕生でパネルは畳まれず、組み立てた依頼文が差し出される" <|
                \_ ->
                    let
                        ( born, result ) =
                            sketchPainted
                                |> step NewGame.CreateClicked
                                |> Tuple.mapFirst (NewGame.accepted (Just "/games/rough_game"))
                                |> (\( m, _ ) -> NewGame.gotLog { running = False, exitCode = Just 0, lines = [] } m)
                    in
                    ( result
                    , NewGame.isPolling born
                    , NewGame.shownGenesisPrompt born
                        |> Maybe.map (String.contains "## 画面のラフ")
                    )
                        |> Expect.equal ( NewGame.LogSketchBorn, False, Just True )
            , test "「コピーして開く」は依頼文と、保存すべきラフの写し(screen)を運ぶ" <|
                \_ ->
                    let
                        out =
                            sketchPainted
                                |> step NewGame.CreateClicked
                                |> Tuple.mapFirst (NewGame.accepted (Just "/games/rough_game"))
                                |> (\( m, _ ) -> NewGame.gotLog { running = False, exitCode = Just 0, lines = [] } m)
                                |> (\( m, _ ) -> NewGame.update NewGame.OpenBornClicked m)
                                |> Tuple.second
                    in
                    case out of
                        NewGame.OutCopyPromptAndOpen info ->
                            ( info.dir
                            , info.sketchFile.path
                            , String.contains "## 画面のラフ" info.prompt
                            )
                                |> Expect.equal ( "/games/rough_game", "draft/sketch/screen/v1.json", True )

                        _ ->
                            Expect.fail "OutCopyPromptAndOpen が出ていません"
            ]
        , describe "つくり待ち・誕生後の札の行き来"
            [ test "つくっている最中の札替えは無視される(封筒も飛ばない)" <|
                \_ ->
                    let
                        ( model, out ) =
                            sketchPainted
                                |> step NewGame.CreateClicked
                                |> step (NewGame.FamilyChosen "action")
                    in
                    ( model.family, out )
                        |> Expect.equal ( Just NewGame.sketchFamilyId, NewGame.OutNone )
            , test "ラフ発の作成待ち中に他の札を押しても、誕生はラフの誕生画面になる" <|
                \_ ->
                    let
                        ( born, result ) =
                            sketchPainted
                                |> step NewGame.CreateClicked
                                |> Tuple.mapFirst (NewGame.accepted (Just "/games/rough_game"))
                                |> step (NewGame.FamilyChosen "action")
                                |> (\( m, _ ) -> NewGame.gotLog { running = False, exitCode = Just 0, lines = [] } m)
                    in
                    ( result, born.born /= Nothing )
                        |> Expect.equal ( NewGame.LogSketchBorn, True )
            , test "誕生後に他の札→ラフ札と戻っても、合成依頼文は残っている" <|
                \_ ->
                    let
                        back =
                            sketchBorn
                                |> (\m -> NewGame.gotFamilies genesisFamilies m)
                                |> (\m -> ( m, NewGame.OutNone ))
                                |> step (NewGame.FamilyChosen "rpg")
                                |> step (NewGame.FamilyChosen NewGame.sketchFamilyId)
                                |> Tuple.first
                    in
                    NewGame.shownGenesisPrompt back
                        |> Maybe.map (String.contains "## 画面のラフ")
                        |> Expect.equal (Just True)
            , test "誕生でなまえ・題名は空に戻る(同名でもう一度うんで 409 に落とさない)" <|
                \_ ->
                    ( sketchBorn.name, sketchBorn.title )
                        |> Expect.equal ( "", "" )
            , test "誕生後に遅れて届いた公式文は、合成依頼文を上書きしない" <|
                \_ ->
                    sketchBorn
                        |> NewGame.gotGenesisPrompt "遅れて届いた別の公式文"
                        |> NewGame.shownGenesisPrompt
                        |> Maybe.map (String.contains "## 画面のラフ")
                        |> Expect.equal (Just True)
            , test "「新しいラフでもう一度」で誕生状態が捨てられ、土台を取り直しに行く" <|
                \_ ->
                    let
                        ( model, out ) =
                            NewGame.update NewGame.SketchRestartClicked sketchBorn
                    in
                    ( model.born, out )
                        |> Expect.equal
                            ( Nothing
                            , NewGame.OutFetchGenesisPrompt { family = "blank", direction = "" }
                            )
            , test "「新しいラフでもう一度」で「伝えたいこと」も空に戻る" <|
                \_ ->
                    sketchPainted
                        |> step (NewGame.SketchNoteEdited "夜の温泉街")
                        |> step NewGame.SketchRestartClicked
                        |> Tuple.first
                        |> .sketchNote
                        |> Expect.equal ""
            , test "未塗り + 公式文の失敗で誕生すると、依頼文は空(コピーではなく取り直しの導線)" <|
                \_ ->
                    ( sketchBornEmpty.born, NewGame.shownGenesisPrompt sketchBornEmpty )
                        |> Expect.equal
                            ( Just { dir = Just "/games/rough_game", prompt = "" }
                            , Nothing
                            )
            , test "取り直しで blank を取り寄せ、届いたら合成し直して差し出す" <|
                \_ ->
                    let
                        ( retried, out ) =
                            NewGame.update NewGame.BornRetryClicked sketchBornEmpty

                        shown =
                            retried
                                |> NewGame.gotGenesisPrompt "公式文のつもり"
                                |> NewGame.shownGenesisPrompt
                    in
                    ( out, shown )
                        |> Expect.equal
                            ( NewGame.OutFetchGenesisPrompt { family = "blank", direction = "" }
                            , Just ("公式文のつもり\n\n" ++ interviewAsk)
                            )
            ]
        , describe "ラフ札の Main の配線"
            [ test "誕生(exitCode 0)でも自動では開かない — /projects の取り直しだけ" <|
                \_ ->
                    sketchBornMain
                        |> Tuple.second
                        |> kindsOf
                        |> Expect.equal [ "projects" ]
            , test "「コピーして開く」でコピーと選択フローが同時に飛ぶ" <|
                \_ ->
                    sketchBornMain
                        |> Tuple.first
                        |> Main.update (Main.NewGameMsg NewGame.OpenBornClicked)
                        |> Tuple.second
                        |> kindsOf
                        |> Expect.equal [ "copyClipboard", "selectProject" ]
            , test "開けたら 1 回だけ、ラフの写しが新プロジェクトへ書かれる" <|
                \_ ->
                    sketchBornMain
                        |> Tuple.first
                        |> updateM (Main.NewGameMsg NewGame.OpenBornClicked)
                        |> Main.update
                            (Main.GotApiResponse
                                (envelope "selectProject"
                                    True
                                    (E.object
                                        [ ( "ok", E.bool True )
                                        , ( "dir", E.string "/games/rough_game" )
                                        , ( "title", E.string "ラフのゲーム" )
                                        , ( "design", E.object [ ( "w", E.float 480 ), ( "h", E.float 300 ) ] )
                                        ]
                                    )
                                )
                            )
                        |> Tuple.second
                        |> kindsOf
                        |> Expect.equal [ "files", "resources", "putFile", "journeyState", "annotationsList", "sketchList" ]
            ]
        ]


{-| 依頼文の末尾に必ず入るインタビューの催促。 -}
interviewAsk : String
interviewAsk =
    "実装に入る前に、まず /style-interview を実行して画風を私に質問して決めてください。"


{-| 「あなたの言葉」節つきの公式文(実物の末尾の形をまねた見本)。 -}
officialWithYourWords : String
officialWithYourWords =
    String.join "\n"
        [ "土台: blank の骨組み"
        , "守ること: 完了条件を満たす"
        , ""
        , "--- ここから あなたの言葉(書き換えて詳細を) ---"
        , "世界: (例: 夜の温泉街)"
        , "主人公: (例: ちょうちんを持った子ども)"
        , "エッセンス: (ひとこと — 例: 夜だけの世界)"
        ]


{-| ラフ札を選んだ状態。 -}
sketchChosen : ( NewGame.Model, NewGame.Out )
sketchChosen =
    begin
        |> step (NewGame.FamilyChosen NewGame.sketchFamilyId)
        |> Tuple.mapFirst (NewGame.gotGenesisPrompt "公式文のつもり")


{-| ラフ札 + 名前・題名まで済ませた状態(塗りはまだ)。 -}
sketchFilled : ( NewGame.Model, NewGame.Out )
sketchFilled =
    sketchChosen
        |> step (NewGame.NameEdited "rough_game")
        |> step (NewGame.TitleEdited "ラフのゲーム")


{-| さらに 1 マス塗った状態(map プリセットの先頭ラベルで)。 -}
sketchPainted : ( NewGame.Model, NewGame.Out )
sketchPainted =
    sketchFilled
        |> step (NewGame.SketchMsg (SketchPad.CellDown ( 0, 0 )))
        |> step (NewGame.SketchMsg SketchPad.StrokeEnded)


{-| ラフ札で誕生(exitCode 0)まで進めた状態(合成依頼文あり)。 -}
sketchBorn : NewGame.Model
sketchBorn =
    sketchPainted
        |> step NewGame.CreateClicked
        |> Tuple.mapFirst (NewGame.accepted (Just "/games/rough_game"))
        |> (\( m, _ ) -> NewGame.gotLog { running = False, exitCode = Just 0, lines = [] } m)
        |> Tuple.first


{-| 何も塗らず、公式文も失敗したまま誕生した状態(依頼文が空)。 -}
sketchBornEmpty : NewGame.Model
sketchBornEmpty =
    begin
        |> step (NewGame.FamilyChosen NewGame.sketchFamilyId)
        |> Tuple.mapFirst (NewGame.genesisPromptFailed "HTTP 500: /prompt/genesis — こわれた")
        |> step (NewGame.NameEdited "rough_game")
        |> step NewGame.CreateClicked
        |> Tuple.mapFirst (NewGame.accepted (Just "/games/rough_game"))
        |> (\( m, _ ) -> NewGame.gotLog { running = False, exitCode = Just 0, lines = [] } m)
        |> Tuple.first


{-| Main 側でラフ札の誕生(exitCode 0)まで進めた状態。 -}
sketchBornMain : ( Main.Model, Effect )
sketchBornMain =
    pickerModel
        |> updateM (Main.NewGameMsg (NewGame.FamilyChosen NewGame.sketchFamilyId))
        |> updateM
            (Main.GotApiResponse
                (envelope "promptGenesis" True (E.object [ ( "prompt", E.string "公式文のつもり" ) ]))
            )
        |> updateM (Main.NewGameMsg (NewGame.NameEdited "rough_game"))
        |> updateM (Main.NewGameMsg (NewGame.SketchMsg (SketchPad.CellDown ( 0, 0 ))))
        |> updateM (Main.NewGameMsg (NewGame.SketchMsg SketchPad.StrokeEnded))
        |> updateM (Main.NewGameMsg NewGame.CreateClicked)
        |> updateM
            (Main.GotApiResponse
                (envelope "projectNew"
                    True
                    (E.object [ ( "ok", E.bool True ), ( "dir", E.string "/games/rough_game" ) ])
                )
            )
        |> Main.update
            (Main.GotApiResponse
                (envelope "projectNewLog"
                    True
                    (E.object
                        [ ( "running", E.bool False )
                        , ( "exitCode", E.int 0 )
                        , ( "lines", E.list E.string [] )
                        ]
                    )
                )
            )


{-| ジャンルの札のサンプル(starter 有り / 無し / free)。 -}
genesisFamilies : List NewGame.Family
genesisFamilies =
    [ { id = "action", name = "アクション", verb = "走って、跳んで、壊す", includes = "ブロック崩し", controls = "←→ + スペース", starter = "templates/game-starter" }
    , { id = "rpg", name = "RPG", verb = "歩いて、話して、強くなる", includes = "剣と探索", controls = "←→↑↓ + 話す", starter = "" }
    , { id = "free", name = "フリージャンル", verb = "言葉から始める", includes = "ここにない、全部", controls = "あなたが決める", starter = "" }
    ]
