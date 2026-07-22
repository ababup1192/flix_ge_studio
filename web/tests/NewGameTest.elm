module NewGameTest exposing (suite)

{-| 「＋ 新しいゲームをはじめる」(まっさらから)の規則のテスト。

守るのは規則だけ: 名前の検証ゲート、ポーリングの始まりと止まり、
成功で /projects を取り直して選択フローへ乗ること、404 の fail-open(準備中)。
チップの見た目やパネルの文言の演出は焼かない。

-}

import Atelier
import Effect exposing (Effect)
import Expect
import Json.Decode as D
import Json.Encode as E
import Main
import NewGame
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
            , test "scaffold の成功で /atelier/slots を取り直す(refetch intent)" <|
                \_ ->
                    pickerModel
                        |> Main.update
                            (Main.GotApiResponse
                                (envelope "scaffoldDoc"
                                    True
                                    (E.object
                                        [ ( "ok", E.bool True )
                                        , ( "files", E.list E.string [ "assets/enemy.json" ] )
                                        , ( "wirePrompt", E.string "配線して" )
                                        ]
                                    )
                                )
                            )
                        |> Tuple.mapSecond kindsOf
                        |> Tuple.mapFirst (\m -> Atelier.shownWirePrompt m.atelier)
                        |> Expect.equal ( Just "配線して", [ "atelierSlots" ] )
            ]
        , describe "/projects/new/log の橋渡し"
            [ test "欠けたフィールドは既定に倒す(fail-open)" <|
                \_ ->
                    D.decodeString NewGame.logDecoder "{}"
                        |> Expect.equal
                            (Ok { running = False, exitCode = Nothing, lines = [] })
            ]
        , describe "家族えらび(genesis)"
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
            , test "starter 無しの家族を選ぶと、その場で公式プロンプトを取りに行く" <|
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
        ]


{-| 家族の札のサンプル(starter 有り / 無し / free)。 -}
genesisFamilies : List NewGame.Family
genesisFamilies =
    [ { id = "action", name = "アクション", verb = "走って、跳んで、壊す", includes = "ブロック崩し", controls = "←→ + スペース", starter = "templates/game-starter" }
    , { id = "rpg", name = "RPG", verb = "歩いて、話して、強くなる", includes = "剣と探索", controls = "←→↑↓ + 話す", starter = "" }
    , { id = "free", name = "フリージャンル", verb = "言葉から始める", includes = "ここにない、全部", controls = "あなたが決める", starter = "" }
    ]
