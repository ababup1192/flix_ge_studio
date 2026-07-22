module NewGame exposing
    ( Family
    , LogResult(..)
    , Model
    , Msg(..)
    , Out(..)
    , Phase(..)
    , accepted
    , createFailed
    , familiesDecoder
    , familiesUnavailable
    , genesisPromptFailed
    , gotFamilies
    , gotGenesisPrompt
    , gotLog
    , init
    , isPolling
    , logDecoder
    , nameError
    , selectedFamily
    , shownError
    , shownGenesisPrompt
    , unavailable
    , update
    , view
    )

{-| 「＋ 新しいゲームをはじめる」— プロジェクトピッカーからのまっさら開始。

まっさらな画面は作らない: 入口は GET /genesis/families の 9 ジャンル(並びは
人気順 — サーバの順のまま表示する)。starter 持ちのジャンルは既存の作成フロー
(名前入力 → POST /projects/new)へ、starter 無しは GET /prompt/genesis の
公式プロンプト(編集可)を差し出す。フリージャンルだけは「どんなゲーム?」の
言葉(direction)が必須。

スクロール鉄則: ジャンルカードは内側スクロール、確認バー(次の一歩)は
選ぶ前から見える固定位置に常設する。

サーバ往復は持たない(封筒は Main が発行し、応答をここへ流し込む)。
送りたい事は update の戻り値 Out で Main へ返す(Atelier / Journey と同じ流儀)。

POST /projects/new(202)→ GET /projects/new/log を 2 秒毎に回す
(ゲーム起動と同じ進捗ミニパネルの作法)。exitCode 0 で誕生 —
Main が /projects を取り直し、できたプロジェクトを既存の選択フローで開く。
失敗の時だけはログを隠さない(自動で全文展開)。

エンドポイント未実装のサーバ(404)は「準備中」に倒す(fail-open —
ピッカーの既存機能はそのまま生きる)。/genesis/families が無い旧サーバでは
従来のプリセット(w/h)入力に倒す。

-}

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Progress


{-| つくりの進み。Creating の間だけ Main がログのポーリングを回す。 -}
type Phase
    = Idle
    | Creating { lines : List String }
    | Failed { lines : List String }


{-| ジャンル 1 枚。starter が非空 = 公式テンプレートつき(複製でそのまま生まれる)。 -}
type alias Family =
    { id : String
    , name : String
    , verb : String
    , includes : String
    , controls : String
    , starter : String
    }


{-| ジャンル一覧の取り寄せの進み。Unavailable(旧サーバの 404 等)は従来の
プリセット入力に倒す(fail-open — 作れなくはならない)。
-}
type Families
    = FamiliesNotAsked
    | FamiliesLoading
    | FamiliesReady (List Family)
    | FamiliesUnavailable


{-| 公式プロンプト(GET /prompt/genesis)の進み。Ready の中身は編集可 —
「あなたの言葉」欄を書き換えてから渡す前提の下書き。
-}
type GenesisPrompt
    = GenesisIdle
    | GenesisLoading
    | GenesisReady String
    | GenesisFailed String


type alias Model =
    { open : Bool
    , name : String
    , title : String

    -- 画面サイズは入力欄の文字列が正(チップは書き込むだけ)。送る時に数へ
    , w : String
    , h : String
    , phase : Phase

    -- 202 応答が教えてくれる「産まれるゲームの絶対パス」。誕生時の自動選択は
    -- これを使う(ログの target は make のターゲット名で、dir ではない)。
    -- 旧サーバは dir を返さない = Nothing(/projects 再取得だけに倒す fail-open)
    , dir : Maybe String

    -- 進捗ミニパネルの全文展開。既定は畳み、失敗時だけ自動で開く
    , logExpanded : Bool

    -- 送信前検証・サーバ 400/409 の理由(日本語)をボタンの近くに出す
    , error : Maybe String

    -- ジャンルえらび(GET /genesis/families)。初回に開いた時だけ取り寄せる
    , families : Families
    , family : Maybe String

    -- フリージャンルの「どんなゲーム?」(必須)
    , freeDirection : String

    -- starter 無しのジャンルに差し出す公式プロンプト(編集可)
    , genesisPrompt : GenesisPrompt
    , genesisCopied : Bool
    }


init : Model
init =
    { open = False
    , name = ""
    , title = ""
    , w = "480"
    , h = "300"
    , phase = Idle
    , dir = Nothing
    , logExpanded = False
    , error = Nothing
    , families = FamiliesNotAsked
    , family = Nothing
    , freeDirection = ""
    , genesisPrompt = GenesisIdle
    , genesisCopied = False
    }


type Msg
    = Toggled
    | NameEdited String
    | TitleEdited String
    | PresetChosen Int Int
    | WidthEdited String
    | HeightEdited String
    | CreateClicked
    | LogToggled
    | FamilyChosen String
    | FreeDirectionEdited String
    | FreePromptRequested
    | GenesisPromptEdited String
    | CopyGenesisPromptClicked


{-| サーバへ送りたい事(封筒の発行は Main)。 -}
type Out
    = OutNone
    | OutCreate { name : String, title : String, w : Int, h : Int }
    | OutFetchFamilies
    | OutFetchGenesisPrompt { family : String, direction : String }
    | OutCopyPrompt String


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        Toggled ->
            if not model.open && model.families == FamiliesNotAsked then
                -- 初めて開いた時だけジャンル一覧を取り寄せる(404 はプリセットに倒す)
                ( { model | open = True, families = FamiliesLoading }, OutFetchFamilies )

            else
                ( { model | open = not model.open }, OutNone )

        FamilyChosen id ->
            let
                m1 =
                    { model | family = Just id, error = Nothing, genesisCopied = False }
            in
            case selectedFamily { m1 | family = Just id } of
                Just family ->
                    if family.starter /= "" || family.id == "free" then
                        -- テンプレートつきは名前入力へ、free は言葉(direction)を待つ
                        ( { m1 | genesisPrompt = GenesisIdle }, OutNone )

                    else
                        -- starter 無し: 選んだ瞬間に公式プロンプトを取りに行く
                        ( { m1 | genesisPrompt = GenesisLoading }
                        , OutFetchGenesisPrompt { family = id, direction = "" }
                        )

                Nothing ->
                    ( m1, OutNone )

        FreeDirectionEdited text_ ->
            ( { model | freeDirection = text_ }, OutNone )

        FreePromptRequested ->
            if model.genesisPrompt == GenesisLoading then
                -- 飛行中の二度押しは送らない
                ( model, OutNone )

            else
                case String.trim model.freeDirection of
                    "" ->
                        -- 空はサーバへ行かない(ボタンも無効だが、防御は二重に)
                        ( model, OutNone )

                    direction ->
                        ( { model | genesisPrompt = GenesisLoading, genesisCopied = False }
                        , OutFetchGenesisPrompt { family = "free", direction = direction }
                        )

        GenesisPromptEdited text_ ->
            case model.genesisPrompt of
                GenesisReady _ ->
                    ( { model | genesisPrompt = GenesisReady text_ }, OutNone )

                _ ->
                    ( model, OutNone )

        CopyGenesisPromptClicked ->
            case model.genesisPrompt of
                GenesisReady prompt ->
                    ( { model | genesisCopied = True }, OutCopyPrompt prompt )

                _ ->
                    ( model, OutNone )

        NameEdited text_ ->
            -- 打ち直しで古いサーバの理由は畳む(生きた検証は nameError が担う)
            ( { model | name = text_, error = Nothing }, OutNone )

        TitleEdited text_ ->
            ( { model | title = text_ }, OutNone )

        PresetChosen w h ->
            ( { model | w = String.fromInt w, h = String.fromInt h }, OutNone )

        WidthEdited text_ ->
            ( { model | w = text_ }, OutNone )

        HeightEdited text_ ->
            ( { model | h = text_ }, OutNone )

        CreateClicked ->
            case model.phase of
                Creating _ ->
                    -- 飛行中の二度押しは送らない
                    ( model, OutNone )

                _ ->
                    submit model

        LogToggled ->
            ( { model | logExpanded = not model.logExpanded }, OutNone )


{-| 送信前の関所。名前・サイズが規則に合わない限り封筒は飛ばない。 -}
submit : Model -> ( Model, Out )
submit model =
    let
        name =
            String.trim model.name

        size =
            Maybe.map2 Tuple.pair (String.toInt model.w) (String.toInt model.h)
                |> Maybe.andThen
                    (\( w, h ) ->
                        if w > 0 && h > 0 then
                            Just ( w, h )

                        else
                            Nothing
                    )
    in
    if name == "" then
        ( { model | error = Just "なまえを入れてください(半角の小文字。例: block_breaker2)" }, OutNone )

    else
        case nameError name of
            Just reason ->
                ( { model | error = Just reason }, OutNone )

            Nothing ->
                case size of
                    Nothing ->
                        ( { model | error = Just "画面サイズは正の数で入れてください" }, OutNone )

                    Just ( w, h ) ->
                        ( { model | phase = Creating { lines = [] }, dir = Nothing, logExpanded = False, error = Nothing }
                        , OutCreate
                            { name = name
                            , title =
                                case String.trim model.title of
                                    "" ->
                                        -- 題名が空なら名前で代用(空の題は寂しい)
                                        name

                                    title ->
                                        title
                            , w = w
                            , h = h
                            }
                        )


{-| なまえの生きた検証(^[a-z][a-z0-9_]*$)。空はまだ何も言わない
(送る時に「入れてください」が出る)。
-}
nameError : String -> Maybe String
nameError name =
    if name == "" || isValidName name then
        Nothing

    else
        Just "半角の小文字で始め、a-z 0-9 _ だけが使えます(例: block_breaker2)"


isValidName : String -> Bool
isValidName name =
    case String.uncons name of
        Just ( c, rest ) ->
            Char.isLower c && String.all (\x -> Char.isLower x || Char.isDigit x || x == '_') rest

        Nothing ->
            False


{-| いま画面に出ている失敗の理由(テストの覗き窓)。 -}
shownError : Model -> Maybe String
shownError model =
    model.error


{-| POST /projects/new の失敗(400/409)。日本語の理由をその場に出し、ボタンを戻す。 -}
createFailed : String -> Model -> Model
createFailed message model =
    { model | phase = Idle, error = Just (cleanReason message) }


{-| エンドポイント未実装のサーバ(404 等)。「準備中」に倒すだけ(fail-open)。 -}
unavailable : Model -> Model
unavailable model =
    { model | phase = Idle, error = Just "この機能はまだ準備中です(サーバが古い可能性があります)" }


{-| GET /genesis/families 成功。並びはサーバの順のまま(人気順が仕様)。 -}
gotFamilies : List Family -> Model -> Model
gotFamilies families model =
    { model | families = FamiliesReady families }


{-| /genesis/families が無い・読めない(旧サーバ)。従来のプリセット入力に
倒す(fail-open — 新しいゲームは作れなくならない)。
-}
familiesUnavailable : Model -> Model
familiesUnavailable model =
    { model | families = FamiliesUnavailable }


{-| GET /prompt/genesis 成功。届いた下書きは編集可になる。 -}
gotGenesisPrompt : String -> Model -> Model
gotGenesisPrompt prompt model =
    { model | genesisPrompt = GenesisReady prompt, genesisCopied = False }


{-| GET /prompt/genesis の失敗。理由だけを箱の場所に出す。 -}
genesisPromptFailed : String -> Model -> Model
genesisPromptFailed message model =
    { model | genesisPrompt = GenesisFailed (cleanReason message) }


{-| いま画面の下書きに映っている公式プロンプト(テストの覗き窓)。 -}
shownGenesisPrompt : Model -> Maybe String
shownGenesisPrompt model =
    case model.genesisPrompt of
        GenesisReady prompt ->
            Just prompt

        _ ->
            Nothing


{-| いま選んでいるジャンルの札。 -}
selectedFamily : Model -> Maybe Family
selectedFamily model =
    case ( model.families, model.family ) of
        ( FamiliesReady families, Just id ) ->
            List.head (List.filter (\f -> f.id == id) families)

        _ ->
            Nothing


{-| GET /genesis/families。id 以外の欠けは空文字に倒す(fail-open)。 -}
familiesDecoder : D.Decoder (List Family)
familiesDecoder =
    D.field "families" (D.list familyDecoder)


familyDecoder : D.Decoder Family
familyDecoder =
    D.map6 Family
        (D.field "id" D.string)
        (stringOr "name")
        (stringOr "verb")
        (stringOr "includes")
        (stringOr "controls")
        (stringOr "starter")


stringOr : String -> D.Decoder String
stringOr name =
    D.oneOf [ D.field name D.string, D.succeed "" ]


{-| JS 橋の "Error: HTTP 400: … — 理由" から理由だけを取り出す。 -}
cleanReason : String -> String
cleanReason message =
    case String.split " — " message of
        _ :: rest ->
            if List.isEmpty rest then
                message

            else
                String.join " — " rest

        [] ->
            message


{-| POST /projects/new の 202 応答。dir(産まれるゲームの絶対パス)を覚える。
旧サーバは dir を返さない = Nothing のまま(誕生時は /projects 再取得だけ)。
-}
accepted : Maybe String -> Model -> Model
accepted dir model =
    { model | dir = dir }


{-| GET /projects/new/log。running の間だけ Main がポーリングを回す。 -}
isPolling : Model -> Bool
isPolling model =
    case model.phase of
        Creating _ ->
            True

        _ ->
            False


type alias Log =
    { running : Bool
    , exitCode : Maybe Int
    , lines : List String
    }


{-| ログの流し込み。つくり待ちの間だけ意味を持つ(古い応答は無視)。
exitCode 0 = 誕生(Main が /projects を取り直し、202 で覚えた dir を開く。
dir 不明の旧サーバは Nothing — 取り直しだけに倒す)。
失敗の時だけはログを隠さない(自動で全文展開)。
-}
type LogResult
    = LogContinue
    | LogSuccess { dir : Maybe String }
    | LogFailure


gotLog : Log -> Model -> ( Model, LogResult )
gotLog log model =
    case model.phase of
        Creating _ ->
            if log.running then
                ( { model | phase = Creating { lines = log.lines } }, LogContinue )

            else
                case log.exitCode of
                    Just 0 ->
                        ( { model | phase = Idle, open = False, name = "", title = "" }
                        , LogSuccess { dir = model.dir }
                        )

                    Just _ ->
                        ( { model | phase = Failed { lines = log.lines }, logExpanded = True }
                        , LogFailure
                        )

                    Nothing ->
                        -- まだ始まっていない(ログ口が空)。回し続ける
                        ( model, LogContinue )

        _ ->
            ( model, LogContinue )


{-| GET /projects/new/log。欠けは既定値に倒す(fail-open)。 -}
logDecoder : D.Decoder Log
logDecoder =
    D.map3 Log
        (D.oneOf [ D.field "running" D.bool, D.succeed False ])
        (D.oneOf [ D.field "exitCode" (D.map Just D.int), D.succeed Nothing ])
        (D.oneOf [ D.field "lines" (D.list D.string), D.succeed [] ])



-- 画面


{-| ジャンルの相場観をチップの説明で与える(よこ長=村、たて長=崩し等)。 -}
presets : List { w : Int, h : Int, label : String, genre : String }
presets =
    [ { w = 480, h = 300, label = "よこ長 480×300", genre = "村・見下ろし向き" }
    , { w = 240, h = 320, label = "たて長 240×320", genre = "ブロック崩し・シューティング向き" }
    , { w = 320, h = 320, label = "ましかく 320×320", genre = "" }
    ]


view : Model -> Html Msg
view model =
    div [ HA.class "newgame mb-5 rounded-lg border border-edge bg-panel" ]
        (button
            [ HA.class "newgame-bar flex w-full cursor-pointer items-center gap-2 rounded-lg px-4 py-2.5 text-left text-sm font-semibold text-ink transition-colors hover:bg-white/5"
            , HE.onClick Toggled
            ]
            [ span [] [ text "＋ 新しいゲームをはじめる" ]
            , span [ HA.class "flex-1" ] []
            , span [ HA.class "rounded border border-edge px-2 py-0.5 text-[12px] font-normal text-ink-soft" ]
                [ text
                    (if model.open then
                        "▾ とじる"

                     else
                        "▸ ひらく"
                    )
                ]
            ]
            :: (if model.open then
                    [ div [ HA.class "px-4 pb-4" ] (viewBody model) ]

                else
                    []
               )
        )


{-| 中身の出し分け。ジャンル一覧が読めたらジャンルえらび、旧サーバ(404)は
従来のプリセット入力に倒す(fail-open)。
-}
viewBody : Model -> List (Html Msg)
viewBody model =
    case model.families of
        FamiliesReady families ->
            viewGenesis families model

        FamiliesUnavailable ->
            viewForm model

        _ ->
            [ div [ HA.class "newgame-families-loading mt-2 text-[11px] text-ink-soft" ]
                [ text "⏳ ジャンルの一覧を取り寄せています…" ]
            ]



-- ジャンルえらび(genesis)


viewGenesis : List Family -> Model -> List (Html Msg)
viewGenesis families model =
    [ div [ HA.class "mt-1 text-[11px] leading-relaxed text-ink-soft" ]
        [ text "まっさらな画面はありません。動くテンプレートから選ぶ — サイズも操作もテンプレートが決めてくれます(変えたい人は project.json を直接)。" ]
    , div [ HA.class "mt-0.5 text-[10px] text-ink-faint" ]
        [ text "並びは、昔の家庭用ゲーム機で人気だった順。" ]

    -- カード一覧は内側スクロール。確認バー(次の一歩)は下に常設 —
    -- 選ぶ前から、選んだ後に何が起きる場所かが見えている
    , div [ HA.class "newgame-families mt-2 grid max-h-[38vh] grid-cols-2 gap-1.5 overflow-y-auto pr-1 md:grid-cols-3" ]
        (List.map (viewFamilyCard model.family) families)
    , div [ HA.class "newgame-confirm mt-3 border-t border-edge pt-3" ]
        (case selectedFamily model of
            Nothing ->
                [ div [ HA.class "text-[11px] text-ink-faint" ]
                    [ text "ジャンルをひとつ選ぶと、ここに次の一歩が出ます" ]
                , div [ HA.class "mt-2" ]
                    [ button [ HA.class "btn btn-primary", HA.disabled True ]
                        [ text "テンプレートではじめる →" ]
                    ]
                ]

            Just family ->
                viewConfirm family model
        )
    ]


viewFamilyCard : Maybe String -> Family -> Html Msg
viewFamilyCard chosen family =
    button
        [ HA.classList
            [ ( "newgame-family cursor-pointer rounded-lg border p-2 text-left transition-colors hover:bg-white/5", True )
            , ( "border-accent/70 ring-1 ring-accent/40", chosen == Just family.id )
            , ( "border-edge", chosen /= Just family.id )
            ]
        , HE.onClick (FamilyChosen family.id)
        ]
        [ div [ HA.class "text-xs font-semibold text-ink" ] [ text family.name ]
        , div [ HA.class "mt-0.5 text-[11px] text-ink-soft" ] [ text family.verb ]
        , div [ HA.class "mt-1 text-[10px] leading-relaxed text-ink-faint" ] [ text ("含む: " ++ family.includes) ]
        , div [ HA.class "mt-0.5 font-mono text-[10px] text-ink-faint" ] [ text ("操作: " ++ family.controls) ]
        ]


{-| 確認バーの中身(選んだジャンルの次の一歩)。 -}
viewConfirm : Family -> Model -> List (Html Msg)
viewConfirm family model =
    if family.starter /= "" then
        -- 公式テンプレートつき: 名前を決めて、テンプレートの複製で生まれる(既存フロー)
        div [ HA.class "text-[11px] leading-relaxed text-ink-soft" ]
            [ span [ HA.class "font-semibold text-ink" ] [ text family.name ]
            , text
                (if family.id == "action" then
                    " — テンプレートは、いちばん小さいアクション「ブロック崩し」から。名前を決めるだけで生まれます。"

                 else
                    " — 公式テンプレートつき。名前を決めるだけで生まれます。"
                )
            ]
            :: viewStarterForm model

    else if family.id == "free" then
        viewFreeConfirm family model

    else
        viewPromptConfirm family model


{-| テンプレートつきジャンルの作成フォーム。サイズはテンプレートが決めるので聞かない
(変えたい人は project.json を直接)。
-}
viewStarterForm : Model -> List (Html Msg)
viewStarterForm model =
    [ div [ HA.class "mt-2 text-[11px] text-ink-soft" ] [ text "なまえ(フォルダ名になります)" ]
    , input
        [ HA.class "field mt-1 w-full font-mono text-xs"
        , HA.placeholder "block_breaker2"
        , HA.value model.name
        , HE.onInput NameEdited
        ]
        []
    , case nameError model.name of
        Just reason ->
            div [ HA.class "newgame-name-error mt-1 text-[11px] text-danger" ] [ text reason ]

        Nothing ->
            text ""
    , div [ HA.class "mt-2 text-[11px] text-ink-soft" ] [ text "題名" ]
    , input
        [ HA.class "field mt-1 w-full text-xs"
        , HA.placeholder "くずしブロック2"
        , HA.value model.title
        , HE.onInput TitleEdited
        ]
        []
    , div [ HA.class "mt-3" ]
        [ button
            [ HA.class "btn btn-primary"
            , HA.disabled (isPolling model)
            , HE.onClick CreateClicked
            ]
            [ text
                (if isPolling model then
                    "⏳ つくっています…"

                 else
                    "このテンプレートではじめる →"
                )
            ]
        ]
    , viewCreateError model
    ]
        ++ viewProgress model


{-| starter 無しのジャンル: 公式プロンプト(編集可)を差し出す。 -}
viewPromptConfirm : Family -> Model -> List (Html Msg)
viewPromptConfirm family model =
    div [ HA.class "text-[11px] leading-relaxed text-ink-soft" ]
        [ span [ HA.class "font-semibold text-ink" ] [ text family.name ]
        , text " — このジャンルのテンプレートは公式プロンプトから生まれます。骨格と合格条件は焼き込み済み — 下の「あなたの言葉」を書き換えて、詳細を詰めてから渡して構いません。"
        ]
        :: viewGenesisPrompt model
        ++ [ div [ HA.class "mt-2 text-[10px] leading-relaxed text-ink-faint" ]
                [ text "良いテンプレートが生まれたら templates/ に昇格 — 次からこのジャンルは公式テンプレートつきになります。" ]
           ]


{-| フリージャンル: 「どんなゲーム?」の言葉が必須。 -}
viewFreeConfirm : Family -> Model -> List (Html Msg)
viewFreeConfirm family model =
    [ div [ HA.class "text-[11px] leading-relaxed text-ink-soft" ]
        [ span [ HA.class "font-semibold text-ink" ] [ text family.name ]
        , text " — ジャンルに当てはまらないときは、言葉で。AI はいちばん近いジャンルのテンプレートを土台に変形します(ゼロからは作りません)。"
        ]
    , div [ HA.class "mt-2 text-[11px] text-ink-soft" ] [ text "どんなゲーム?" ]
    , Html.textarea
        [ HA.class "field mt-1 h-auto min-h-[3.5rem] w-full resize-y py-1.5 text-xs leading-relaxed"
        , HA.rows 2
        , HA.placeholder "例: 猫が屋根を跳びわたって、街で魚を集めるゲーム"
        , HA.value model.freeDirection
        , HE.onInput FreeDirectionEdited
        ]
        []
    , div [ HA.class "mt-2" ]
        [ button
            [ HA.class "btn btn-primary"
            , HA.disabled
                (String.trim model.freeDirection
                    == ""
                    || model.genesisPrompt
                    == GenesisLoading
                )
            , HE.onClick FreePromptRequested
            ]
            [ text
                (if model.genesisPrompt == GenesisLoading then
                    "⏳ 作っています…"

                 else
                    "依頼文を作る"
                )
            ]
        ]
    ]
        ++ viewGenesisPrompt model


{-| 公式プロンプトの箱。届いたら編集可の textarea + コピー。 -}
viewGenesisPrompt : Model -> List (Html Msg)
viewGenesisPrompt model =
    case model.genesisPrompt of
        GenesisIdle ->
            []

        GenesisLoading ->
            [ div [ HA.class "newgame-genesis-loading mt-2 text-[11px] text-ink-soft" ]
                [ text "⏳ 公式プロンプトを作っています…" ]
            ]

        GenesisFailed reason ->
            [ div [ HA.class "newgame-genesis-error mt-2 text-[11px] text-danger" ]
                [ text ("依頼文を作れませんでした — " ++ reason) ]
            ]

        GenesisReady prompt ->
            [ Html.textarea
                [ HA.class "newgame-genesis-prompt mt-2 h-44 max-h-44 w-full resize-none overflow-y-auto rounded border border-edge bg-black/40 p-2 font-mono text-[11px] leading-relaxed text-ink"
                , HA.value prompt
                , HE.onInput GenesisPromptEdited
                ]
                []
            , div [ HA.class "mt-2" ]
                [ button [ HA.class "btn btn-primary w-full", HE.onClick CopyGenesisPromptClicked ]
                    [ text
                        (if model.genesisCopied then
                            "✓ コピーしました"

                         else
                            "📋 コピー"
                        )
                    ]
                ]
            , div [ HA.class "mt-1 text-[10px] leading-relaxed text-ink-faint" ]
                [ text "Claude Code などの AI に貼ると、テンプレートづくりが始まります。" ]
            ]


viewCreateError : Model -> Html Msg
viewCreateError model =
    case model.error of
        Just reason ->
            div [ HA.class "newgame-error mt-2 text-[11px] text-danger" ]
                [ text ("作れませんでした — " ++ reason) ]

        Nothing ->
            text ""



-- 従来のプリセット入力(旧サーバへの fail-open)


viewForm : Model -> List (Html Msg)
viewForm model =
    [ div [ HA.class "mt-1 text-[11px] text-ink-soft" ] [ text "なまえ(フォルダ名になります)" ]
    , input
        [ HA.class "field mt-1 w-full font-mono text-xs"
        , HA.placeholder "block_breaker2"
        , HA.value model.name
        , HE.onInput NameEdited
        ]
        []
    , case nameError model.name of
        Just reason ->
            div [ HA.class "newgame-name-error mt-1 text-[11px] text-danger" ] [ text reason ]

        Nothing ->
            text ""
    , div [ HA.class "mt-3 text-[11px] text-ink-soft" ] [ text "題名" ]
    , input
        [ HA.class "field mt-1 w-full text-xs"
        , HA.placeholder "くずしブロック2"
        , HA.value model.title
        , HE.onInput TitleEdited
        ]
        []
    , div [ HA.class "mt-3 text-[11px] text-ink-soft" ] [ text "画面サイズ" ]
    , div [ HA.class "mt-1 flex flex-wrap gap-2" ] (List.map (viewPreset model) presets)
    , div [ HA.class "mt-2 flex items-center gap-2" ]
        [ span [ HA.class "text-[11px] text-ink-soft" ] [ text "よこ" ]
        , input
            [ HA.class "field w-20 font-mono text-xs"
            , HA.type_ "number"
            , HA.value model.w
            , HE.onInput WidthEdited
            ]
            []
        , span [ HA.class "text-[11px] text-ink-soft" ] [ text "たて" ]
        , input
            [ HA.class "field w-20 font-mono text-xs"
            , HA.type_ "number"
            , HA.value model.h
            , HE.onInput HeightEdited
            ]
            []
        ]
    , div [ HA.class "mt-3" ]
        [ button
            [ HA.class "btn btn-primary"
            , HA.disabled (isPolling model)
            , HE.onClick CreateClicked
            ]
            [ text
                (if isPolling model then
                    "つくっています…"

                 else
                    "つくる"
                )
            ]
        ]
    , case model.error of
        Just reason ->
            div [ HA.class "newgame-error mt-2 text-[11px] text-danger" ]
                [ text ("作れませんでした — " ++ reason) ]

        Nothing ->
            text ""
    ]
        ++ viewProgress model


viewPreset : Model -> { w : Int, h : Int, label : String, genre : String } -> Html Msg
viewPreset model preset =
    let
        active =
            model.w == String.fromInt preset.w && model.h == String.fromInt preset.h
    in
    button
        [ HA.classList
            [ ( "btn btn-mini", True )
            , ( "btn-primary", active )
            ]
        , HE.onClick (PresetChosen preset.w preset.h)
        ]
        [ text
            (if preset.genre == "" then
                preset.label

             else
                preset.label ++ "(" ++ preset.genre ++ ")"
            )
        ]


{-| つくりの進み(進捗ミニパネル — ゲーム起動と同じ形)。待たせる理由を
一言で固定し、ログは末尾だけ(⤢ で全文)。失敗の時だけは自動で全文を開く。
-}
viewProgress : Model -> List (Html Msg)
viewProgress model =
    case model.phase of
        Idle ->
            []

        Creating info ->
            [ div [ HA.class "newgame-progress mt-3" ]
                [ Progress.view
                    { message = "ひな形を写して、最初の絵を描き出しています… 1〜2分かかります"
                    , lines = info.lines
                    , failed = False
                    , expanded = model.logExpanded
                    , onToggle = LogToggled
                    }
                ]
            ]

        Failed info ->
            [ div [ HA.class "newgame-progress mt-3" ]
                [ Progress.view
                    { message = "作れませんでした。ログを確認してください"
                    , lines = info.lines
                    , failed = True
                    , expanded = model.logExpanded
                    , onToggle = LogToggled
                    }
                ]
            ]
