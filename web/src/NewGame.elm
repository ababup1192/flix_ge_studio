module NewGame exposing
    ( LogResult(..)
    , Model
    , Msg(..)
    , Out(..)
    , Phase(..)
    , accepted
    , createFailed
    , gotLog
    , init
    , isPolling
    , logDecoder
    , nameError
    , shownError
    , unavailable
    , update
    , view
    )

{-| 「＋ 新しいゲームをはじめる」— プロジェクトピッカーからのまっさら開始。

サーバ往復は持たない(封筒は Main が発行し、応答をここへ流し込む)。
送りたい事は update の戻り値 Out で Main へ返す(Atelier / Gallery と同じ流儀)。

POST /projects/new(202)→ GET /projects/new/log を 2 秒毎に回す
(ゲーム起動と同じ進捗ミニパネルの作法)。exitCode 0 で誕生 —
Main が /projects を取り直し、できたプロジェクトを既存の選択フローで開く。
失敗の時だけはログを隠さない(自動で全文展開)。

エンドポイント未実装のサーバ(404)は「準備中」に倒す(fail-open —
ピッカーの既存機能はそのまま生きる)。

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


{-| サーバへ送りたい事(封筒の発行は Main)。 -}
type Out
    = OutNone
    | OutCreate { name : String, title : String, w : Int, h : Int }


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        Toggled ->
            ( { model | open = not model.open }, OutNone )

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
                    [ div [ HA.class "px-4 pb-4" ] (viewForm model) ]

                else
                    []
               )
        )


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
                    { message = "ひな形を写して、最初の絵を焼いています… 1〜2分かかります"
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
