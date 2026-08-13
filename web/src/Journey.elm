module Journey exposing
    ( Change
    , Changes
    , Model
    , Msg(..)
    , Nav(..)
    , State
    , changesDecoder
    , failed
    , gameRunning
    , init
    , loaded
    , starterFresh
    , stateDecoder
    , update
    , view
    )

{-| ホーム画面。GET /journey/state の「次のやること」1 枚と道しるべを出す。

サーバ往復は持たない(封筒の発行・受領は Main の request / handleOk が一元管理)。
ここは応答の読み取り(stateDecoder / changesDecoder)と、読み取った状態の見せ方だけ。
エンドポイント未実装のサーバでも画面を落とさない — 失敗は「準備中」の 1 枚に倒す。

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


{-| 提案の行き先。サーバの文字列(atelier|changes|launch|arrange|home)を型に起こす —
知らない文字列はホーム留まり(押しても壊れない)。
-}
type Nav
    = ToAtelier
    | ToChanges
    | ToLaunch
    | ToArrange
    | ToHome


type alias Suggestion =
    { id : String
    , title : String
    , detail : String
    , nav : Nav
    }


type alias Checks =
    { atelierCandidates : Int

    -- 生まれたてのテンプレート(sample.kind.json が残っている)か。
    -- 「テンプレートのまま」前提の UI(インタビューのチップ等)の出し分けに使う
    , hasStarterDoc : Bool

    -- このプロジェクトのゲームが走っているか
    , gameRunning : Bool
    }


type alias State =
    { suggestion : Suggestion
    , checks : Checks
    }


{-| この状態は「生まれたてのテンプレート」か。Main が Atelier へ渡す(チップの出し分け)。 -}
starterFresh : State -> Bool
starterFresh state =
    state.checks.hasStarterDoc


{-| checks の gameRunning(読めていない間は「走っていない」に倒す)。
Main のミニプレイヤーの状態行が使う。
-}
gameRunning : Model -> Bool
gameRunning model =
    case model of
        Loaded info ->
            info.state.checks.gameRunning

        _ ->
            False


{-| skipped は「今日はこの提案に乗らない」の印。次の応答が来たら新しい提案として
また出す(スキップを覚え込まない — 提案は毎回サーバが選び直す)。
-}
type Model
    = Loading
    | Failed String
    | Loaded { state : State, skipped : Bool }


init : Model
init =
    Loading


loaded : State -> Model
loaded state =
    Loaded { state = state, skipped = False }


failed : String -> Model
failed message =
    Failed message


type Msg
    = SkipClicked
    | GoClicked Nav


{-| 画面遷移はタブの持ち主(Main)の仕事なので、行き先は値で返すだけ。 -}
update : Msg -> Model -> ( Model, Maybe Nav )
update msg model =
    case msg of
        SkipClicked ->
            case model of
                Loaded info ->
                    ( Loaded { info | skipped = True }, Nothing )

                _ ->
                    ( model, Nothing )

        GoClicked nav ->
            ( model, Just nav )



-- デコーダ


stateDecoder : D.Decoder State
stateDecoder =
    D.map2 State
        (D.field "suggestion" suggestionDecoder)
        (D.field "checks" checksDecoder)


suggestionDecoder : D.Decoder Suggestion
suggestionDecoder =
    D.map4 Suggestion
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "detail" D.string)
        (D.field "nav" (D.map navFrom D.string))


navFrom : String -> Nav
navFrom raw =
    case raw of
        "atelier" ->
            ToAtelier

        "changes" ->
            ToChanges

        "launch" ->
            ToLaunch

        "arrange" ->
            ToArrange

        _ ->
            ToHome


checksDecoder : D.Decoder Checks
checksDecoder =
    D.map3 Checks
        (D.field "atelierCandidates" D.int)
        -- 無いサーバでは「テンプレートではない」に倒す(fail-open)
        (D.oneOf [ D.field "hasStarterDoc" D.bool, D.succeed False ])
        -- 無いサーバでは「走っていない」に倒す(fail-open)
        (D.oneOf [ D.field "gameRunning" D.bool, D.succeed False ])


{-| GET /journey/changes — 自動検査の知らせ。baking は描き出しが走っている最中、
changes は「前と今」を見比べられる場面の列。
-}
type alias Changes =
    { baking : Bool
    , seen : Bool
    , changes : List Change
    }


{-| 変わった場面 1 件。before/after はプロジェクト相対パス
(reference/archive/<場面>.vN.png / reference/<場面>.png)。
-}
type alias Change =
    { name : String
    , ver : Int
    , before : String
    , after : String
    }


changesDecoder : D.Decoder Changes
changesDecoder =
    D.map3 Changes
        (D.field "baking" D.bool)
        (D.oneOf [ D.field "seen" D.bool, D.succeed True ])
        (D.oneOf [ D.field "changes" (D.list changeDecoder), D.succeed [] ])


changeDecoder : D.Decoder Change
changeDecoder =
    D.map4 Change
        (D.field "name" D.string)
        (D.field "ver" D.int)
        (D.field "before" D.string)
        (D.field "after" D.string)



-- 画面


view : Model -> Html Msg
view model =
    div [ HA.class "journey mx-auto mt-10 w-full max-w-lg px-4" ]
        (case model of
            Loading ->
                [ quietCard "読み込み中…" "次のやることを考えています。" ]

            Failed _ ->
                -- 提案が読めない時も行き止まりにしない。して欲しいことを言う
                [ div [ HA.class "journey-card rounded-lg border border-edge bg-panel p-5" ]
                    [ div [ HA.class "text-sm font-semibold text-ink" ] [ text "ゲームをアレンジしてみましょう" ]
                    , div [ HA.class "mt-1.5 text-xs text-ink-soft" ]
                        [ text "ゲームのパラメータを変えられます。保存した瞬間、走っているゲームに反映されます。" ]
                    , div [ HA.class "mt-4" ]
                        [ button [ HA.class "btn btn-primary", HE.onClick (GoClicked ToArrange) ]
                            [ text "アレンジする" ]
                        ]
                    ]
                ]

            Loaded info ->
                if info.skipped then
                    [ quietCard "今日は自由にどうぞ" "提案はお休みしました。次に開いた時にまた考えます。"
                    , viewTrail info.state.suggestion.id
                    ]

                else
                    [ viewSuggestion info.state
                    , viewTrail info.state.suggestion.id
                    ]
        )


viewSuggestion : State -> Html Msg
viewSuggestion state =
    let
        s =
            state.suggestion
    in
    div [ HA.class "journey-card rounded-lg border border-edge bg-panel p-5 shadow-[0_2px_8px_rgb(0_0_0/0.35)]" ]
        [ div [ HA.class "flex items-start gap-3" ]
            [ span [ HA.class "journey-icon text-3xl leading-none" ] [ text (iconFor s.id) ]
            , div [ HA.class "min-w-0 flex-1" ]
                [ div [ HA.class "text-[11px] text-ink-faint" ] [ text "次のやること" ]
                , div [ HA.class "mt-0.5 text-sm font-semibold text-ink" ] [ text s.title ]
                , div [ HA.class "mt-1.5 text-xs leading-relaxed text-ink-soft" ] [ text s.detail ]
                , viewChecksLine state.checks
                ]
            ]
        , div [ HA.class "mt-4 flex items-center gap-3" ]
            [ button [ HA.class "btn btn-primary", HE.onClick (GoClicked s.nav) ]
                [ text (goLabel s.nav) ]
            , button [ HA.class "journey-skip cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft", HE.onClick SkipClicked ]
                [ text "スキップ" ]
            ]
        ]


{-| 提案の根拠を 1 行で添える(空なら出さない)。数字が見えると提案に納得しやすい。 -}
viewChecksLine : Checks -> Html msg
viewChecksLine checks =
    if checks.atelierCandidates > 0 then
        div [ HA.class "mt-1.5 text-[11px] text-ink-faint" ]
            [ text ("候補 " ++ String.fromInt checks.atelierCandidates ++ " 件") ]

    else
        text ""


quietCard : String -> String -> Html msg
quietCard title detail =
    div [ HA.class "journey-card rounded-lg border border-edge bg-panel p-5" ]
        [ div [ HA.class "text-sm font-semibold text-ink" ] [ text title ]
        , div [ HA.class "mt-1.5 text-xs text-ink-soft" ] [ text detail ]
        ]


iconFor : String -> String
iconFor id =
    case id of
        "pick" ->
            "🎨"

        "changed" ->
            "🔔"

        "create" ->
            "🌱"

        "launch" ->
            "🕹️"

        "arrange" ->
            "🎨"

        _ ->
            "🧭"


goLabel : Nav -> String
goLabel nav =
    case nav of
        ToAtelier ->
            "アトリエへ"

        ToChanges ->
            "見比べる"

        ToLaunch ->
            "▶ 起動する"

        ToArrange ->
            "アレンジする"

        ToHome ->
            "はじめる"


{-| 道しるべ。提案 id から現在地を推し量る — 進行の正確な管理はサーバの仕事で、
ここは「いまどの辺りか」の雰囲気だけを見せる。create(新しい一巡の始まり)は
現在地なし・済みなしの全部淡色 — 済みで塗ると初回起動の初学者が
「もう全部終わっている?」と誤解する(完了状態は UI 上に存在しない)。
-}
viewTrail : String -> Html msg
viewTrail sid =
    let
        -- 生まれたて(launch / arrange)は「遊ぶ → アレンジ → 反映」の一巡、
        -- それ以外はアセットの一巡(候補 → 使う → 知らせ)
        ( steps, current ) =
            case sid of
                "launch" ->
                    ( starterSteps, Just 0 )

                "arrange" ->
                    ( starterSteps, Just 1 )

                "pick" ->
                    ( materialSteps, Just 0 )

                "changed" ->
                    ( materialSteps, Just 2 )

                _ ->
                    -- "create" と未知の id: どのステップも点けない
                    ( materialSteps, Nothing )

        starterSteps =
            [ "遊んでみる", "アレンジする", "ゲームに反映させる" ]

        materialSteps =
            [ "候補を選ぶ", "使う", "変わったら知らせ" ]

        done index =
            case current of
                Just c ->
                    index < c

                Nothing ->
                    False

        step index label =
            span
                [ HA.classList
                    [ ( "journey-step badge", True )
                    , ( "journey-step-done bg-ok/15 text-ok", done index )
                    , ( "journey-step-current bg-accent/20 text-accent ring-1 ring-accent/50", current == Just index )
                    ]
                ]
                [ text
                    ((if done index then
                        "✓ "

                      else
                        ""
                     )
                        ++ label
                    )
                ]
    in
    div []
        [ if current == Nothing then
            div [ HA.class "journey-trail-note mt-5 text-[11px] text-ink-faint" ]
                [ text "新しい一巡を始めましょう" ]

          else
            text ""
        , div
            [ HA.classList
                [ ( "journey-trail flex flex-wrap items-center gap-1.5", True )
                , ( "mt-2", current == Nothing )
                , ( "mt-5", current /= Nothing )
                ]
            ]
            (steps
                |> List.indexedMap
                    (\index label ->
                        if index == 0 then
                            [ step index label ]

                        else
                            [ span [ HA.class "text-[10px] text-ink-faint" ] [ text "→" ], step index label ]
                    )
                |> List.concat
            )
        ]
