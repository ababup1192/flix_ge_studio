module Journey exposing
    ( Model
    , Msg(..)
    , Nav(..)
    , State
    , failed
    , init
    , loaded
    , stateDecoder
    , update
    , view
    )

{-| ホーム(旅路)画面。GET /journey/state の「つぎの一手」1 枚と道しるべを出す。

サーバ往復は持たない(封筒の発行・受領は Main の request / handleOk が一元管理)。
ここは応答の読み取り(stateDecoder)と、読み取った状態の見せ方だけ。
エンドポイント未実装のサーバでも画面を落とさない — 失敗は「準備中」の 1 枚に倒す。

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


{-| 提案の行き先。サーバの文字列(atelier|gallery|home)を型に起こす —
知らない文字列はホーム留まり(押しても壊れない)。
-}
type Nav
    = ToAtelier
    | ToGallery
    | ToHome


type alias Suggestion =
    { id : String
    , title : String
    , detail : String
    , nav : Nav
    }


type alias Checks =
    { atelierCandidates : Int
    , staleGallery : Bool
    , diffCount : Int
    }


type alias State =
    { suggestion : Suggestion
    , checks : Checks
    }


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

        "gallery" ->
            ToGallery

        _ ->
            ToHome


checksDecoder : D.Decoder Checks
checksDecoder =
    D.map3 Checks
        (D.field "atelierCandidates" D.int)
        (D.field "staleGallery" D.bool)
        (D.field "diffCount" D.int)



-- 画面


view : Model -> Html Msg
view model =
    div [ HA.class "journey mx-auto mt-10 w-full max-w-lg px-4" ]
        (case model of
            Loading ->
                [ quietCard "読み込み中…" "つぎの一手を考えています。" ]

            Failed _ ->
                -- サーバがまだ /journey/state を持たない段階でも画面は生かす
                [ quietCard "準備中" "旅路の案内はまだ使えません。アトリエやギャラリーは開けます。" ]

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
                [ div [ HA.class "text-[11px] text-ink-faint" ] [ text "つぎの一手" ]
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
    let
        parts =
            List.concat
                [ if checks.atelierCandidates > 0 then
                    [ "候補 " ++ String.fromInt checks.atelierCandidates ++ " 件" ]

                  else
                    []
                , if checks.diffCount > 0 then
                    [ "見た目の差分 " ++ String.fromInt checks.diffCount ++ " 件" ]

                  else
                    []
                , if checks.staleGallery then
                    [ "ギャラリーが古くなっています" ]

                  else
                    []
                ]
    in
    if List.isEmpty parts then
        text ""

    else
        div [ HA.class "mt-1.5 text-[11px] text-ink-faint" ] [ text (String.join " ・ " parts) ]


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

        "bake" ->
            "🔥"

        "bless" ->
            "✨"

        "create" ->
            "🌱"

        _ ->
            "🧭"


goLabel : Nav -> String
goLabel nav =
    case nav of
        ToAtelier ->
            "アトリエへ"

        ToGallery ->
            "ギャラリーへ"

        ToHome ->
            "はじめる"


{-| 道しるべ。提案 id から現在地を推し量る — 進行の正確な管理はサーバの仕事で、
ここは「いまどの辺りか」の雰囲気だけを見せる。create(新しい一巡の始まり)は
現在地なし・済みなしの全部淡色 — 済みで塗ると初回起動の初学者が
「もう全部終わっている?」と誤解する(完了状態は UI 上に存在しない)。
-}
viewTrail : String -> Html msg
viewTrail suggestionId =
    let
        current =
            case suggestionId of
                "pick" ->
                    Just 0

                "bake" ->
                    Just 2

                "bless" ->
                    Just 3

                _ ->
                    -- "create" と未知の id: どのステップも点けない
                    Nothing

        steps =
            [ "候補を選ぶ", "装着", "焼いて確認", "祝福", "完了" ]

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
