module SceneView exposing
    ( Handlers
    , State
    , galleryCountUrl
    , galleryImageUrl
    , imageUrl
    , sceneLabel
    , shownScene
    , view
    , viewZoom
    )

{-| 焼き上がった場面の絵(golden / gallery の PNG)を見せる物一式。

今はミニプレイヤー(右下の小窓)と、その拡大表示が住む。絵の URL の組み方と
「どの場面を映すか」の決めごとも、見せる側と同じ場所に置く — 画面が増えても
同じ絵が同じ規則で出るように。

呼び側(Main)からは Handlers(押された時に投げる便り)と State(映すのに
要る今の状態)だけを受け取る。Model 丸ごとは知らない。

-}

import Html exposing (Html, button, div, img, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Api
import Journey
import Url


{-| 小窓の操作が投げる便り。 -}
type alias Handlers msg =
    { onToggle : msg
    , onScene : Maybe String -> msg
    , onZoomOpen : String -> msg
    , onZoomClosed : msg
    , onStart : msg
    }


{-| 映すのに要る今の状態。running / starting は「走っているか」の判定を
呼び側に任せるための 2 つ(ここは絵と操作の担当で、起動の事情は知らない)。
-}
type alias State =
    { open : Bool
    , swapNotice : Bool
    , scenes : List String
    , pin : Maybe String
    , changes : List Journey.Change
    , baking : Bool
    , zoom : Maybe String
    , refresh : Int
    , serverBase : String
    , root : String
    , running : Bool
    , starting : Bool
    , failed : Bool
    }


{-| 画像の URL。base(接続先サーバ)+ クエリで組む — vite dev(別オリジン)
でも .app(同一オリジン = base 空)でも同じ式で通すため。
p= はプロジェクトの識別子(root 由来)— title.png 等の定番名はプロジェクト間で
同名になるので、URL に混ぜないと前のプロジェクトの絵がブラウザキャッシュから
出てしまう。サーバは未知のクエリを無視する(変更不要)。
-}
galleryImageUrl : String -> String -> String -> String -> String
galleryImageUrl base root dir name =
    base
        ++ "/gallery/image?p="
        ++ Api.projectKey root
        ++ "&dir="
        ++ Url.percentEncode dir
        ++ "&name="
        ++ Url.percentEncode name


{-| dir 直下の *.png の枚数を数える口(復元した焼き上がりのコマ数を知るのに使う
— 焼きの応答そのものには乗らない情報を、後から棚を見て埋め合わせる)。
p= の要り用は galleryImageUrl と同じ。
-}
galleryCountUrl : String -> String -> String -> String
galleryCountUrl base root dir =
    base
        ++ "/gallery/count?p="
        ++ Api.projectKey root
        ++ "&dir="
        ++ Url.percentEncode dir


view : Handlers msg -> State -> Html msg
view handlers state =
    div [ HA.class "mini-player fixed bottom-4 right-4 z-40" ]
        [ if state.open then
            div [ HA.class "w-72 rounded-lg border border-edge bg-panel p-3 shadow-[0_4px_16px_rgb(0_0_0/0.45)]" ]
                [ viewHeader handlers state
                , viewChips handlers state
                , viewPicture handlers state
                , viewStatus handlers state
                ]

          else
            button
                [ HA.class "mini-player-pill flex cursor-pointer items-center gap-1.5 rounded-full border border-edge bg-panel px-3 py-1.5 text-xs text-ink shadow-[0_2px_8px_rgb(0_0_0/0.35)] hover:bg-white/5"
                , HE.onClick handlers.onToggle
                ]
                [ text "🎞️ ミニプレイヤー" ]
        ]


{-| ヘッダ。クリックで畳む/開く。差し替え直後だけ「✓ 差し替わりました」を添える。 -}
viewHeader : Handlers msg -> State -> Html msg
viewHeader handlers state =
    button
        [ HA.class "flex w-full cursor-pointer items-center gap-1.5 text-left text-xs font-semibold text-ink"
        , HE.onClick handlers.onToggle
        ]
        [ text "🎞️ ミニプレイヤー"
        , if state.swapNotice then
            span [ HA.class "mini-swap shrink-0 text-[10px] font-normal text-ok" ]
                [ text "✓ 差し替わりました" ]

          else
            text ""
        , span [ HA.class "flex-1" ] []
        , span [ HA.class "shrink-0 text-[11px] font-normal text-ink-faint" ] [ text "たたむ" ]
        ]


{-| 場面チップ列。「自動(変わった場面)」が既定、場面チップでピン留め。
場面がまだ無ければ列ごと出さない(絵の枠の空の一言に任せる)。
-}
viewChips : Handlers msg -> State -> Html msg
viewChips handlers state =
    if List.isEmpty state.scenes then
        text ""

    else
        let
            chip cls label active msg =
                button
                    [ HA.classList
                        [ ( cls ++ " badge cursor-pointer", True )
                        , ( "bg-accent/20 text-accent", active )
                        , ( "text-ink-faint hover:text-ink-soft", not active )
                        ]
                    , HE.onClick msg
                    ]
                    [ text label ]
        in
        div [ HA.class "mini-chips mt-2 flex max-h-16 flex-wrap gap-1 overflow-y-auto" ]
            (chip "mini-chip-auto" "自動(変わった場面)" (state.pin == Nothing) (handlers.onScene Nothing)
                :: List.map
                    (\name ->
                        chip "mini-chip"
                            (sceneLabel name)
                            (state.pin == Just name)
                            (handlers.onScene (Just name))
                    )
                    state.scenes
            )


{-| 選択中の場面の絵(golden/ の PNG)。描き直し中はオーバーレイを重ねる。
場面が 1 つも無い時は静かな一言 — 枠は出したまま(下段の起動は生きている)。
-}
viewPicture : Handlers msg -> State -> Html msg
viewPicture handlers state =
    case shownScene { pin = state.pin, changes = state.changes, scenes = state.scenes } of
        Nothing ->
            div [ HA.class "mini-empty mt-2 flex h-28 items-center justify-center rounded border border-edge bg-well px-3 text-center text-[11px] text-ink-faint" ]
                [ text "場面の絵はまだありません" ]

        Just name ->
            div [ HA.class "relative mt-2" ]
                [ img
                    [ HA.class "scene-shot mini-shot block w-full cursor-zoom-in rounded border border-edge bg-well"
                    , HA.src (imageUrl state name)
                    , HA.alt name
                    , HA.title "クリックで拡大"
                    , HE.onClick (handlers.onZoomOpen name)
                    ]
                    []
                , if state.baking then
                    div [ HA.class "mini-baking absolute inset-0 flex items-center justify-center gap-2 rounded bg-black/60 text-[11px] text-ink" ]
                        [ span [ HA.class "progress-spinner shrink-0", HA.attribute "aria-hidden" "true" ] []
                        , text "描き直しています…"
                        ]

                  else
                    text ""
                ]


{-| 下段 — 起動状態と「▶ 起動する」。起動はアトリエと同じ経路
(StartGameClicked)に委譲するので二度押しの守りもそのまま。
-}
viewStatus : Handlers msg -> State -> Html msg
viewStatus handlers state =
    let
        running =
            state.running
    in
    div []
        [ div [ HA.class "mt-2 flex items-center gap-1.5 text-[11px] text-ink-soft" ]
            [ span
                [ HA.classList
                    [ ( "inline-block h-2 w-2 rounded-full", True )
                    , ( "bg-ok", running )
                    , ( "bg-danger", not running && state.failed )
                    , ( "bg-ink-faint", not running && not state.failed )
                    ]
                ]
                []
            , text
                (if running then
                    "起動中"

                 else if state.failed then
                    "起動できませんでした"

                 else
                    "停止中"
                )
            , span [ HA.class "flex-1" ] []
            , if running then
                text ""

              else if state.starting then
                button [ HA.class "btn btn-mini", HA.disabled True ] [ text "⏳ 起動しています…" ]

              else if state.failed then
                button [ HA.class "mini-retry btn btn-mini", HE.onClick handlers.onStart ] [ text "↻ もう一度起動する" ]

              else
                button [ HA.class "btn btn-mini", HE.onClick handlers.onStart ] [ text "▶ 起動する" ]
            ]
        , div [ HA.class "mt-1 text-[10px] text-ink-faint" ]
            [ text
                (if running then
                    "保存はゲームの画面にすぐ反映されます(ここは焼き上がりの記録)"

                 else if state.failed then
                    "アトリエのタブに、しくじった時のログが残っています"

                 else
                    "▶ 起動すると、保存が実際のゲームにすぐ反映されます"
                )
            ]
        ]


{-| ミニプレイヤーが映す場面の決めごと(純関数)。ピン留めが最優先。
「自動」は知らせの最新 — サーバ(Changes)は新しい変化を列の末尾へ積むので
末尾を取る。知らせがまだ無ければ一覧の先頭、場面が無ければ Nothing(空の一言)。
-}
shownScene : { pin : Maybe String, changes : List Journey.Change, scenes : List String } -> Maybe String
shownScene info =
    case info.pin of
        Just name ->
            Just name

        Nothing ->
            case List.reverse info.changes of
                latest :: _ ->
                    Just latest.name

                [] ->
                    List.head info.scenes


{-| 場面チップの見出し(拡張子は落とす — チップは日常語の場面名だけ)。 -}
sceneLabel : String -> String
sceneLabel name =
    if String.endsWith ".png" name then
        String.dropRight 4 name

    else
        name


{-| 場面の絵(golden/ の PNG)の URL。焼き直しで中身が入れ替わるファイルなので
v でキャッシュを破る(知らせの版 + 描き直しの目盛り — 同じ URL のまま古い絵を
出さない)。ミニプレイヤー本体と拡大表示で同じ式(同じ絵)を使う。
-}
imageUrl : State -> String -> String
imageUrl state name =
    let
        ver =
            state.changes
                |> List.filter (\c -> c.name == name)
                |> List.head
                |> Maybe.map .ver
                |> Maybe.withDefault 0
    in
    galleryImageUrl state.serverBase state.root "golden" name
        ++ ("&v=" ++ String.fromInt ver ++ "-" ++ String.fromInt state.refresh)


{-| ミニプレイヤーの絵の拡大。アトリエのプレビュー拡大(lightbox)と同じ流儀 —
暗幕の上に PNG を大きく(〜90vw / 85vh・ドットのまま)、場面名を添える。
閉じるのはクリック・閉じる ボタン・Esc(Esc の購読は subscriptions)。
開いた時の場面名を持つので裏の自動追従で別場面へは切り替わらない —
差し替え(同じ場面の新しい絵)だけが v の進みで映る。
-}
viewZoom : Handlers msg -> State -> Html msg
viewZoom handlers state =
    case state.zoom of
        Nothing ->
            text ""

        Just name ->
            div
                [ HA.class "mini-zoom fixed inset-0 z-50 flex cursor-zoom-out flex-col items-center justify-center gap-3 bg-black/80"
                , HE.onClick handlers.onZoomClosed
                ]
                [ img
                    [ HA.class "scene-shot h-[85vh] max-w-[90vw] object-contain"
                    , HA.src (imageUrl state name)
                    , HA.alt name
                    ]
                    []
                , div [ HA.class "flex items-center gap-3" ]
                    [ div [ HA.class "font-mono text-[11px] text-ink" ] [ text (sceneLabel name) ]
                    , button [ HA.class "btn btn-mini", HE.onClick handlers.onZoomClosed ] [ text "閉じる" ]
                    ]
                ]
