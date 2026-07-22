module Progress exposing (excerpt, view)

{-| 進捗ミニパネル — 長い外部処理(bake / ゲーム起動)の見せ方をここで統一する。

既定は「動くしるし + 一言 + ログ末尾 2〜3 行」だけの小さな顔。
右上の ⤢ で全文のモノスペースパネルに展開できる(もう一度で畳む)。
展開フラグは呼び手の model が持つ(既定は畳み、失敗時だけ呼び手が自動で開く —
失敗の時だけは隠さない、という規則も呼び手の update が正)。

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE


type alias Config msg =
    { message : String
    , lines : List String
    , failed : Bool
    , expanded : Bool
    , onToggle : msg
    }


view : Config msg -> Html msg
view cfg =
    div [ HA.class "progress-panel rounded border border-edge bg-well p-2" ]
        [ div [ HA.class "flex items-center gap-2" ]
            [ if cfg.failed then
                span [ HA.class "shrink-0 text-xs text-danger" ] [ text "✗" ]

              else
                span [ HA.class "progress-spinner shrink-0", HA.attribute "aria-hidden" "true" ] []
            , span
                [ HA.classList
                    [ ( "min-w-0 flex-1 text-[11px]", True )
                    , ( "text-danger", cfg.failed )
                    , ( "text-ink-soft", not cfg.failed )
                    ]
                ]
                [ text cfg.message ]
            , button
                [ HA.class "btn btn-ghost btn-mini shrink-0"
                , HA.title
                    (if cfg.expanded then
                        "ログをたたむ"

                     else
                        "ログ全体を見る"
                    )
                , HE.onClick cfg.onToggle
                ]
                [ text
                    (if cfg.expanded then
                        "⤡ たたむ"

                     else
                        "⤢ ログ全体"
                    )
                ]
            ]
        , if cfg.expanded then
            -- flex-col-reverse + 逆順描画 = 見た目は正順のまま、ブラウザの
            -- スクロール錨が「末尾」に付く(開いた時も、新しい行が来た時も
            -- 自動で最新行へ追随する。port なしで済む唯一の素直な手)
            div [ HA.class "mt-1.5 flex max-h-56 flex-col-reverse overflow-y-auto font-mono text-[11px] leading-relaxed whitespace-pre-wrap text-ink-soft" ]
                (if List.isEmpty cfg.lines then
                    [ div [ HA.class "text-ink-faint" ] [ text "(ログ待ち…)" ] ]

                 else
                    List.map (\line -> div [] [ text line ]) (List.reverse cfg.lines)
                )

          else
            case excerpt cfg.lines of
                [] ->
                    text ""

                tail ->
                    div [ HA.class "mt-1.5 font-mono text-[10px] leading-relaxed text-ink-faint" ]
                        (List.map
                            (\line -> div [ HA.class "overflow-hidden text-ellipsis whitespace-nowrap" ] [ text line ])
                            tail
                        )
        ]


{-| 畳んだ姿に見せる抜粋 = 常にログの「最後の 3 行」。新しい行が届くたびに
末尾へ追随する(先頭で止まったままにしない — 進んでいる感が要)。
-}
excerpt : List String -> List String
excerpt lines =
    List.drop (max 0 (List.length lines - 3)) lines
