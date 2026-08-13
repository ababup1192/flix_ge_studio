module FormHelp exposing (body, section, toggle)

{-| 説明書き(schema の help)の見せ方。

label は短い名前、hint は 1 行の効き目、help は畳んである長い文 — この 3 段の
うち help だけが「開いている間だけ画面に置く」。開いている名前の集まりは
呼び側(Model)が持ち、ここは見せ方だけを決める。

閉じている help の本文は作らない。フォームの欄が数十個あって、その全部に
長い説明が付いていても、描く仕事が増えないのはこの一点による。

-}

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Schema
import Set exposing (Set)


{-| ラベルの隣の小さな "?"。help を書いていない欄には出さない
(既存のスキーマの見た目は変わらない)。
-}
toggle : (String -> msg) -> Set String -> String -> Maybe String -> Html msg
toggle onToggle open key help =
    case help of
        Just _ ->
            button
                [ HA.class "help-toggle ml-1 inline-flex h-3.5 w-3.5 cursor-pointer items-center justify-center rounded-full border border-edge text-[9px] leading-none text-ink-faint hover:border-accent hover:text-accent"
                , HA.title "説明"
                , HA.attribute "aria-expanded"
                    (if Set.member key open then
                        "true"

                     else
                        "false"
                    )
                , HE.stopPropagationOn "click" (D.succeed ( onToggle key, True ))
                ]
                [ text "?" ]

        Nothing ->
            text ""


{-| 開いている間だけ本文を作る — 閉じたままの説明は画面に置かない。 -}
body : Set String -> String -> Maybe String -> Html msg
body open key help =
    case help of
        Just content ->
            if Set.member key open then
                div
                    [ HA.class "form-help mb-1 rounded border border-edge bg-well/40 px-1.5 py-1 text-[10px] leading-relaxed break-words whitespace-pre-wrap text-ink-faint" ]
                    [ text content ]

            else
                text ""

        Nothing ->
            text ""


{-| セクションの説明書き(一覧などの見出しの隣)。help が無ければ何も足さない。 -}
section : (String -> msg) -> Set String -> String -> Schema.Section -> List (Html msg)
section onToggle open key sec =
    case sec.help of
        Just _ ->
            let
                key_ =
                    sectionKey key
            in
            [ div [ HA.class "section-help mb-1.5" ]
                [ div [ HA.class "flex items-center text-[11px] text-ink-soft" ]
                    [ text (Maybe.withDefault key sec.label)
                    , toggle onToggle open key_ sec.help
                    ]
                , body open key_ sec.help
                ]
            ]

        Nothing ->
            []


{-| セクションの開閉のキー。欄(書き戻し先のパス)と衝突しない名前空間を持たせる。 -}
sectionKey : String -> String
sectionKey key =
    "section:" ++ key
