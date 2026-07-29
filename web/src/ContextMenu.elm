module ContextMenu exposing
    ( Anchor
    , Item
    , danger
    , item
    , onOpen
    , placeIn
    , separator
    , view
    )

{-| 右クリックの小さなメニュー(汎用)。

一覧の行にも盤面の印にも同じ物を出せるよう、中身は「札の列 + 押した時の便り」
だけを受け取る。どこに出すか(カーソルの位置)と、閉じ方(外側クリック / Esc)は
どの呼び出し元でも同じなので、こちらが持つ。

画面の端では、はみ出す方向へ寄せる。開いた位置と一緒に窓の大きさも受け取るのは、
そのためだけ — メニューのために窓の大きさを Model に持ち回らせない。

-}

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


{-| 出す場所(カーソル)と、その時の窓の大きさ。 -}
type alias Anchor =
    { x : Float
    , y : Float
    , windowW : Float
    , windowH : Float
    }


type Item msg
    = Command { label : String, msg : msg, danger : Bool }
    | Separator


item : String -> msg -> Item msg
item label msg =
    Command { label = label, msg = msg, danger = False }


{-| 取り返しのつかない札(削除)。赤字にして、他の札と見間違えないようにする。 -}
danger : String -> msg -> Item msg
danger label msg =
    Command { label = label, msg = msg, danger = True }


separator : Item msg
separator =
    Separator


{-| 右クリックを拾う属性。カーソルの位置と窓の大きさを一緒に読む
(preventDefault はブラウザ標準のメニューを出さないため)。
-}
onOpen : (Anchor -> msg) -> Html.Attribute msg
onOpen toMsg =
    HE.custom "contextmenu"
        (D.map4 (\x y w h -> { x = x, y = y, windowW = w, windowH = h })
            (D.field "clientX" D.float)
            (D.field "clientY" D.float)
            (windowSize "innerWidth" 1280)
            (windowSize "innerHeight" 800)
            |> D.map
                (\anchor ->
                    { message = toMsg anchor, stopPropagation = True, preventDefault = True }
                )
        )


{-| 窓の大きさ。読めない環境(古い WebView 等)は既定値に倒す —
はみ出しの手当てが効かないだけで、メニューは出す。
-}
windowSize : String -> Float -> D.Decoder Float
windowSize field fallback =
    D.oneOf
        [ D.at [ "view", field ] D.float
        , D.at [ "target", "ownerDocument", "defaultView", field ] D.float
        , D.succeed fallback
        ]


{-| 実際に置く左上。右や下がはみ出すならその分だけ手前へ寄せる
(カーソルの下に隠れないよう、最低でも 0 は下回らない)。
-}
placeIn : Anchor -> Int -> { left : Float, top : Float }
placeIn anchor itemCount =
    let
        width =
            200

        height =
            toFloat itemCount * 26 + 8
    in
    { left = max 0 (min anchor.x (anchor.windowW - width))
    , top = max 0 (min anchor.y (anchor.windowH - height))
    }


{-| メニュー本体。後ろ一面の受け皿を敷いて、外側クリックで閉じる
(Esc の購読は呼び側 — 画面全体のキー配りは 1 か所に集めたいため)。
-}
view : { onClose : msg } -> Anchor -> List (Item msg) -> Html msg
view handlers anchor items =
    let
        spot =
            placeIn anchor (List.length items)
    in
    div
        [ HA.class "context-menu-layer fixed inset-0 z-[60]"
        , HE.onClick handlers.onClose
        , HE.custom "contextmenu"
            (D.succeed { message = handlers.onClose, stopPropagation = True, preventDefault = True })
        ]
        [ div
            [ HA.class "context-menu min-w-[200px] rounded border border-edge bg-panel py-1 shadow-[0_4px_16px_rgb(0_0_0/0.45)]"
            , HA.style "position" "fixed"
            , HA.style "left" (String.fromFloat spot.left ++ "px")
            , HA.style "top" (String.fromFloat spot.top ++ "px")
            ]
            (List.map viewItem items)
        ]


viewItem : Item msg -> Html msg
viewItem entry =
    case entry of
        Command command ->
            button
                [ HA.classList
                    [ ( "context-item block w-full cursor-pointer px-3 py-1 text-left text-xs", True )
                    , ( "text-danger hover:bg-danger/15", command.danger )
                    , ( "text-ink-soft hover:bg-white/5 hover:text-ink", not command.danger )
                    ]

                -- 受け皿の「外側クリックで閉じる」に食わせない(札の仕事が先)
                , HE.stopPropagationOn "click" (D.succeed ( command.msg, True ))
                ]
                [ text command.label ]

        Separator ->
            div [ HA.class "context-separator my-1 border-t border-edge" ] []
