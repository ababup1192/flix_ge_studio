module Widgets.KeyCapture exposing (Outcome(..), decide, keyOfCode)

{-| キー割り当て欄(widget "key")の聞き取り。

KeyboardEvent.code(物理キー位置)をエンジンのキー名へ写し、聞き取り中の
1 打をどう扱うかを決める。ここは純ロジック(Html を作らない・状態も持たない)。

code を使うのは、エンジン側(GLFW)も物理キー位置基準だから — key(文字)で
読むとキーボード配列や IME の状態で名前がずれる。

-}

import Dict exposing (Dict)


{-| 聞き取り中の 1 打の結末。

  - Assigned: 割り当て成立。新しい列をこの形のまま set 1 本で書き戻す
  - Rejected: 別の行に同じキーが居る。理由を見せて聞き取りを閉じる
  - Dismissed: Esc・内容が変わらない打鍵。黙って閉じる
  - Pending: 知らない code・選択肢に無いキー。聞き取りを続ける

-}
type Outcome
    = Assigned (List String)
    | Rejected String
    | Dismissed
    | Pending


decide : { choices : List String, items : List String, index : Maybe Int } -> String -> Outcome
decide ctx code =
    -- Esc は取り消し専用にする(Escape 自体を割り当てたい時は一覧から選ぶ)。
    -- 聞き取りに「やめる」手が 1 つも無いと、押し間違いから戻れない
    if code == "Escape" then
        Dismissed

    else
        case keyOfCode code of
            Nothing ->
                Pending

            Just key ->
                if not (List.member key ctx.choices) then
                    -- スキーマの enum に無い名前は書かない —
                    -- フォーム経由で不正値を作らない(壊れた値の吸収はゲーム側の仕事)
                    Pending

                else
                    case positionOf key ctx.items of
                        Just at ->
                            if Just at == ctx.index then
                                -- 同じ行への同じキーは書かない —
                                -- 無変更の 1 手が履歴と保存の往復に残る
                                Dismissed

                            else
                                -- 同じキーの 2 行目は先勝ちの表で意味を持たないので、
                                -- 黙って増やさず理由を見せて閉じる
                                Rejected ("既に割り当て済み: " ++ key)

                        Nothing ->
                            let
                                placed =
                                    place ctx.index key ctx.items
                            in
                            if placed == ctx.items then
                                -- 差し替え先の行が既に無い(表示と文書がずれた瞬間) —
                                -- 何も書かずに閉じて、ずれたまま上書きしない
                                Dismissed

                            else
                                Assigned placed


{-| code → エンジンのキー名(GameEngine.Key の ToString 表記)。
Nothing は「エンジンに無いキー」— 聞き取りでは聞き流す。
-}
keyOfCode : String -> Maybe String
keyOfCode code =
    if String.startsWith "Key" code && String.length code == 4 then
        Just (String.dropLeft 3 code)

    else if String.startsWith "Digit" code && String.length code == 6 then
        Just code

    else if String.startsWith "Numpad" code then
        -- Numpad0..NumpadEqual は browser と engine で名前が同じ。
        -- engine に無い code(NumpadComma 等)もそのまま通す — enum の
        -- 実在チェックが後段に居るので、ここで一覧を抱え込まない
        Just code

    else
        Dict.get code specials


specials : Dict String String
specials =
    Dict.fromList
        ([ ( "ArrowUp", "Up" )
         , ( "ArrowDown", "Down" )
         , ( "ArrowLeft", "Left" )
         , ( "ArrowRight", "Right" )
         , ( "Enter", "Enter" )
         , ( "Space", "Space" )
         , ( "Escape", "Escape" )
         , ( "Tab", "Tab" )
         , ( "Backspace", "Backspace" )
         , ( "Insert", "Insert" )
         , ( "Delete", "Delete" )
         , ( "PageUp", "PageUp" )
         , ( "PageDown", "PageDown" )
         , ( "Home", "Home" )
         , ( "End", "End" )
         , ( "CapsLock", "CapsLock" )
         , ( "Quote", "Apostrophe" )
         , ( "Comma", "Comma" )
         , ( "Minus", "Minus" )
         , ( "Period", "Period" )
         , ( "Slash", "Slash" )
         , ( "Semicolon", "Semicolon" )
         , ( "Equal", "Equal" )
         , ( "BracketLeft", "LeftBracket" )
         , ( "BracketRight", "RightBracket" )
         , ( "Backslash", "Backslash" )
         , ( "Backquote", "GraveAccent" )
         , ( "ShiftLeft", "LeftShift" )
         , ( "ShiftRight", "RightShift" )
         , ( "ControlLeft", "LeftCtrl" )
         , ( "ControlRight", "RightCtrl" )
         , ( "AltLeft", "LeftAlt" )
         , ( "AltRight", "RightAlt" )
         , ( "MetaLeft", "LeftSuper" )
         , ( "MetaRight", "RightSuper" )
         , ( "ContextMenu", "Menu" )
         ]
            ++ (List.range 1 12 |> List.map (\n -> ( "F" ++ String.fromInt n, "F" ++ String.fromInt n )))
        )


-- List.Extra の elemIndex / setAt 相当。2 関数のために direct 依存を増やさない
-- (list-extra はテストの間接依存にしか居ない)


positionOf : String -> List String -> Maybe Int
positionOf key items =
    items
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, v ) -> v == key)
        |> List.head
        |> Maybe.map Tuple.first


place : Maybe Int -> String -> List String -> List String
place index key items =
    case index of
        Nothing ->
            items ++ [ key ]

        Just i ->
            items
                |> List.indexedMap
                    (\n v ->
                        if n == i then
                            key

                        else
                            v
                    )
