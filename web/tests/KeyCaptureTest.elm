module KeyCaptureTest exposing (suite)

{-| キー割り当ての聞き取り。code → キー名の写しと、聞き取り中の 1 打の結末
(割り当て・重複で弾く・Esc で閉じる・聞き流す)を具体値で固定する。
-}

import Expect
import Test exposing (Test, describe, test)
import Widgets.KeyCapture as KeyCapture exposing (Outcome(..))


choices : List String
choices =
    [ "Z", "Enter", "Space", "Left", "LeftShift" ]


ctx : Maybe Int -> List String -> { choices : List String, items : List String, index : Maybe Int }
ctx index items =
    { choices = choices, items = items, index = index }


suite : Test
suite =
    describe "KeyCapture — キーの聞き取り"
        [ test "code → キー名 — 英字・数字・矢印・特殊・記号・修飾・Numpad・F 列の代表" <|
            \_ ->
                [ "KeyA", "Digit7", "ArrowLeft", "Space", "Enter", "BracketLeft", "Backquote", "Quote", "ShiftRight", "MetaLeft", "Numpad3", "F5", "ContextMenu" ]
                    |> List.map KeyCapture.keyOfCode
                    |> Expect.equal
                        [ Just "A", Just "Digit7", Just "Left", Just "Space", Just "Enter", Just "LeftBracket", Just "GraveAccent", Just "Apostrophe", Just "RightShift", Just "LeftSuper", Just "Numpad3", Just "F5", Just "Menu" ]
        , test "エンジンに無い code は Nothing" <|
            \_ ->
                [ "IntlYen", "Fn", "MediaPlayPause", "KeyAB" ]
                    |> List.map KeyCapture.keyOfCode
                    |> Expect.equal [ Nothing, Nothing, Nothing, Nothing ]
        , test "Numpad はエンジンに無い code も素通し(実在の判定は enum の仕事)" <|
            \_ ->
                KeyCapture.keyOfCode "NumpadComma"
                    |> Expect.equal (Just "NumpadComma")
        , test "追加(index 無し)は末尾へ・差し替え(index 付き)はその行だけ変わる" <|
            \_ ->
                ( KeyCapture.decide (ctx Nothing [ "Z" ]) "Space"
                , KeyCapture.decide (ctx (Just 0) [ "Z", "Enter" ]) "Space"
                )
                    |> Expect.equal
                        ( Assigned [ "Z", "Space" ]
                        , Assigned [ "Space", "Enter" ]
                        )
        , test "同じ行への同じキー・行が既に無い差し替えは、何も書かずに閉じる" <|
            \_ ->
                ( KeyCapture.decide (ctx (Just 1) [ "Z", "Enter" ]) "Enter"
                , KeyCapture.decide (ctx (Just 99) [ "Z" ]) "Space"
                )
                    |> Expect.equal ( Dismissed, Dismissed )
        , test "別の行に居るキーは弾く(理由つき)" <|
            \_ ->
                KeyCapture.decide (ctx Nothing [ "Z" ]) "KeyZ"
                    |> Expect.equal (Rejected "既に割り当て済み: Z")
        , test "Esc は黙って閉じる(Escape を choices に入れても聞き取りでは拾わない)" <|
            \_ ->
                ( KeyCapture.decide (ctx Nothing []) "Escape"
                , KeyCapture.decide { choices = [ "Escape" ], items = [], index = Nothing } "Escape"
                )
                    |> Expect.equal ( Dismissed, Dismissed )
        , test "知らない code・choices に無いキーは聞き流す(聞き取り続行)" <|
            \_ ->
                ( KeyCapture.decide (ctx Nothing []) "IntlYen"
                , KeyCapture.decide (ctx Nothing []) "KeyQ"
                )
                    |> Expect.equal ( Pending, Pending )
        ]
