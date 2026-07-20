module DraftTest exposing (suite)

{-| 打ちかけ(draft)の確定パースと ↑↓ 増減の純ロジック。
int 丸め・clamp・step の既定値(int 1 / float 0.1)・Shift ×10・
浮動小数の桁ゴミ払い・パース不能時に値を出さないことを固定する。
-}

import Draft
import Expect
import Test exposing (Test, describe, test)


{-| level.json の atX 相当(min だけの int)。 -}
intSpec : Draft.NumberSpec
intSpec =
    { isInt = True, min = Just 0, max = Just 100, step = Nothing }


floatSpec : Draft.NumberSpec
floatSpec =
    { isInt = False, min = Just 0, max = Just 2, step = Nothing }


{-| routes.speed 相当(スキーマが step を宣言する float)。 -}
steppedSpec : Draft.NumberSpec
steppedSpec =
    { isInt = False, min = Just 0, max = Just 150, step = Just 5 }


{-| 増減結果は表示文字で比べる(欄に見える形ごと固定する)。 -}
stepText : Draft.NumberSpec -> { dir : Int, shift : Bool } -> String -> Maybe String
stepText spec arg text =
    Draft.step spec arg text |> Maybe.map (Draft.format spec)


suite : Test
suite =
    describe "Draft"
        [ describe "parse(確定)"
            [ test "int は前後の空白ごと整数に確定する" <|
                \_ ->
                    Draft.parse intSpec " 12 "
                        |> Expect.equal (Just 12)
            , test "int 欄への小数は四捨五入で受ける" <|
                \_ ->
                    Draft.parse intSpec "3.6"
                        |> Expect.equal (Just 4)
            , test "min/max の外は clamp して確定する" <|
                \_ ->
                    ( Draft.parse intSpec "999", Draft.parse intSpec "-5" )
                        |> Expect.equal ( Just 100, Just 0 )
            , test "パース不能は Nothing(編集を出さない)" <|
                \_ ->
                    ( Draft.parse intSpec "12abc", Draft.parse intSpec "" )
                        |> Expect.equal ( Nothing, Nothing )
            ]
        , describe "step(↑↓ 増減)"
            [ test "int は 1 ずつ動く" <|
                \_ ->
                    ( stepText intSpec { dir = 1, shift = False } "12"
                    , stepText intSpec { dir = -1, shift = False } "12"
                    )
                        |> Expect.equal ( Just "13", Just "11" )
            , test "Shift 併用は ×10" <|
                \_ ->
                    stepText intSpec { dir = 1, shift = True } "12"
                        |> Expect.equal (Just "22")
            , test "float の既定 step は 0.1 で桁ゴミを出さない" <|
                \_ ->
                    stepText floatSpec { dir = 1, shift = False } "0.2"
                        |> Expect.equal (Just "0.3")
            , test "スキーマの step があればそれで動く" <|
                \_ ->
                    stepText steppedSpec { dir = 1, shift = False } "85"
                        |> Expect.equal (Just "90")
            , test "min/max で clamp される" <|
                \_ ->
                    ( stepText steppedSpec { dir = 1, shift = False } "148"
                    , stepText intSpec { dir = -1, shift = True } "5"
                    )
                        |> Expect.equal ( Just "150", Just "0" )
            , test "空欄は 0 起点で動く" <|
                \_ ->
                    stepText intSpec { dir = 1, shift = False } ""
                        |> Expect.equal (Just "1")
            , test "数字になっていない打ちかけからは動かない" <|
                \_ ->
                    stepText intSpec { dir = 1, shift = False } "1x"
                        |> Expect.equal Nothing
            ]
        ]
