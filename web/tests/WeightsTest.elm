module WeightsTest exposing (suite)

{-| weights(合計固定の複数連動スライダー)の再配分・丸め・行の増減を固定する。
合計の同値は format 後の文字列でも確かめる — 画面と元データに出るのはこの形。
-}

import Expect
import Json.Encode as E
import Test exposing (Test, describe, test)
import Widgets.Weights as Weights


int100 : Weights.Config
int100 =
    { total = 100, decimals = 0 }


unit1 : Weights.Config
unit1 =
    { total = 1.0, decimals = 2 }


sumOf : List ( String, Float ) -> Float
sumOf entries =
    List.sum (List.map Tuple.second entries)


suite : Test
suite =
    describe "Widgets.Weights — 合計固定の再配分"
        [ test "1 本動かしても合計は total のまま(整数 total)" <|
            \_ ->
                Weights.redistribute int100 "claw" 50 [ ( "claw", 70 ), ( "cannon", 30 ) ]
                    |> Expect.equal [ ( "claw", 50 ), ( "cannon", 50 ) ]
        , test "残りは元の比を保ち、端数は最大剰余法で配り切る(合計誤差ゼロ)" <|
            \_ ->
                let
                    result =
                        Weights.redistribute int100 "a" 50 [ ( "a", 60 ), ( "b", 30 ), ( "c", 10 ) ]
                in
                ( result, sumOf result )
                    -- b:37.5 / c:12.5 の同率端数は先に書いた b が勝つ
                    |> Expect.equal ( [ ( "a", 50 ), ( "b", 38 ), ( "c", 12 ) ], 100 )
        , test "0 の行は他を動かしても 0 のまま(比例配分は零を保つ)" <|
            \_ ->
                Weights.redistribute int100 "a" 40 [ ( "a", 70 ), ( "b", 30 ), ( "c", 0 ) ]
                    |> Expect.equal [ ( "a", 40 ), ( "b", 60 ), ( "c", 0 ) ]
        , test "1 行だけなら常に total(動かしようがない)" <|
            \_ ->
                Weights.redistribute int100 "a" 10 [ ( "a", 40 ) ]
                    |> Expect.equal [ ( "a", 100 ) ]
        , test "残りが全部 0 なら均等割(比が決められない時の逃げ道)" <|
            \_ ->
                Weights.redistribute int100 "a" 40 [ ( "a", 100 ), ( "b", 0 ), ( "c", 0 ) ]
                    |> Expect.equal [ ( "a", 40 ), ( "b", 30 ), ( "c", 30 ) ]
        , test "total を超える値は clamp(残りは 0 へ)" <|
            \_ ->
                Weights.redistribute int100 "a" 150 [ ( "a", 70 ), ( "b", 30 ) ]
                    |> Expect.equal [ ( "a", 100 ), ( "b", 0 ) ]
        , test "total=1.0 は小数 2 桁で末尾調整され、単位の合計がぴったり 1 になる" <|
            \_ ->
                let
                    result =
                        Weights.redistribute unit1 "a" 0.5 [ ( "a", 0.4 ), ( "b", 0.35 ), ( "c", 0.25 ) ]

                    unitsSum =
                        result |> List.map (\( _, v ) -> round (v * 100)) |> List.sum
                in
                ( List.map (\( k, v ) -> ( k, Weights.format unit1 v )) result, unitsSum )
                    -- b:29.17 / c:20.83 → 端数の大きい c が繰り上がる
                    |> Expect.equal ( [ ( "a", "0.5" ), ( "b", "0.29" ), ( "c", "0.21" ) ], 100 )
        , test "行の削除は残りを total へ再配分する" <|
            \_ ->
                Weights.removeKey int100 "b" [ ( "a", 60 ), ( "b", 20 ), ( "c", 20 ) ]
                    |> Expect.equal [ ( "a", 75 ), ( "c", 25 ) ]
        , test "最後の 1 行を消すと空(配分先が無い)" <|
            \_ ->
                Weights.removeKey int100 "a" [ ( "a", 100 ) ]
                    |> Expect.equal []
        , test "changedEntries は値の変わった行だけ(最小 diff)" <|
            \_ ->
                Weights.changedEntries
                    [ ( "a", 70 ), ( "b", 30 ) ]
                    [ ( "a", 70 ), ( "b", 20 ), ( "c", 10 ) ]
                    |> Expect.equal [ ( "b", 20 ), ( "c", 10 ) ]
        , test "configFrom は widget の {weights:{total}} から目盛りを決める" <|
            \_ ->
                ( Weights.configFrom (Just (E.object [ ( "weights", E.object [ ( "total", E.int 100 ) ] ) ]))
                , Weights.configFrom (Just (E.object [ ( "weights", E.object [ ( "total", E.float 1.0 ) ] ) ]))
                , Weights.configFrom (Just (E.string "slider"))
                )
                    |> Expect.equal
                        ( Just { total = 100, decimals = 0 }
                        , Just { total = 1.0, decimals = 2 }
                        , Nothing
                        )
        ]
