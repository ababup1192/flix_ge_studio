module WaveformTest exposing (suite)

{-| 波形の帯の決めごと(純ロジック)。

固定するのは「押した / なぞった結果、どこから何秒鳴らすか」だけ。
線の見た目や色は検査しない(演出は目視の仕事)。

-}

import Expect
import Test exposing (Test, describe, test)
import Waveform


suite : Test
suite =
    describe "Waveform — 再生位置と範囲選択"
        [ test "クリックだけでは範囲を作らない(シークのたびに選択が付いて回らない)" <|
            \_ ->
                let
                    model =
                        Waveform.init |> Waveform.pressAt 0.5 |> Waveform.release
                in
                ( Waveform.selection model, Waveform.playFrom model, Waveform.playSpan model )
                    |> Expect.equal ( Nothing, 0.5, Nothing )
        , test "なぞると範囲になり、▶ はその範囲だけ(頭と長さ)" <|
            \_ ->
                let
                    model =
                        Waveform.init |> Waveform.pressAt 0.5 |> Waveform.dragTo 1.5 |> Waveform.release
                in
                ( Waveform.selection model, Waveform.playFrom model, Waveform.playSpan model )
                    |> Expect.equal ( Just { from = 0.5, to = 1.5 }, 0.5, Just 1 )
        , test "右から左へなぞっても範囲は前後が揃う" <|
            \_ ->
                Waveform.init
                    |> Waveform.pressAt 1.5
                    |> Waveform.dragTo 0.5
                    |> Waveform.release
                    |> Waveform.selection
                    |> Expect.equal (Just { from = 0.5, to = 1.5 })
        , test "手の震え(1ms 未満)は範囲にしない" <|
            \_ ->
                Waveform.init
                    |> Waveform.pressAt 0.5
                    |> Waveform.dragTo 0.5005
                    |> Waveform.release
                    |> Waveform.selection
                    |> Expect.equal Nothing
        , test "選択を解くと全体に戻る(開始位置は残る)" <|
            \_ ->
                let
                    model =
                        Waveform.init
                            |> Waveform.pressAt 0.5
                            |> Waveform.dragTo 1.5
                            |> Waveform.release
                            |> Waveform.clear
                in
                ( Waveform.selection model, Waveform.playFrom model, Waveform.playSpan model )
                    |> Expect.equal ( Nothing, 0.5, Nothing )
        , test "押し直しは前の範囲を捨てる(選び直しが素直に効く)" <|
            \_ ->
                Waveform.init
                    |> Waveform.pressAt 0.5
                    |> Waveform.dragTo 1.5
                    |> Waveform.release
                    |> Waveform.pressAt 0.2
                    |> Waveform.selection
                    |> Expect.equal Nothing
        , test "負の秒は 0 に丸める(器の左端より外を押しても頭から)" <|
            \_ ->
                Waveform.init |> Waveform.pressAt -0.3 |> Waveform.playFrom |> Expect.equal 0
        ]
