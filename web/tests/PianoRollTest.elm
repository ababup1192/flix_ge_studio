module PianoRollTest exposing (suite)

{-| 譜を読む規則(純ロジック)。

固定するのは「どれを音符の列と見なすか」「どの音域に収めるか」「何拍あるか」。
棒の色や線の濃さは検査しない(見た目は目視の仕事)。

-}

import Expect
import Json.Decode as D
import Json.Encode as E
import PianoRoll
import Test exposing (Test, describe, test)


entry : String -> D.Value
entry text =
    D.decodeString D.value text |> Result.withDefault E.null


{-| kaidan の tunes.moonlight と同じ骨格。 -}
tune : D.Value
tune =
    entry """
{
  "looping": true,
  "decay": 1.0,
  "notes": [
    { "at": 0.0, "len": 4.0, "midi": 37, "gain": 0.6 },
    { "at": 0.0, "len": 1.2, "midi": 56, "gain": 0.34 },
    { "at": 2.0, "len": 2.0, "midi": 61 }
  ]
}
"""


suite : Test
suite =
    describe "PianoRoll — 譜を読む"
        [ test "音符の列は形で見つける(キー名は問わない・数の at/len/midi が揃う列)" <|
            \_ ->
                PianoRoll.notesIn tune
                    |> Maybe.map (\roll -> ( roll.field, List.map .midi roll.notes ))
                    |> Expect.equal (Just ( "notes", [ 37, 56, 61 ] ))
        , test "gain が無い音符は 1(強さの宣言が無い譜も読める)" <|
            \_ ->
                PianoRoll.notesIn tune
                    |> Maybe.map (\roll -> roll.notes |> List.map .gain)
                    |> Expect.equal (Just [ 0.6, 0.34, 1 ])
        , test "並び順を index に持つ(押した音符の JSON 行を指すため)" <|
            \_ ->
                PianoRoll.notesIn tune
                    |> Maybe.map (\roll -> roll.notes |> List.map .index)
                    |> Expect.equal (Just [ 0, 1, 2 ])
        , test "形の合わない列は音符と見なさない(数でない・キーが欠ける・空)" <|
            \_ ->
                [ PianoRoll.notesIn (entry """{ "says": [ "こんばんは" ] }""")
                , PianoRoll.notesIn (entry """{ "rows": [ { "at": 0, "len": 1 } ] }""")
                , PianoRoll.notesIn (entry """{ "notes": [] }""")
                ]
                    |> List.map (Maybe.map .field)
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "音域は実データの上下 ± 2 半音(端の音が枠に貼り付かない)" <|
            \_ ->
                PianoRoll.notesIn tune
                    |> Maybe.map (\roll -> PianoRoll.rangeOf roll.notes)
                    |> Expect.equal (Just { lo = 35, hi = 63 })
        , test "音符が無ければ既定の音域(空の譜でも格子は出る)" <|
            \_ ->
                PianoRoll.rangeOf [] |> Expect.equal { lo = 60, hi = 72 }
        , test "譜の長さは最後の音符が鳴り終わるまでの拍" <|
            \_ ->
                PianoRoll.notesIn tune
                    |> Maybe.map (\roll -> PianoRoll.beats roll.notes)
                    |> Expect.equal (Just 4)
        , test "音名は 60 = C4(目盛りの読み)" <|
            \_ ->
                List.map PianoRoll.noteName [ 60, 61, 37, 72 ]
                    |> Expect.equal [ "C4", "C#4", "C#2", "C5" ]
        ]
