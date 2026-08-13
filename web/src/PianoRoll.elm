module PianoRoll exposing
    ( Handlers
    , Note
    , Range
    , beats
    , noteName
    , notesIn
    , rangeOf
    , view
    )

{-| 音符の並びを、時間 × 高さの格子で読む(読み取り専用)。

出す条件は名前でなく**形**で決める: 一覧のエントリが「数の at / len / midi を持つ
オブジェクトの列」を持っていれば、それは並べて読める音符の列だと見なす。
どのゲームの、どんなキー名でも同じ規則で当たる。

ここは読むだけ — 音符を動かす・足す・消すは持たない。編集は左のフォームの
担当で、こちらは「楽譜として見え、押した所へ飛べる」ことだけを引き受ける。

拍から秒への換算は 1 か所(beats → 秒 = 60 / テンポ)。焼き上がりの波形と
同じ秒の物差しに乗るので、上下に並べた時に同じ場所が同じ時間を指す。

-}

import Html exposing (Html, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


{-| 音符 1 つ。at / len は拍、midi は高さ(60 = 中央のド)、gain は強さ(0〜1)。
index は文書の中の並び順 — 押した音符の JSON 行を指すのに使う。
-}
type alias Note =
    { index : Int
    , at : Float
    , len : Float
    , midi : Int
    , gain : Float
    }


{-| 出す音域(半音の上下端)。 -}
type alias Range =
    { lo : Int, hi : Int }


{-| エントリの中から「音符の列」を探す。最初に見つかった 1 つだけを返す
(キー名は問わない — 形が合う物が音符の列)。呼び側は field を JSON の道しるべに使う。
-}
notesIn : D.Value -> Maybe { field : String, notes : List Note }
notesIn entry =
    D.decodeValue (D.keyValuePairs D.value) entry
        |> Result.withDefault []
        |> List.filterMap
            (\( key, value ) ->
                case D.decodeValue (D.list noteDecoder) value of
                    Ok notes ->
                        if List.isEmpty notes then
                            Nothing

                        else
                            Just { field = key, notes = List.indexedMap (\i note -> { note | index = i }) notes }

                    Err _ ->
                        Nothing
            )
        |> List.head


{-| at / len / midi が数で揃っている物だけを音符と見なす。
gain は無ければ 1(強さの宣言が無い譜でも読める)。
-}
noteDecoder : D.Decoder Note
noteDecoder =
    D.map4 (Note 0)
        (D.field "at" D.float)
        (D.field "len" D.float)
        (D.field "midi" D.int)
        (D.oneOf [ D.field "gain" D.float, D.succeed 1 ])


{-| 出す音域。実際に使われている高さの上下に 2 半音の余白 —
端の音が枠に貼り付くと、線なのか音符なのか読めないため。
-}
rangeOf : List Note -> Range
rangeOf notes =
    case List.map .midi notes of
        [] ->
            { lo = 60, hi = 72 }

        first :: rest ->
            let
                lo =
                    List.foldl min first rest

                hi =
                    List.foldl max first rest
            in
            { lo = lo - 2, hi = hi + 2 }


{-| 譜の長さ(拍)。最後の音符が鳴り終わるまで。 -}
beats : List Note -> Float
beats notes =
    notes
        |> List.map (\note -> note.at + note.len)
        |> List.maximum
        |> Maybe.withDefault 0


{-| midi 番号の音名(60 = C4)。目盛りに薄く添えるだけなので、
半音は # 側の名前ひとつに決める。
-}
noteName : Int -> String
noteName midi =
    let
        names =
            [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

        step =
            modBy 12 midi

        octave =
            midi // 12 - 1
    in
    (names |> List.drop step |> List.head |> Maybe.withDefault "?")
        ++ String.fromInt octave


type alias Handlers msg =
    { onSeek : Float -> msg
    , onNote : Note -> msg
    }


{-| 譜 1 枚。playheadKey は再生位置の線の照合キー(波形と同じ物を渡すと、
1 回の再生で両方の線が動く)。
-}
view :
    Handlers msg
    -> { key : String, bpm : Float, notes : List Note, selected : Maybe Int }
    -> Html msg
view handlers spec =
    let
        span_ =
            beats spec.notes

        range =
            rangeOf spec.notes

        rows =
            max 1 (range.hi - range.lo + 1)

        secondsPerBeat =
            60 / max 1 spec.bpm
    in
    div [ HA.class "piano-roll flex w-full gap-1" ]
        [ viewKeys range
        , div
            [ HA.class "roll-grid relative h-40 min-w-0 flex-1 cursor-text overflow-hidden rounded border border-edge bg-well"
            , HE.on "click"
                (D.map2 (\x w -> handlers.onSeek (x / max 1 w * span_ * secondsPerBeat))
                    (D.field "offsetX" D.float)
                    (D.at [ "currentTarget", "clientWidth" ] D.float)
                )
            ]
            (viewBeatLines span_
                ++ List.map (viewNote handlers spec.selected span_ range rows) spec.notes
                ++ [ div
                        [ HA.class "waveform-playhead"
                        , HA.attribute "data-playhead" spec.key
                        , HA.style "display" "none"
                        ]
                        []
                   ]
            )
        ]


{-| 左端の音名。上が高い音(五線譜と同じ向き)。数が多い時は端だけに間引く。 -}
viewKeys : Range -> Html msg
viewKeys range =
    div [ HA.class "roll-keys flex h-40 w-8 shrink-0 flex-col justify-between py-0.5 text-right font-mono text-[9px] text-ink-faint" ]
        [ span [] [ text (noteName range.hi) ]
        , span [] [ text (noteName ((range.lo + range.hi) // 2)) ]
        , span [] [ text (noteName range.lo) ]
        ]


{-| 拍の線(薄)と 4 拍ごとの小節線(やや濃)。拍が多すぎる譜では拍線を省く
(線で埋まって音符が読めなくなるより、小節だけの方が読める)。
-}
viewBeatLines : Float -> List (Html msg)
viewBeatLines span_ =
    let
        total =
            ceiling span_

        showBeats =
            total <= 64
    in
    List.range 0 total
        |> List.filterMap
            (\beat ->
                let
                    isBar =
                        modBy 4 beat == 0
                in
                if not isBar && not showBeats then
                    Nothing

                else
                    Just
                        (div
                            [ HA.classList
                                [ ( "roll-line pointer-events-none absolute top-0 bottom-0 w-px", True )
                                , ( "bg-edge", isBar )
                                , ( "bg-edge/40", not isBar )
                                ]
                            , HA.style "left" (percent span_ (toFloat beat))
                            ]
                            []
                        )
            )


viewNote : Handlers msg -> Maybe Int -> Float -> Range -> Int -> Note -> Html msg
viewNote handlers selected span_ range rows note =
    let
        height =
            100 / toFloat rows

        top =
            toFloat (range.hi - note.midi) * height
    in
    div
        [ HA.classList
            [ ( "roll-note absolute cursor-pointer rounded-[1px]", True )
            , ( "bg-accent ring-1 ring-ok", selected == Just note.index )
            , ( "bg-accent", selected /= Just note.index )
            ]
        , HA.style "left" (percent span_ note.at)
        , HA.style "width" (percent span_ (max 0.05 note.len))
        , HA.style "top" (String.fromFloat top ++ "%")
        , HA.style "height" (String.fromFloat (max 2 (height - 1)) ++ "%")

        -- 強さは濃さで(選んだ音符は濃さに関わらず見えるよう縁で示す)
        , HA.style "opacity" (String.fromFloat (0.35 + 0.65 * clamp 0 1 note.gain))
        , HA.title (noteName note.midi ++ " · " ++ String.fromFloat note.at ++ " 拍から " ++ String.fromFloat note.len ++ " 拍")
        , HE.stopPropagationOn "click" (D.succeed ( handlers.onNote note, True ))
        ]
        []


percent : Float -> Float -> String
percent span_ value =
    if span_ <= 0 then
        "0%"

    else
        String.fromFloat (clamp 0 100 (value / span_ * 100)) ++ "%"
