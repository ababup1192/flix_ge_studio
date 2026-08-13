module Draft exposing (NumberSpec, format, parse, step)

{-| 下書き(draft)の文字列を数値として確定・増減する純ロジック。

int 丸め・min/max clamp・浮動小数の桁ゴミ払いを view/update に散らすと
場所ごとに挙動がずれるので、ここへ 1 箇所に集める。

-}


type alias NumberSpec =
    { isInt : Bool
    , min : Maybe Float
    , max : Maybe Float
    , step : Maybe Float
    }


{-| 下書きを確定値へ。パース不能は Nothing — 呼び側はそのとき文書を触らない。
int 欄の小数は弾かず四捨五入で受ける(打った意図に一番近い値が残る)。
-}
parse : NumberSpec -> String -> Maybe Float
parse spec text =
    String.toFloat (String.trim text)
        |> Maybe.map (intify spec >> clampTo spec)


{-| ↑↓ の 1 回分。Shift は ×10。
空欄からは 0 起点 — 「触れば動く」の方が何も起きないより手に馴染む。
数字になっていない下書きからは動かさない(どこ起点か決めようがない)。
-}
step : NumberSpec -> { dir : Int, shift : Bool } -> String -> Maybe Float
step spec arg text =
    let
        base =
            if String.trim text == "" then
                Just 0

            else
                String.toFloat (String.trim text)

        amount =
            stepSize spec
                * toFloat arg.dir
                * (if arg.shift then
                    10

                   else
                    1
                  )
    in
    base
        |> Maybe.map
            (\b ->
                -- 桁丸め → clamp の順。先に clamp すると 0.95 のような境界値が
                -- 桁丸めで min/max の外へ押し戻される
                (b + amount)
                    |> roundToStepDigits spec
                    |> intify spec
                    |> clampTo spec
            )


{-| 表示用の文字列。int を "3.0" と見せない。 -}
format : NumberSpec -> Float -> String
format spec v =
    if spec.isInt then
        String.fromInt (round v)

    else
        String.fromFloat v


{-| 増減の 1 目盛り。スキーマの step が正、無ければ int=1 / float=0.1。 -}
stepSize : NumberSpec -> Float
stepSize spec =
    case spec.step of
        Just s ->
            s

        Nothing ->
            if spec.isInt then
                1

            else
                0.1


{-| 0.2 + 0.1 = 0.30000000000000004 の桁ゴミを step の小数桁数で払う。 -}
roundToStepDigits : NumberSpec -> Float -> Float
roundToStepDigits spec v =
    let
        digits =
            stepSize spec
                |> String.fromFloat
                |> String.split "."
                |> List.drop 1
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 0

        factor =
            toFloat (10 ^ digits)
    in
    toFloat (round (v * factor)) / factor


intify : NumberSpec -> Float -> Float
intify spec v =
    if spec.isInt then
        toFloat (round v)

    else
        v


clampTo : NumberSpec -> Float -> Float
clampTo spec v =
    v
        |> boundBy max spec.min
        |> boundBy min spec.max


boundBy : (Float -> Float -> Float) -> Maybe Float -> Float -> Float
boundBy pick bound v =
    case bound of
        Just b ->
            pick b v

        Nothing ->
            v
