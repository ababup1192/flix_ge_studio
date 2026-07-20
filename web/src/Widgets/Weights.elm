module Widgets.Weights exposing
    ( Config
    , changedEntries
    , configFrom
    , format
    , redistribute
    , removeKey
    )

{-| 合計固定の複数連動スライダー(weights)の純ロジック。

1 本を動かすと残りが比例配分されて合計が常に total に保たれる。
浮動小数のまま足すと丸め誤差で合計が揺れるので、計算は最小目盛りの
整数(units)へ持ち上げてから最大剰余法で配り切る — 合計の同値は
「たまたま」でなく構造で保証する。

-}

import Json.Decode as D


type alias Config =
    { total : Float

    -- 値の小数桁数。0 = 整数の重み(total 100 等)・2 = 確率の形(total 1.0)
    , decimals : Int
    }


{-| スキーマの widget 値({"weights": {"total": 100}})から設定を読む。
形が違えば Nothing — 呼び側は生 JSON 行へ倒す(編集できない項目を作らない)。
-}
configFrom : Maybe D.Value -> Maybe Config
configFrom widget =
    widget
        |> Maybe.andThen
            (\w ->
                D.decodeValue (D.field "weights" (D.field "total" D.float)) w
                    |> Result.toMaybe
            )
        |> Maybe.map (\total -> { total = total, decimals = decimalsFor total })


{-| total 1.0(確率)だけ整数目盛りでは 0/1 しか作れないので小数 2 桁にする。
2 以上の整数 total は書き手が「整数の重み」を意図したと読む。
-}
decimalsFor : Float -> Int
decimalsFor total =
    if total == toFloat (round total) && total > 1 then
        0

    else
        2


{-| key を value へ動かし、残りを比例配分した結果(順序は元のまま)。
key が居ない時は触らない。1 行だけなら常に total(動かしようがない)。
-}
redistribute : Config -> String -> Float -> List ( String, Float ) -> List ( String, Float )
redistribute config key value entries =
    if not (List.any (\( k, _ ) -> k == key) entries) then
        entries

    else
        let
            factor =
                toFloat (10 ^ config.decimals)

            totalUnits =
                round (config.total * factor)

            movedUnits =
                clamp 0 totalUnits (round (value * factor))

            otherWeights =
                entries
                    |> List.filter (\( k, _ ) -> k /= key)
                    |> List.map Tuple.second

            otherUnits =
                apportion (totalUnits - movedUnits) otherWeights
        in
        if List.isEmpty otherWeights then
            [ ( key, config.total ) ]

        else
            rebuild factor key movedUnits entries otherUnits


{-| key の行を消し、残りを total へ再配分した結果。 -}
removeKey : Config -> String -> List ( String, Float ) -> List ( String, Float )
removeKey config key entries =
    let
        factor =
            toFloat (10 ^ config.decimals)

        rest =
            List.filter (\( k, _ ) -> k /= key) entries
    in
    if List.isEmpty rest then
        []

    else
        List.map2 (\( k, _ ) units -> ( k, toFloat units / factor ))
            rest
            (apportion (round (config.total * factor)) (List.map Tuple.second rest))


{-| 書き戻しは変わった行だけ(最小 diff)。新しい側に居る行のうち、
旧値と一致しない物を新しい側の順で返す。
-}
changedEntries : List ( String, Float ) -> List ( String, Float ) -> List ( String, Float )
changedEntries old new =
    new
        |> List.filter
            (\( k, v ) ->
                (old |> List.filter (\( ok, _ ) -> ok == k) |> List.head |> Maybe.map Tuple.second)
                    /= Just v
            )


{-| 表示用の文字列。整数の重みを "70.0" と見せない。 -}
format : Config -> Float -> String
format config v =
    if config.decimals == 0 then
        String.fromInt (round v)

    else
        let
            factor =
                toFloat (10 ^ config.decimals)
        in
        String.fromFloat (toFloat (round (v * factor)) / factor)



-- 内部: 最大剰余法


{-| unitsTotal 個の目盛りを weights の比で配り切る(合計は必ず unitsTotal)。
weights が全部 0(比が決められない)の時だけ均等割へ倒す。
-}
apportion : Int -> List Float -> List Int
apportion unitsTotal weights =
    let
        positive =
            List.map (Basics.max 0) weights

        weightSum =
            List.sum positive

        ratios =
            if weightSum > 0 then
                List.map (\w -> w / weightSum) positive

            else
                List.map (\_ -> 1 / toFloat (Basics.max 1 (List.length positive))) positive

        shares =
            List.map (\r -> r * toFloat unitsTotal) ratios

        floors =
            List.map floor shares

        leftover =
            unitsTotal - List.sum floors

        -- 端数の大きい行から 1 目盛りずつ足す(同率は先に書いた行が勝つ)
        winners =
            List.map2 (\share fl -> share - toFloat fl) shares floors
                |> List.indexedMap Tuple.pair
                |> List.sortBy (\( i, frac ) -> ( -frac, i ))
                |> List.take (Basics.max 0 leftover)
                |> List.map Tuple.first
    in
    floors
        |> List.indexedMap
            (\i fl ->
                if List.member i winners then
                    fl + 1

                else
                    fl
            )


{-| 元の順序を保ったまま、動かした行は movedUnits・他の行は otherUnits を順に嵌める。 -}
rebuild : Float -> String -> Int -> List ( String, Float ) -> List Int -> List ( String, Float )
rebuild factor key movedUnits entries otherUnits =
    entries
        |> List.foldl
            (\( k, _ ) ( acc, rest ) ->
                if k == key then
                    ( acc ++ [ ( k, toFloat movedUnits / factor ) ], rest )

                else
                    case rest of
                        u :: more ->
                            ( acc ++ [ ( k, toFloat u / factor ) ], more )

                        [] ->
                            -- apportion は他の行と同数を返すのでここには来ない
                            ( acc ++ [ ( k, 0 ) ], [] )
            )
            ( [], otherUnits )
        |> Tuple.first
