module Summary exposing (line)

{-| エントリ 1 件の 1 行見出し。

スキーマを知らずに値そのものから作る — 盤面の行一覧も表の行も同じ見出しに
なるように、規則をここ 1 か所だけに置く。

-}

import Json.Decode as D


{-| 最初の空でないスカラ(on:"enter" 等)と、最初の文字列の列の先頭(says の 1 行目)。
どちらも無ければ空文字 — 見出しが作れないだけで、行は一覧に出す。
-}
line : D.Value -> String
line value =
    let
        pairs =
            D.decodeValue (D.keyValuePairs D.value) value |> Result.withDefault []

        firstScalar =
            pairs
                |> List.filterMap (\( _, v ) -> scalarText v)
                |> List.filter (\t -> t /= "")
                |> List.head

        firstLine =
            pairs
                |> List.filterMap
                    (\( _, v ) ->
                        D.decodeValue (D.list D.string) v
                            |> Result.toMaybe
                            |> Maybe.andThen List.head
                    )
                |> List.filter (\t -> t /= "")
                |> List.head
    in
    [ firstScalar, firstLine ]
        |> List.filterMap identity
        |> String.join " — "


scalarText : D.Value -> Maybe String
scalarText v =
    case D.decodeValue D.string v of
        Ok s ->
            Just s

        Err _ ->
            case D.decodeValue D.int v of
                Ok i ->
                    Just (String.fromInt i)

                Err _ ->
                    Nothing
