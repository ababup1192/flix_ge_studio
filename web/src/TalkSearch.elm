module TalkSearch exposing
    ( Hit
    , across
    , firstFrom
    , inText
    , maxHits
    , step
    )

{-| 会話のシナリオ(.yarn)の中を字で探す決まり(純粋)。

開いているファイルの中を探す(⌘F)のと、全部の .yarn を横断して「どのファイルのどのノードの何行目に
その台詞があるか」を探すのは、同じ 1 つの探し方(inText)を使う。大文字と小文字は区別しない。

見つかった所は行(1 始まり)と、ファイルの頭からの文字位置(from・to。本文の欄の選択にそのまま渡せる)と、
その行が入っているノード(`title:` から `===` まで。どのノードの外なら Nothing)で持つ。

-}


{-| 見つかった 1 か所。 -}
type alias Hit =
    { file : String
    , line : Int
    , node : Maybe String

    -- その行の字(前後の空白は落とす)
    , text : String

    -- ファイルの頭からの文字位置(from から to の手前まで)
    , from : Int
    , to : Int
    }


{-| 一度に出す数の上限(短い字で全部のファイルを探しても画面が固まらないように)。 -}
maxHits : Int
maxHits =
    500


{-| 1 つのファイルの本文の中を探す。空の字では探さない。 -}
inText : String -> String -> String -> List Hit
inText query file body =
    let
        needle =
            String.toLower query

        width =
            String.length query

        scan lines lineNo offset node acc =
            case lines of
                [] ->
                    List.reverse acc

                line :: rest ->
                    let
                        trimmed =
                            String.trim line

                        here =
                            if String.startsWith "title:" trimmed then
                                Just (String.trim (String.dropLeft 6 trimmed))

                            else
                                node

                        hits =
                            String.indexes needle (String.toLower line)
                                |> List.map
                                    (\i ->
                                        { file = file
                                        , line = lineNo
                                        , node = here
                                        , text = trimmed
                                        , from = offset + i
                                        , to = offset + i + width
                                        }
                                    )

                        next =
                            if trimmed == "===" then
                                Nothing

                            else
                                here
                    in
                    scan rest (lineNo + 1) (offset + String.length line + 1) next (List.reverse hits ++ acc)
    in
    if String.isEmpty needle then
        []

    else
        scan (String.lines body) 1 0 Nothing []


{-| 何本ものファイルを横断して探す(並びはファイルの並びのまま。上限 maxHits まで)。 -}
across : String -> List ( String, String ) -> List Hit
across query files =
    files
        |> List.concatMap (\( file, body ) -> inText query file body)
        |> List.take maxHits


{-| カーソルの行から下で最初に見つかった所の番号(下に無ければ先頭へ回る)。 -}
firstFrom : Int -> List Hit -> Int
firstFrom line hits =
    hits
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, h ) -> h.line >= line)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


{-| 次(+1)・前(-1)の番号。端では反対の端へ回る。 -}
step : Int -> Int -> List Hit -> Int
step delta at hits =
    let
        n =
            List.length hits
    in
    if n == 0 then
        0

    else
        modBy n (at + delta)
