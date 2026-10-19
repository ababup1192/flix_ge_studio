module TalkTags exposing
    ( LineTags
    , Vocab
    , choicesFor
    , emptyVocab
    , label
    , lineTagsAt
    , setTagAt
    , setTagOnLine
    , splitTags
    , tagKeys
    , vocabDecoder
    )

{-| 会話の行のタグ(`#face:happy` のような行の後ろの印)を読む・書き換える決まり(純粋)。

選ぶダイアログ(TalkView の Picker)の元。値の一覧は台本の語彙(.yarn と同じフォルダの vocab.json。
ゲームの Talk.vocab が書く)の `tags` から取る。`speakerTags`(話し手 → タグのキー → 値)があれば
その話し手の値を先に使う(engine の語彙にはまだ無い。ゲームが書けば効く)。

書き換えは行の後ろのタグの並びだけを触り、台詞の字とコメントはそのまま残す。
タグが無い時は語彙のキーの順(vocab.json に書いた順)に差し込み、語彙に無いタグ(`#line:` など)は後ろに置いたまま。

-}

import Json.Decode as D


{-| 台本の語彙のうち、ダイアログで使う所。並びは vocab.json に書いた順。 -}
type alias Vocab =
    { tags : List ( String, List String )
    , speakerTags : List ( String, List ( String, List String ) )
    }


emptyVocab : Vocab
emptyVocab =
    { tags = [], speakerTags = [] }


{-| vocab.json を読む(話し手ごとの値 speakerTags は無くてよい)。 -}
vocabDecoder : D.Decoder Vocab
vocabDecoder =
    let
        table =
            D.keyValuePairs (D.list D.string)
    in
    D.map2 Vocab
        (D.oneOf [ D.field "tags" table, D.succeed [] ])
        (D.oneOf [ D.field "speakerTags" (D.keyValuePairs table), D.succeed [] ])


{-| 値を選べるタグのキー(語彙に値が 1 つ以上ある物。vocab.json の順)。 -}
tagKeys : Vocab -> List String
tagKeys vocab =
    vocab.tags |> List.filter (\( _, values ) -> not (List.isEmpty values)) |> List.map Tuple.first


{-| その話し手がそのキーで選べる値。話し手ごとの値があればそれ、無ければ全体の値。 -}
choicesFor : Vocab -> Maybe String -> String -> List String
choicesFor vocab speaker key =
    let
        lookup k pairs =
            pairs |> List.filter (\( name, _ ) -> name == k) |> List.head |> Maybe.map Tuple.second

        own =
            speaker
                |> Maybe.andThen (\s -> lookup s vocab.speakerTags)
                |> Maybe.andThen (lookup key)
                |> Maybe.withDefault []
    in
    if List.isEmpty own then
        lookup key vocab.tags |> Maybe.withDefault []

    else
        own


{-| 画面に出すキーの呼び名。 -}
label : String -> String
label key =
    case key of
        "face" ->
            "表情"

        "backdrop" ->
            "背景"

        _ ->
            key


{-| タグを付けられる行の話し手とタグ。 -}
type alias LineTags =
    { speaker : Maybe String, tags : List ( String, String ) }


{-| 本文の n 行目(1 始まり)がタグを付けられる行ならその中身。ノードの本文(`---` と `===` の間)の、
台詞・地の文・選択肢(`->`)の行だけ(見出し・コマンド `<<…>>`・コメント・空の行は Nothing)。
-}
lineTagsAt : String -> Int -> Maybe LineTags
lineTagsAt body n =
    let
        lines =
            String.lines body

        inBody =
            lines
                |> List.take (n - 1)
                |> List.foldl
                    (\l acc ->
                        case String.trim l of
                            "---" ->
                                True

                            "===" ->
                                False

                            _ ->
                                acc
                    )
                    False
    in
    case lines |> List.drop (n - 1) |> List.head of
        Just line ->
            let
                trimmed =
                    String.trim line
            in
            if
                not inBody
                    || trimmed
                    == ""
                    || List.any (\p -> String.startsWith p trimmed) [ "//", "<<", "===", "---" ]
            then
                Nothing

            else
                let
                    parts =
                        splitTags line
                in
                Just { speaker = speakerOf parts.head, tags = parts.tags }

        Nothing ->
            Nothing


{-| `girl: 字` の girl(選択肢の `-> girl: 字` も)。字に : が無い・名前の形でなければ地の文(Nothing)。 -}
speakerOf : String -> Maybe String
speakerOf head =
    let
        text =
            String.trim head
                |> (\t ->
                        if String.startsWith "->" t then
                            String.trim (String.dropLeft 2 t)

                        else
                            t
                   )
    in
    case String.indexes ":" text |> List.head of
        Just i ->
            let
                name =
                    String.left i text
            in
            if name /= "" && String.all (\c -> Char.isAlphaNum c || c == '_') name then
                Just name

            else
                Nothing

        Nothing ->
            Nothing


{-| 行を「タグの前の字・タグ(キー, 値)・コメント(// から後ろ。無ければ空)」に分ける。
タグは行の後ろに空白で並ぶ `#key` / `#key:value`(engine の yarn と同じく後ろにしか書けない)。
-}
splitTags : String -> { head : String, tags : List ( String, String ), comment : String }
splitTags line =
    let
        ( beforeComment, comment ) =
            case commentStart line of
                Just i ->
                    ( String.left i line, String.dropLeft i line )

                Nothing ->
                    ( line, "" )

        ( head, rawTags ) =
            peelTags (String.trimRight beforeComment) []
    in
    { head = head
    , tags =
        rawTags
            |> List.concatMap (String.split "#" >> List.filter ((/=) ""))
            |> List.map
                (\body ->
                    case String.indexes ":" body |> List.head of
                        Just i ->
                            ( String.left i body, String.dropLeft (i + 1) body )

                        Nothing ->
                            ( body, "" )
                )
    , comment = comment
    }


{-| 行のコメントの始まり(行頭か空白の後の //)。 -}
commentStart : String -> Maybe Int
commentStart line =
    String.indexes "//" line
        |> List.filter (\i -> i == 0 || String.slice (i - 1) i line == " " || String.slice (i - 1) i line == "\t")
        |> List.head


{-| 後ろから # で始まる語を剥がす(残りの字, 剥がした語の並び)。 -}
peelTags : String -> List String -> ( String, List String )
peelTags text acc =
    let
        cut =
            String.indexes " " text ++ String.indexes "\t" text |> List.maximum
    in
    case cut of
        Just i ->
            let
                word =
                    String.dropLeft (i + 1) text
            in
            if String.startsWith "#" word then
                peelTags (String.trimRight (String.left i text)) (word :: acc)

            else
                ( text, acc )

        Nothing ->
            if String.startsWith "#" text then
                ( "", text :: acc )

            else
                ( text, acc )


{-| 1 行のタグ key の値を value にする(あれば置き換え、無ければ語彙のキーの順 order で差し込む)。 -}
setTagOnLine : List String -> String -> String -> String -> String
setTagOnLine order key value line =
    let
        parts =
            splitTags line

        rank k =
            order
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, name ) -> name == k)
                |> List.head
                |> Maybe.map Tuple.first

        newRank =
            rank key |> Maybe.withDefault (List.length order)

        -- 後ろに来るべき最初のタグ(語彙で後ろのキーか、語彙に無いキー)の前に差し込む
        comesAfter ( k, _ ) =
            case rank k of
                Just r ->
                    r > newRank

                Nothing ->
                    True

        tags =
            if List.any (\( k, _ ) -> k == key) parts.tags then
                parts.tags
                    |> List.map
                        (\( k, v ) ->
                            if k == key then
                                ( k, value )

                            else
                                ( k, v )
                        )

            else
                insertBefore comesAfter ( key, value ) parts.tags

        render ( k, v ) =
            if v == "" then
                "#" ++ k

            else
                "#" ++ k ++ ":" ++ v

        tagText =
            tags |> List.map render |> String.join " "
    in
    String.join " " (List.filter ((/=) "") [ String.trimRight parts.head, tagText, parts.comment ])
        |> (\s ->
                -- 行頭の字下げは残す(head が空の時も)
                if String.trim parts.head == "" then
                    leadingSpace line ++ String.trimLeft s

                else
                    s
           )


leadingSpace : String -> String
leadingSpace line =
    String.left (String.length line - String.length (String.trimLeft line)) line


insertBefore : (a -> Bool) -> a -> List a -> List a
insertBefore isAfter item list =
    case list of
        [] ->
            [ item ]

        x :: rest ->
            if isAfter x then
                item :: x :: rest

            else
                x :: insertBefore isAfter item rest


{-| 本文の n 行目(1 始まり)のタグ key を value にする。ほかの行は触らない。 -}
setTagAt : List String -> Int -> String -> String -> String -> String
setTagAt order n key value body =
    String.lines body
        |> List.indexedMap
            (\i line ->
                if i == n - 1 then
                    setTagOnLine order key value line

                else
                    line
            )
        |> String.join "\n"
