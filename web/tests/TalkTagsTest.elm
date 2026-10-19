module TalkTagsTest exposing (suite)

{-| 会話の行のタグの読み書き(表情・背景を選ぶダイアログの元)の決まり。 -}

import Expect
import Json.Decode as D
import TalkTags
import Test exposing (Test, describe, test)


script : String
script =
    String.join "\n"
        [ "title: op_desk"
        , "---"
        , "girl: よし、できた！ #face:happy"
        , "<<wait 1>>"
        , "avatar: 違う！ #face:angry #backdrop:fight"
        , "地の文だけの行"
        , "-> girl: 選ぶ #face:sad"
        , "==="
        ]


order : List String
order =
    [ "face", "backdrop" ]


suite : Test
suite =
    describe "TalkTags"
        [ test "本文の台詞・地の文・選択肢の行だけタグを付けられる(見出し・コマンド・区切りは付けない)" <|
            \_ ->
                List.map (\n -> TalkTags.lineTagsAt script n |> Maybe.map .speaker) (List.range 1 8)
                    |> Expect.equal
                        [ Nothing, Nothing, Just (Just "girl"), Nothing, Just (Just "avatar"), Just Nothing, Just (Just "girl"), Nothing ]
        , test "行の後ろのタグを順に読む" <|
            \_ ->
                TalkTags.lineTagsAt script 5
                    |> Maybe.map .tags
                    |> Expect.equal (Just [ ( "face", "angry" ), ( "backdrop", "fight" ) ])
        , test "あるタグは値だけ置き換える(台詞の字はそのまま)" <|
            \_ ->
                TalkTags.setTagOnLine order "face" "sad" "avatar: 違う！ #face:angry #backdrop:fight"
                    |> Expect.equal "avatar: 違う！ #face:sad #backdrop:fight"
        , test "無いタグは語彙の順に差し込む(表情は背景の前・語彙に無い #line: は後ろのまま)" <|
            \_ ->
                ( TalkTags.setTagOnLine order "face" "happy" "girl: 字 #backdrop:fight"
                , TalkTags.setTagOnLine order "backdrop" "board" "girl: 字 #face:sad #line:a1"
                , TalkTags.setTagOnLine order "face" "happy" "girl: 字"
                )
                    |> Expect.equal
                        ( "girl: 字 #face:happy #backdrop:fight"
                        , "girl: 字 #face:sad #backdrop:board #line:a1"
                        , "girl: 字 #face:happy"
                        )
        , test "行の後ろのコメントと字下げは残す" <|
            \_ ->
                TalkTags.setTagOnLine order "face" "sad" "    girl: 字 #face:happy // メモ"
                    |> Expect.equal "    girl: 字 #face:sad // メモ"
        , test "書き換えるのはその行だけ" <|
            \_ ->
                TalkTags.setTagAt order 3 "face" "excited" script
                    |> String.lines
                    |> Expect.equal
                        (String.lines script
                            |> List.indexedMap
                                (\i l ->
                                    if i == 2 then
                                        "girl: よし、できた！ #face:excited"

                                    else
                                        l
                                )
                        )
        , test "語彙は書いた順に読み、話し手ごとの値があればそれ・無ければ全体の値" <|
            \_ ->
                let
                    vocab =
                        D.decodeString TalkTags.vocabDecoder
                            """{"speakers":["girl","avatar"],"tags":{"face":["happy","sad"],"backdrop":["fight"],"mark":[]},
                                "speakerTags":{"avatar":{"face":["determined"]}}}"""
                            |> Result.withDefault TalkTags.emptyVocab
                in
                ( TalkTags.tagKeys vocab
                , TalkTags.choicesFor vocab (Just "avatar") "face"
                , ( TalkTags.choicesFor vocab (Just "girl") "face", TalkTags.choicesFor vocab Nothing "backdrop" )
                )
                    |> Expect.equal ( [ "face", "backdrop" ], [ "determined" ], ( [ "happy", "sad" ], [ "fight" ] ) )
        ]
