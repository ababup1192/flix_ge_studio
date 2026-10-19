module JarBuildTest exposing (suite)

{-| 共通 jar の様子(GET /jar/status)の読みと、上の帯の一言。 -}

import Expect
import JarBuild exposing (JarBuild(..))
import Json.Decode as D
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "JarBuild"
        [ test "state と elapsed・log を読む(知らない state と口の無い版は JarUnknown)" <|
            \_ ->
                List.map (D.decodeString JarBuild.decoder)
                    [ """{"state":"building","elapsed":42,"log":[]}"""
                    , """{"state":"ready","elapsed":0,"log":[]}"""
                    , """{"state":"failed","elapsed":0,"log":["MethodTooLarge"]}"""
                    , """{"state":"unsupported"}"""
                    ]
                    |> Expect.equal [ Ok (JarBuilding 42), Ok JarReady, Ok (JarFailed [ "MethodTooLarge" ]), Ok JarUnknown ]
        , test "帯の一言は作っている間と作れなかった時だけ(会話のプレビューと同じ言い方)" <|
            \_ ->
                List.map JarBuild.label [ JarBuilding 42, JarReady, JarFailed [], JarUnknown ]
                    |> Expect.equal [ "⏳ jar を作っています(1〜2 分)… 42 秒", "", "⚠ jar を作れませんでした", "" ]
        , test "聞き直すのは作っている間だけ" <|
            \_ ->
                List.map JarBuild.isBuilding [ JarBuilding 1, JarReady, JarFailed [], JarUnknown ]
                    |> Expect.equal [ True, False, False, False ]
        ]
