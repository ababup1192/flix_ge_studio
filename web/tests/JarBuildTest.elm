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
                    [ """{"state":"building","elapsed":42,"log":[],"lastBuildSeconds":95}"""
                    , """{"state":"ready","elapsed":0,"log":[]}"""
                    , """{"state":"failed","elapsed":0,"log":["MethodTooLarge"]}"""
                    , """{"state":"unsupported"}"""
                    ]
                    |> Expect.equal [ Ok (JarBuilding { secs = 42, last = Just 95 }), Ok JarReady, Ok (JarFailed [ "MethodTooLarge" ]), Ok JarUnknown ]
        , test "帯の一言は作っている間と作れなかった時だけ(目安は前回の秒数が分かる時だけ)" <|
            \_ ->
                List.map JarBuild.label [ JarBuilding { secs = 42, last = Just 95 }, JarBuilding { secs = 42, last = Nothing }, JarReady, JarFailed [], JarUnknown ]
                    |> Expect.equal [ "⏳ jar を作っています… 42 秒(前回は 95 秒)", "⏳ jar を作っています… 42 秒", "", "⚠ jar を作れませんでした", "" ]
        , test "聞き直すのは作っている間だけ" <|
            \_ ->
                List.map JarBuild.isBuilding [ JarBuilding { secs = 1, last = Nothing }, JarReady, JarFailed [], JarUnknown ]
                    |> Expect.equal [ True, False, False, False ]
        ]
