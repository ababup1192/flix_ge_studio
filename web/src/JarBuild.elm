module JarBuild exposing (JarBuild(..), decoder, isBuilding, label)

{-| 共通 jar(engine の headless-run が作る 1 本。プレイ・描き出し・会話のプレビューが使い回す)の様子。
サーバの GET /jar/status を読む。作っている間は画面が 3 秒おきに頼み直し、上の帯に経過秒を出す。

  - JarUnknown … まだ聞いていない・口の無いサーバ・engine に作るだけの口が無い(今までどおり flix run)
  - JarBuilding … 作っている(秒と、分かれば前回かかった秒数。目安は決め打ちしない)
  - JarReady … jar がある(押せばすぐ流れる)
  - JarFailed ログの末尾 … 作れなかった

-}

import Json.Decode as D


type JarBuild
    = JarUnknown
    | JarBuilding { secs : Int, last : Maybe Int }
    | JarReady
    | JarFailed (List String)


decoder : D.Decoder JarBuild
decoder =
    D.map4
        (\state elapsed log last ->
            case state of
                "building" ->
                    JarBuilding { secs = elapsed, last = last }

                "ready" ->
                    JarReady

                "failed" ->
                    JarFailed log

                _ ->
                    JarUnknown
        )
        (D.oneOf [ D.field "state" D.string, D.succeed "" ])
        (D.oneOf [ D.field "elapsed" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "log" (D.list D.string), D.succeed [] ])
        (D.maybe (D.field "lastBuildSeconds" D.int))


isBuilding : JarBuild -> Bool
isBuilding jar =
    case jar of
        JarBuilding _ ->
            True

        _ ->
            False


{-| 上の帯の一言(出さない時は空)。会話のプレビューの「jar を作っています」と同じ言い方にそろえる
(目安は決め打ちせず、前回の秒数が分かる時だけ言う)。
-}
label : JarBuild -> String
label jar =
    case jar of
        JarBuilding b ->
            "⏳ jar を作っています… "
                ++ String.fromInt b.secs
                ++ " 秒"
                ++ (case b.last of
                        Just n ->
                            "(前回は " ++ String.fromInt n ++ " 秒)"

                        Nothing ->
                            ""
                   )

        JarFailed _ ->
            "⚠ jar を作れませんでした"

        _ ->
            ""
