module JarBuild exposing (JarBuild(..), decoder, isBuilding, label)

{-| 共通 jar(engine の headless-run が作る 1 本。プレイ・描き出し・会話のプレビューが使い回す)の様子。
サーバの GET /jar/status を読む。作っている間は画面が 3 秒おきに頼み直し、上の帯に経過秒を出す。

  - JarUnknown … まだ聞いていない・口の無いサーバ・engine に作るだけの口が無い(今までどおり flix run)
  - JarBuilding 秒 … 作っている(src が変わった時だけ。1〜2 分)
  - JarReady … jar がある(押せばすぐ流れる)
  - JarFailed ログの末尾 … 作れなかった

-}

import Json.Decode as D


type JarBuild
    = JarUnknown
    | JarBuilding Int
    | JarReady
    | JarFailed (List String)


decoder : D.Decoder JarBuild
decoder =
    D.map3
        (\state elapsed log ->
            case state of
                "building" ->
                    JarBuilding elapsed

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


isBuilding : JarBuild -> Bool
isBuilding jar =
    case jar of
        JarBuilding _ ->
            True

        _ ->
            False


{-| 上の帯の一言(出さない時は空)。会話のプレビューの「jar を作っています」と同じ言い方にそろえる。 -}
label : JarBuild -> String
label jar =
    case jar of
        JarBuilding secs ->
            "⏳ jar を作っています(1〜2 分)… " ++ String.fromInt secs ++ " 秒"

        JarFailed _ ->
            "⚠ jar を作れませんでした"

        _ ->
            ""
