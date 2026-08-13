module Effect exposing
    ( Effect(..)
    , batch
    , encodeRequest
    , none
    , perform
    )

{-| update が返す「外へ頼みたい事」の自前データ。

Cmd を直接返すとテストから中身が見えない(POST を発行したかを検査できない)ので、
封筒 {id, kind, payload} を値のまま持ち、Cmd への変換は端の 1 関数(perform)に
閉じ込める。id の採番は update 側(純粋)で済んでいる。

-}

import Json.Encode as E
import Process
import Task


type Effect
    = SendApi { id : Int, kind : String, payload : E.Value }
      -- トースト通知の消灯予約。seq と message は「その通知がまだ最新の物か」を
      -- 発火時に確かめる照合キー(古いタイマーが新しい通知を消さない)
    | ExpireNotice { seq : Int, message : String, afterMs : Float }
      -- ライブ反映の自動保存予約。編集のたびに seq が進むので、発火時に
      -- 最新の予約だけが保存する(debounce)
    | Autosave { seq : Int, afterMs : Float }
      -- 焼き上がりの確かめのように「少し置いてからもう一度」の予約。
      -- seq は古い予約が新しい物を追い越さないための照合キー
    | Delayed { seq : Int, afterMs : Float }
      -- 横断検索の打鍵デバウンス。seq は「その予約がまだ最新か」の照合キーで、
      -- 打っている間の予約は次々に失効する(最後の 1 回だけがサーバへ行く)
    | SearchDebounce { seq : Int, afterMs : Float }
      -- 行を選ぶたびに 1 枚焼きを頼まないための間(連打の最後だけ焼く)
    | FramePeek { seq : Int, afterMs : Float }
      -- 焼き係が立ち上がるのを訊き直す間(初回のコンパイルは数分かかる)
    | WakePoll { seq : Int, afterMs : Float }
    | NoFx
    | Batch (List Effect)


none : Effect
none =
    NoFx


batch : List Effect -> Effect
batch =
    Batch


{-| 封筒の JSON 化。perform とテストの模擬ポートが同じ形を共有する —
二重に組み立てると片方だけ形がずれても気づけないため。
-}
encodeRequest : { id : Int, kind : String, payload : E.Value } -> E.Value
encodeRequest req =
    E.object
        [ ( "id", E.int req.id )
        , ( "kind", E.string req.kind )
        , ( "payload", req.payload )
        ]


{-| Effect → Cmd 変換の端の 1 関数。port はアプリ(Main)しか持てないので
引数で受ける — ここに port を書くと Effect がテストから import できなくなる。
noticeExpired も Msg 構築子を知らないここでは作れないので同様に受ける。
-}
perform :
    { toPort : E.Value -> Cmd msg
    , noticeExpired : { seq : Int, message : String } -> msg
    , autosaveFired : Int -> msg
    , delayFired : Int -> msg
    , searchDebounced : Int -> msg
    , framePeeked : Int -> msg
    , wakePolled : Int -> msg
    }
    -> Effect
    -> Cmd msg
perform caps effect =
    case effect of
        SendApi req ->
            caps.toPort (encodeRequest req)

        ExpireNotice info ->
            Process.sleep info.afterMs
                |> Task.perform (\_ -> caps.noticeExpired { seq = info.seq, message = info.message })

        Autosave info ->
            Process.sleep info.afterMs
                |> Task.perform (\_ -> caps.autosaveFired info.seq)

        Delayed info ->
            Process.sleep info.afterMs
                |> Task.perform (\_ -> caps.delayFired info.seq)

        SearchDebounce info ->
            Process.sleep info.afterMs
                |> Task.perform (\_ -> caps.searchDebounced info.seq)

        FramePeek info ->
            Process.sleep info.afterMs
                |> Task.perform (\_ -> caps.framePeeked info.seq)

        WakePoll info ->
            Process.sleep info.afterMs
                |> Task.perform (\_ -> caps.wakePolled info.seq)

        NoFx ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map (perform caps) effects)
