module TalkView exposing
    ( Boot(..)
    , Headless(..)
    , Model
    , Msg(..)
    , Node
    , Out(..)
    , Pane(..)
    , Pick
    , Picker(..)
    , Problem
    , Request(..)
    , Shot
    , ShotAnswer(..)
    , Shown(..)
    , Target
    , bootDecoder
    , buildDecoder
    , busyLabel
    , filesDecoder
    , gotBuild
    , gotFile
    , gotFiles
    , gotGoto
    , gotShot
    , gotTree
    , gotVocab
    , init
    , lineAt
    , retryFired
    , shotAnswerDecoder
    , targetAt
    , update
    , view
    , varsOf
    , vocabPathOf
    )

{-| 会話の画面 — 左で会話のシナリオ(.yarn)を書き、右にカーソルの行の本物のゲームの画面を出す。

Studio は自分で描かない。右の絵は窓なしのゲーム(ゲーム自身のコード)が `start talk node=… line=…`
から組んで撮った 1 枚(立ち絵・表情・窓・後ろの場面が実機と同じ)。.yarn を読むのも
ゲームの bin/fge yarn で、誤りは行番号と近い名前の案つきでその行の右に出す。

プレビューの頼みと見せ方は直交する 2 つの型にしてある。

  - Request … 頼み。Idle / InFlight(1 つ頼んでいる)/ InFlightThen(頼んでいる間にカーソルが
    動いた。答えが来たら最後の行だけ頼む — 打鍵ごとに窓なしのゲームへ積まない)
  - Shown … 見せ方。Blank / Shown(今の行の絵)/ Held(前の絵のまま。理由つき — 台本が壊れている・
    ゲームを起こしている最中)

窓なしのゲームの様子は Headless の 1 つで持つ(旗を重ねない)。起こしている間・長く起きない間は
3 秒おきに頼み直し、落ちた時は頼み直さずに理由とログの末尾と「もう一度起こす」を出す。

答えの Target が今の頼みと違えば捨てる(ファイルを開き直した後に、前のファイルの答えが届いても混ぜない)。

保存で組み直し(dialogue.json が書き変わる)、今の行を撮り直す。打っている途中の壊れた台本は
ok:false で返り、前の絵を Held で出したまま誤りの行を示す。

行のタグ(表情 #face・背景 #backdrop など)は、カーソルの行のチップか ⌘E(表情)/ ⌘B(背景)で
選ぶダイアログ(Picker。開閉と選択中を 1 つで持つ)からも書ける。値は台本の語彙(vocab.json)から取り、
選ぶとその行のタグを書き換えて保存し、撮り直す。打って書くやり方はそのまま。

左の面は Pane の 1 つで持つ: 台本(ScriptPane)か、ノードの図(GraphPane。箱と jump・選択肢の矢印。
押すとその台本の行へ戻る)。図は組んだ木(dialogue.json)から描く。

変数の一覧は組んだ木(dialogue.json)の `<<declare>>` を並べ、値を変えるとその値で撮り直す
(engine 0.51 の始まりの引数 `$名前=値`。変えた値は overrides に持ち、初めの値に戻すと消える)。

「この行へ飛ばす」だけが起動中のゲームを動かす(遊んでいる World をその行の場面へ差し替える)。

サーバ往復は Main の仕事(Out で頼む)。

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, img, input, option, select, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import TalkGraph
import TalkTags
import TalkTree


{-| 撮る場面: ノードと .yarn の行(1 始まり)。 -}
type alias Target =
    { node : String, line : Int }


{-| 撮れた 1 枚(PNG の base64)。 -}
type alias Shot =
    { target : Target, png : String }


type Request
    = Idle
    | InFlight Target
    | InFlightThen Target Target


type Shown
    = Blank
    | Shown Shot
    | Held Shot String


type alias Node =
    { title : String, file : String, line : Int }


type alias Problem =
    { file : String, line : Int, msg : String }


{-| 撮った答え。

  - ShotStarting … 窓なしのゲームを起こしている・待っている(少し後に頼み直す。段つき)
  - ShotStalled … 起こしてから長く ready にならない(頼み直しは続ける。ログの末尾つき)
  - ShotCrashed … ready にならずに落ちた(サーバは続けて起こさない。ログの末尾つき)
  - ShotFailed … それ以外の撮れなかった理由(台本・繋がらない等)

-}
type ShotAnswer
    = ShotOk String
    | ShotStarting String Boot
    | ShotStalled String (List String)
    | ShotCrashed String (List String)
    | ShotFailed String


{-| 起こしている 1 本の段(サーバの stage と elapsed。秒は起こしてからの経過)。

  - BootWaiting … 段の分からない待ち(ゲームが起動の途中・古いサーバ・押した直後)
  - BootBuildingJar 秒 … jar を作っている(src が変わった時だけ。1〜2 分)
  - BootStartingGame 秒 … jar はあり、JVM を起こして最初の 1 枚を待っている(数秒)

-}
type Boot
    = BootWaiting
    | BootBuildingJar Int
    | BootStartingGame Int


{-| 窓なしのゲームの様子(画面が知っている限り)。

  - HeadlessOk … 撮れた・まだ何も聞いていない
  - HeadlessBooting … 起こしている(頼み直し中。段つき)
  - HeadlessStalled … 長く起きない(頼み直し中。ログの末尾と「もう一度起こす」を出す)
  - HeadlessCrashed … 落ちた(頼み直さない。ログの末尾と「もう一度起こす」を出す)

-}
type Headless
    = HeadlessOk
    | HeadlessBooting Boot
    | HeadlessStalled (List String)
    | HeadlessCrashed (List String)


{-| ダイアログで選ぶ所: 本文の行(1 始まり)とタグのキー。 -}
type alias Pick =
    { line : Int, key : String }


{-| 行のタグを選ぶダイアログ(開閉と選択中を旗で重ねない)。

  - PickerClosed … 閉じている
  - PickerOpen pick 値の並び 選択中の位置 … 開いている(↑↓ で動かし Enter で選ぶ・Esc で閉じる)

-}
type Picker
    = PickerClosed
    | PickerOpen Pick (List String) Int


{-| 左の面: 台本を書く / ノードの図を見る。 -}
type Pane
    = ScriptPane
    | GraphPane


type alias Model =
    { files : List String

    -- ゲームが窓なしの入口(E2e.serveHeadless)を持っているか
    , hasEntry : Bool
    , path : String
    , text : String

    -- 最後に保存した中身(未保存の印の元)
    , saved : String
    , nodes : List Node
    , problems : List Problem
    , notes : List Problem

    -- カーソルの行(1 始まり)
    , cursor : Int
    , request : Request
    , shown : Shown

    -- 窓なしのゲームの様子(頼み直すか・ボタンを出すかの元)
    , headless : Headless

    -- 絵が無い時に出す一言(Blank の理由)・保存や飛ばした結果の一言
    , note : String
    , status : String

    -- 頼み直しの予約の照合キー(古い予約は捨てる)
    , retrySeq : Int

    -- 保存して組んでいる最中の中身(答えが来たら saved になる)
    , saving : Maybe String

    -- 台本の語彙(.yarn と同じフォルダの vocab.json。まだ読めていなければ Nothing)
    , vocab : Maybe TalkTags.Vocab
    , picker : Picker

    -- 組んだ木(dialogue.json。変数の一覧とノードの図の元)
    , tree : TalkTree.Tree

    -- プレビューで値を変えた変数(名前 → 値の字。初めの値と同じなら持たない)
    , overrides : Dict String String
    , pane : Pane
    }


init : Model
init =
    { files = []
    , hasEntry = True
    , path = ""
    , text = ""
    , saved = ""
    , nodes = []
    , problems = []
    , notes = []
    , cursor = 1
    , request = Idle
    , shown = Blank
    , headless = HeadlessOk
    , note = ""
    , status = ""
    , retrySeq = 0
    , saving = Nothing
    , vocab = Nothing
    , picker = PickerClosed
    , tree = TalkTree.empty
    , overrides = Dict.empty
    , pane = ScriptPane
    }


type Msg
    = FilePicked String
    | TextEdited String Int
    | CaretMoved String Int
    | SaveClicked
    | NodeClicked Node
    | GotoClicked
    | RestartClicked
    | TagPickRequested String
    | PickerKeyed String
    | PickerChosen String
    | PickerDismissed
    | VarEdited String String
    | VarReset String
    | VarsResetAll
    | PaneChosen Pane
    | GraphNodePicked String


type Out
    = OutNone
    | OutRead String
    | OutBuild String (Maybe String)
    | OutShot Target
    | OutGoto Target
    | OutRetryLater Int
    | OutRestart Target



-- 決まり(純粋)


{-| シナリオと同じフォルダの語彙(ゲームの Talk.vocab が書く vocab.json)。 -}
vocabPathOf : String -> String
vocabPathOf path =
    case String.indexes "/" path |> List.reverse |> List.head of
        Just i ->
            String.left i path ++ "/vocab.json"

        Nothing ->
            "vocab.json"


{-| 本文の文字位置が何行目か(1 始まり)。 -}
lineAt : String -> Int -> Int
lineAt body offset =
    String.left offset body
        |> String.indexes "\n"
        |> List.length
        |> (+) 1


{-| その行を中に持つノード(このファイルで title: の行が line 以下の、いちばん後の物)。
engine の Yarn.nodeAtLine と bin/fge talk where と同じ決まり。
-}
targetAt : String -> List Node -> Int -> Maybe Target
targetAt path nodes line =
    nodes
        |> List.filter (\n -> n.file == path && n.line <= line)
        |> List.sortBy .line
        |> List.reverse
        |> List.head
        |> Maybe.map (\n -> { node = n.title, line = line })


{-| t を頼む。頼んでいる最中なら、答えが来た後に最後の 1 つだけ頼む。
force = 同じ行でも撮り直す(保存で台本が変わった)。
-}
ask : Bool -> Target -> Model -> ( Model, Out )
ask force t model =
    case model.request of
        Idle ->
            ( { model | request = InFlight t }, OutShot t )

        InFlight cur ->
            if cur == t && not force then
                ( model, OutNone )

            else
                ( { model | request = InFlightThen cur t }, OutNone )

        InFlightThen cur _ ->
            ( { model | request = InFlightThen cur t }, OutNone )


{-| 今のカーソルの行を頼む(ノードが無い行なら頼まない)。 -}
askCursor : Bool -> Model -> ( Model, Out )
askCursor force model =
    case targetAt model.path model.nodes model.cursor of
        Just t ->
            ask force t model

        Nothing ->
            ( model, OutNone )


{-| 絵を前のまま残して理由を添える。 -}
hold : String -> Model -> Model
hold why model =
    case model.shown of
        Shown shot ->
            { model | shown = Held shot why, note = why }

        Held shot _ ->
            { model | shown = Held shot why, note = why }

        Blank ->
            { model | note = why }



-- 更新


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        FilePicked path ->
            ( { init | files = model.files, hasEntry = model.hasEntry, path = path, retrySeq = model.retrySeq + 1 }
            , OutRead path
            )

        TextEdited body offset ->
            moveCaret offset { model | text = body }

        CaretMoved body offset ->
            moveCaret offset { model | text = body }

        SaveClicked ->
            if model.path == "" || model.saving /= Nothing then
                ( model, OutNone )

            else
                ( { model | saving = Just model.text, status = "" }, OutBuild model.path (Just model.text) )

        NodeClicked node ->
            askCursor False { model | cursor = node.line }

        GotoClicked ->
            case targetAt model.path model.nodes model.cursor of
                Just t ->
                    ( { model | status = "" }, OutGoto t )

                Nothing ->
                    ( { model | status = "この行はどのノードの中でもありません" }, OutNone )

        RestartClicked ->
            -- 頼んでいる最中は押せない(ボタンも出さない)。答えは ShotStarting で返り、頼み直しが続く
            case ( model.request, targetAt model.path model.nodes model.cursor ) of
                ( Idle, Just t ) ->
                    ( { model | request = InFlight t, headless = HeadlessBooting BootWaiting, status = "" }, OutRestart t )

                ( Idle, Nothing ) ->
                    ( { model | status = "カーソルを会話の行に置いてから起こしてください" }, OutNone )

                _ ->
                    ( model, OutNone )

        TagPickRequested key ->
            -- 図を見ている間は開かない(ダイアログは台本の面にある)
            if model.pane == ScriptPane then
                ( openPicker key model, OutNone )

            else
                ( model, OutNone )

        PickerKeyed key ->
            case model.picker of
                PickerOpen pick values at ->
                    case key of
                        "ArrowUp" ->
                            ( { model | picker = PickerOpen pick values (modBy (List.length values) (at - 1)) }, OutNone )

                        "ArrowDown" ->
                            ( { model | picker = PickerOpen pick values (modBy (List.length values) (at + 1)) }, OutNone )

                        "Enter" ->
                            case values |> List.drop at |> List.head of
                                Just value ->
                                    choose pick value model

                                Nothing ->
                                    ( { model | picker = PickerClosed }, OutNone )

                        "Escape" ->
                            ( { model | picker = PickerClosed }, OutNone )

                        _ ->
                            ( model, OutNone )

                PickerClosed ->
                    ( model, OutNone )

        PickerChosen value ->
            case model.picker of
                PickerOpen pick _ _ ->
                    choose pick value model

                PickerClosed ->
                    ( model, OutNone )

        PickerDismissed ->
            ( { model | picker = PickerClosed }, OutNone )

        VarEdited name value ->
            let
                initial =
                    model.tree.variables
                        |> List.filter (\v -> v.name == name)
                        |> List.head
                        |> Maybe.map (.value >> TalkTree.valueText)
            in
            askCursor True
                { model
                    | overrides =
                        if Just value == initial then
                            Dict.remove name model.overrides

                        else
                            Dict.insert name value model.overrides
                }

        VarReset name ->
            askCursor True { model | overrides = Dict.remove name model.overrides }

        PaneChosen pane ->
            ( { model | pane = pane, picker = PickerClosed }, OutNone )

        GraphNodePicked title ->
            -- 図の箱を押した: そのノードの title: の行へカーソルを置いて台本に戻り、撮る
            case model.nodes |> List.filter (\n -> n.title == title && n.file == model.path) |> List.head of
                Just node ->
                    askCursor False { model | cursor = node.line, pane = ScriptPane, picker = PickerClosed }

                Nothing ->
                    ( { model | pane = ScriptPane }, OutNone )

        VarsResetAll ->
            if Dict.isEmpty model.overrides then
                ( model, OutNone )

            else
                askCursor True { model | overrides = Dict.empty }


{-| カーソルの行のタグ key を選ぶダイアログを開く。開けない時は理由を status に出す。
選択中は今の値(無ければ先頭)。
-}
openPicker : String -> Model -> Model
openPicker key model =
    case ( model.vocab, TalkTags.lineTagsAt model.text model.cursor ) of
        ( Nothing, _ ) ->
            { model | status = "台本の語彙(" ++ vocabPathOf model.path ++ ")を読めていません(ゲームの make talk-vocab で書けます)" }

        ( _, Nothing ) ->
            { model | status = "この行にはタグを付けられません(ノードの本文の台詞・地の文・選択肢の行で選べます)" }

        ( Just vocab, Just line ) ->
            case TalkTags.choicesFor vocab line.speaker key of
                [] ->
                    { model | status = "語彙に #" ++ key ++ " の値がありません(" ++ vocabPathOf model.path ++ ")" }

                values ->
                    let
                        current =
                            line.tags |> List.filter (\( k, _ ) -> k == key) |> List.head |> Maybe.map Tuple.second

                        at =
                            values
                                |> List.indexedMap Tuple.pair
                                |> List.filter (\( _, v ) -> Just v == current)
                                |> List.head
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault 0
                    in
                    { model | picker = PickerOpen { line = model.cursor, key = key } values at, status = "" }


{-| 選んだ値をその行に書き、保存して撮り直す(保存の最中なら書くだけ。未保存の印が残る)。 -}
choose : Pick -> String -> Model -> ( Model, Out )
choose pick value model =
    let
        order =
            model.vocab |> Maybe.map TalkTags.tagKeys |> Maybe.withDefault []

        body =
            TalkTags.setTagAt order pick.line pick.key value model.text

        m1 =
            { model | text = body, picker = PickerClosed }
    in
    if body == model.text then
        ( m1, OutNone )

    else
        update SaveClicked m1


{-| 撮る時に渡す変数の値(値を変えた物だけ。宣言から消えた名前は渡さない — ゲームが断るため)。 -}
varsOf : Model -> List ( String, String )
varsOf model =
    model.overrides
        |> Dict.toList
        |> List.filter (\( name, _ ) -> List.any (\v -> v.name == name) model.tree.variables)


{-| 組んだ木が届いた(読めなければ空)。宣言から消えた変数の値は捨てる。 -}
gotTree : Result String TalkTree.Tree -> Model -> Model
gotTree result model =
    let
        tree =
            Result.withDefault TalkTree.empty result
    in
    { model
        | tree = tree
        , overrides = Dict.filter (\name _ -> List.any (\v -> v.name == name) tree.variables) model.overrides
    }


{-| 語彙が届いた(読めなければ Nothing のまま。ダイアログを開く時に理由を出す)。 -}
gotVocab : Result String TalkTags.Vocab -> Model -> Model
gotVocab result model =
    case result of
        Ok vocab ->
            { model | vocab = Just vocab }

        Err _ ->
            { model | vocab = Nothing }


moveCaret : Int -> Model -> ( Model, Out )
moveCaret offset model =
    let
        line =
            lineAt model.text offset
    in
    if line == model.cursor then
        ( model, OutNone )

    else
        -- 別の行へ動いたらダイアログは閉じる(選んだ値を前の行に書かない)
        askCursor False { model | cursor = line, picker = PickerClosed }


{-| シナリオの一覧が届いた。まだ何も開いていなければ先頭を開く。 -}
gotFiles : { files : List String, hasEntry : Bool } -> Model -> ( Model, Out )
gotFiles got model =
    let
        m1 =
            { model | files = got.files, hasEntry = got.hasEntry }
    in
    if model.path == "" then
        case List.head got.files of
            Just first ->
                update (FilePicked first) m1

            Nothing ->
                ( m1, OutNone )

    else
        ( m1, OutNone )


{-| ファイルの中身が届いた。組んで(読むだけ)ノードと誤りを得る。 -}
gotFile : String -> String -> Model -> ( Model, Out )
gotFile path body model =
    if path /= model.path then
        ( model, OutNone )

    else
        ( { model | text = body, saved = body }, OutBuild path Nothing )


{-| 組んだ結果。保存した回は今の行を撮り直す。壊れていれば前の絵のまま理由を出す。 -}
gotBuild : { ok : Bool, saved : Bool, nodes : List Node, problems : List Problem, notes : List Problem } -> Model -> ( Model, Out )
gotBuild report model =
    let
        m1 =
            { model
                | nodes = report.nodes
                , problems = report.problems
                , notes = report.notes
                , saving = Nothing
                , saved =
                    case ( report.saved, model.saving ) of
                        ( True, Just body ) ->
                            body

                        _ ->
                            model.saved
            }
    in
    if report.ok then
        askCursor report.saved { m1 | status = if report.saved then "保存しました" else "" }

    else
        let
            first =
                report.problems
                    |> List.head
                    |> Maybe.map
                        (\p ->
                            if p.line > 0 then
                                String.fromInt p.line ++ " 行目: " ++ p.msg

                            else
                                p.msg
                        )
                    |> Maybe.withDefault "組めませんでした"
        in
        ( hold ("台本に誤りがあるので前の絵のまま(" ++ first ++ ")")
            { m1
                | status =
                    if report.saved then
                        "保存しました(誤りがあるので、ゲームの会話は前のまま)"

                    else
                        ""
            }
        , OutNone
        )


{-| 撮った答え。今の頼みと違う Target の答えは捨てる。 -}
gotShot : Target -> ShotAnswer -> Model -> ( Model, Out )
gotShot t answer model =
    let
        apply m =
            case answer of
                ShotOk png ->
                    ( { m | shown = Shown { target = t, png = png }, note = "", headless = HeadlessOk }, OutNone )

                ShotStarting why boot ->
                    retryLater (hold why { m | headless = HeadlessBooting boot })

                ShotStalled why log ->
                    retryLater (hold why { m | headless = HeadlessStalled log })

                ShotCrashed why log ->
                    ( hold why { m | headless = HeadlessCrashed log }, OutNone )

                ShotFailed why ->
                    ( hold why m, OutNone )
    in
    case model.request of
        InFlight cur ->
            if cur == t then
                apply { model | request = Idle }

            else
                ( model, OutNone )

        InFlightThen cur next ->
            if cur == t then
                -- 答えの中身は見せ、待っていた最後の行をすぐ頼む(頼み直しの予約は要らない)
                let
                    ( m1, _ ) =
                        apply { model | request = InFlight next }
                in
                ( m1, OutShot next )

            else
                ( model, OutNone )

        Idle ->
            ( model, OutNone )


{-| 少し後に頼み直す予約(前の予約は照合キーで捨てられる)。 -}
retryLater : Model -> ( Model, Out )
retryLater model =
    ( { model | retrySeq = model.retrySeq + 1 }, OutRetryLater (model.retrySeq + 1) )


{-| 頼み直しの予約が来た(古い予約は捨てる)。カーソルがノードの外なら頼まずに予約だけ続ける
(予約を落とすと、窓なしのゲームが起きても「起こしています」のまま止まる)。
-}
retryFired : Int -> Model -> ( Model, Out )
retryFired seq model =
    if seq /= model.retrySeq || model.request /= Idle then
        ( model, OutNone )

    else
        case targetAt model.path model.nodes model.cursor of
            Just t ->
                ask True t model

            Nothing ->
                retryLater model


{-| 起動中のゲームへ飛ばした結果。 -}
gotGoto : Result String () -> Model -> Model
gotGoto result model =
    case result of
        Ok () ->
            { model | status = "ゲームをこの行へ飛ばしました" }

        Err why ->
            { model | status = why }



-- デコーダ


filesDecoder : D.Decoder { files : List String, hasEntry : Bool }
filesDecoder =
    D.map2 (\files hasEntry -> { files = files, hasEntry = hasEntry })
        (D.field "files" (D.list D.string))
        (D.oneOf [ D.field "headless" D.bool, D.succeed True ])


problemDecoder : D.Decoder Problem
problemDecoder =
    D.map3 Problem
        (D.oneOf [ D.field "file" D.string, D.succeed "" ])
        (D.oneOf [ D.field "line" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "msg" D.string, D.at [ "message" ] D.string, D.succeed "" ])


{-| /talk/build の答え(道具の Report + saved)。道具が使えない時の {ok:false, error:{message}} も読む。 -}
buildDecoder : D.Decoder { ok : Bool, saved : Bool, nodes : List Node, problems : List Problem, notes : List Problem }
buildDecoder =
    let
        list key decoder =
            D.oneOf [ D.field key (D.list decoder), D.succeed [] ]
    in
    D.map5 (\ok saved nodes problems notes -> { ok = ok, saved = saved, nodes = nodes, problems = problems, notes = notes })
        (D.field "ok" D.bool)
        (D.oneOf [ D.field "saved" D.bool, D.succeed False ])
        (list "nodes" (D.map3 Node (D.field "title" D.string) (D.field "file" D.string) (D.field "line" D.int)))
        (D.oneOf
            [ D.field "problems" (D.list problemDecoder)
            , D.at [ "error", "message" ] D.string |> D.map (\m -> [ { file = "", line = 0, msg = m } ])
            , D.succeed []
            ]
        )
        (list "notes" problemDecoder)


shotAnswerDecoder : D.Decoder ShotAnswer
shotAnswerDecoder =
    D.field "ok" D.bool
        |> D.andThen
            (\ok ->
                if ok then
                    D.map ShotOk (D.field "png" D.string)

                else
                    D.map4
                        (\phase why log boot ->
                            case phase of
                                "starting" ->
                                    ShotStarting why boot

                                "stalled" ->
                                    ShotStalled why log

                                "failed" ->
                                    ShotCrashed why log

                                _ ->
                                    ShotFailed why
                        )
                        (D.oneOf [ D.field "headless" D.string, D.succeed "" ])
                        (D.oneOf [ D.at [ "error", "message" ] D.string, D.succeed "撮れませんでした" ])
                        (D.oneOf [ D.field "log" (D.list D.string), D.succeed [] ])
                        bootDecoder
            )


{-| 段(stage: "jar" / "boot")と経過秒(elapsed)。無い・知らない段は BootWaiting(古いサーバ・ゲームの起動待ち)。 -}
bootDecoder : D.Decoder Boot
bootDecoder =
    D.map2
        (\stage elapsed ->
            case stage of
                "jar" ->
                    BootBuildingJar elapsed

                "boot" ->
                    BootStartingGame elapsed

                _ ->
                    BootWaiting
        )
        (D.oneOf [ D.field "stage" D.string, D.succeed "" ])
        (D.oneOf [ D.field "elapsed" D.int, D.succeed 0 ])


{-| 見出しの一言。起こしている間は頼み直しの合間も出す(jar を作る時だけ「1〜2 分」、起こしているだけなら「数秒」)。
撮っている最中だけ「撮っています」。どちらでもなければ出さない(空)。
-}
busyLabel : Bool -> Headless -> String
busyLabel inFlight headless =
    case headless of
        HeadlessBooting (BootBuildingJar secs) ->
            "⏳ jar を作っています(1〜2 分)… " ++ String.fromInt secs ++ " 秒"

        HeadlessBooting (BootStartingGame secs) ->
            "⏳ 起こしています(数秒)… " ++ String.fromInt secs ++ " 秒"

        HeadlessBooting BootWaiting ->
            "⏳ 待っています…"

        _ ->
            if inFlight then
                "⏳ 撮っています…"

            else
                ""




-- 画面


lineHeight : Int
lineHeight =
    20


view : Model -> Html Msg
view model =
    div [ HA.class "talk flex min-h-0 flex-1 gap-4 px-6 pb-6 pt-4", HE.preventDefaultOn "keydown" (pickerKeyDecoder model.picker) ]
        [ div [ HA.class "flex min-h-0 min-w-0 flex-1 flex-col gap-2" ]
            [ viewHead model
            , viewNodes model
            , case model.pane of
                ScriptPane ->
                    viewLineTags model

                GraphPane ->
                    text ""
            , case model.pane of
                ScriptPane ->
                    viewEditor model

                GraphPane ->
                    viewGraph model
            , viewProblems model
            ]
        , div [ HA.class "flex min-h-0 w-[48%] flex-col gap-2" ]
            [ viewPreview model ]
        ]


{-| 会話の画面のキー(本文の欄からも届く)。⌘E / Ctrl+E で表情・⌘B / Ctrl+B で背景を選ぶダイアログを開き、
開いている間は ↑↓・Enter・Esc をダイアログが取る(本文のカーソルは動かさない・改行しない)。ほかは素通し。
-}
pickerKeyDecoder : Picker -> D.Decoder ( Msg, Bool )
pickerKeyDecoder picker =
    D.map3 (\key meta ctrl -> ( key, meta || ctrl ))
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        |> D.andThen
            (\( key, modified ) ->
                case ( picker, modified, String.toLower key ) of
                    ( _, True, "e" ) ->
                        D.succeed ( TagPickRequested "face", True )

                    ( _, True, "b" ) ->
                        D.succeed ( TagPickRequested "backdrop", True )

                    ( PickerOpen _ _ _, False, _ ) ->
                        if List.member key [ "ArrowUp", "ArrowDown", "Enter", "Escape" ] then
                            D.succeed ( PickerKeyed key, True )

                        else
                            D.fail "ほかのキーは素通し"

                    _ ->
                        D.fail "ほかのキーは素通し"
            )


{-| カーソルの行の話し手とタグのチップ。語彙に値のあるキーは押すと選ぶダイアログ(その下に開く)。 -}
viewLineTags : Model -> Html Msg
viewLineTags model =
    case TalkTags.lineTagsAt model.text model.cursor of
        Nothing ->
            text ""

        Just line ->
            let
                keys =
                    model.vocab |> Maybe.map TalkTags.tagKeys |> Maybe.withDefault []

                valueOf key =
                    line.tags |> List.filter (\( k, _ ) -> k == key) |> List.head |> Maybe.map Tuple.second

                shortcut key =
                    case key of
                        "face" ->
                            "(⌘E)"

                        "backdrop" ->
                            "(⌘B)"

                        _ ->
                            ""

                chip key =
                    button
                        [ HA.class "talk-tag-chip cursor-pointer rounded-sm bg-white/5 px-1.5 font-mono text-[11px] text-ink-soft hover:bg-white/10"
                        , HA.title (TalkTags.label key ++ "を選ぶ" ++ shortcut key)
                        , HE.onClick (TagPickRequested key)
                        ]
                        [ text (TalkTags.label key ++ ": " ++ Maybe.withDefault "—" (valueOf key) ++ " ▾") ]

                others =
                    line.tags
                        |> List.filter (\( k, _ ) -> not (List.member k keys))
                        |> List.map
                            (\( k, v ) ->
                                span [ HA.class "rounded-sm px-1.5 font-mono text-[11px] text-ink-faint" ]
                                    [ text ("#" ++ k ++ (if v == "" then "" else ":" ++ v)) ]
                            )
            in
            div [ HA.class "talk-line-tags relative flex flex-wrap items-center gap-1" ]
                (span [ HA.class "text-[11px] text-ink-faint" ]
                    [ text (String.fromInt model.cursor ++ " 行目" ++ (line.speaker |> Maybe.map (\s -> " · " ++ s) |> Maybe.withDefault "")) ]
                    :: List.map chip keys
                    ++ others
                    ++ [ viewPicker model ]
                )


{-| 選ぶダイアログ(チップの下に開く小さな一覧)。 -}
viewPicker : Model -> Html Msg
viewPicker model =
    case model.picker of
        PickerClosed ->
            text ""

        PickerOpen pick values at ->
            div
                [ HA.class "talk-picker absolute left-0 top-full z-20 mt-1 flex min-w-48 flex-col gap-0.5 rounded-md border border-edge bg-raised p-1 shadow-lg"
                , HA.attribute "role" "listbox"
                ]
                (div [ HA.class "flex items-center gap-2 px-1.5 pb-1 text-[11px] text-ink-faint" ]
                    [ text (TalkTags.label pick.key ++ "を選ぶ(" ++ String.fromInt pick.line ++ " 行目)")
                    , span [ HA.class "ml-auto" ] [ text "↑↓ Enter · Esc" ]
                    , button [ HA.class "talk-picker-close d-btn d-btn-ghost d-btn-xs", HE.onClick PickerDismissed ] [ text "×" ]
                    ]
                    :: List.indexedMap
                        (\i value ->
                            button
                                [ HA.classList
                                    [ ( "talk-picker-item cursor-pointer rounded-sm px-1.5 py-0.5 text-left font-mono text-[12px]", True )
                                    , ( "bg-accent/30 text-ink", i == at )
                                    , ( "text-ink-soft hover:bg-white/10", i /= at )
                                    ]
                                , HA.attribute "role" "option"
                                , HA.attribute "aria-selected" (if i == at then "true" else "false")
                                , HE.onClick (PickerChosen value)
                                ]
                                [ text value ]
                        )
                        values
                )


{-| ノードの図(このファイルのノードの箱と矢印)。木がまだ無ければ案内だけ。 -}
viewGraph : Model -> Html Msg
viewGraph model =
    let
        graph =
            TalkGraph.build model.path model.tree.nodes
    in
    div [ HA.class "talk-graph-pane relative min-h-0 flex-1 overflow-auto rounded-md border border-edge bg-well p-2" ]
        [ if List.isEmpty graph.placed then
            div [ HA.class "text-xs text-ink-faint" ] [ text "このファイルのノードがまだ組まれていません(保存すると dialogue.json が書かれ、図が出ます)" ]

          else
            TalkGraph.view
                { here = targetAt model.path model.nodes model.cursor |> Maybe.map .node
                , onPick = \node -> GraphNodePicked node.title
                }
                graph
        , div [ HA.class "mt-1 text-[10px] text-ink-faint" ]
            [ text "実線 = <<jump>> / 点線 = 選択肢の行き先 / ○ = 別のファイルのノード。箱を押すとその台本へ(並びはヘッダの position:、無ければ jump の段)" ]
        ]


viewPaneSwitch : Model -> Html Msg
viewPaneSwitch model =
    let
        tab pane label =
            button
                [ HA.classList
                    [ ( "talk-pane d-btn d-btn-xs", True )
                    , ( "d-btn-active", model.pane == pane )
                    ]
                , HE.onClick (PaneChosen pane)
                ]
                [ text label ]
    in
    div [ HA.class "d-join" ] [ tab ScriptPane "台本", tab GraphPane "図" ]


viewHead : Model -> Html Msg
viewHead model =
    div [ HA.class "flex items-center gap-2" ]
        [ if List.isEmpty model.files then
            span [ HA.class "text-xs text-ink-soft" ]
                [ text "会話のシナリオ(*.yarn)がありません。engine の docs/migrations/0.48.0.md の「移し方」で assets/talk/ に置くと、ここで書けます。" ]

          else
            select
                [ HA.class "talk-file d-select d-select-xs font-mono"
                , HE.onInput FilePicked
                ]
                (List.map (\f -> option [ HA.value f, HA.selected (f == model.path) ] [ text f ]) model.files)
        , viewPaneSwitch model
        , if model.text /= model.saved then
            span [ HA.class "text-[11px] text-ink-faint" ] [ text "● 未保存" ]

          else
            text ""
        , button
            [ HA.class "talk-save d-btn d-btn-xs ml-auto"
            , HA.disabled (model.path == "" || model.saving /= Nothing)
            , HA.title "保存して組み直し、今の行を撮り直します(⌘S / Ctrl+S)"
            , HE.onClick SaveClicked
            ]
            [ text
                (if model.saving /= Nothing then
                    "組んでいます…"

                 else
                    "保存"
                )
            ]
        ]


viewNodes : Model -> Html Msg
viewNodes model =
    let
        here =
            targetAt model.path model.nodes model.cursor |> Maybe.map .node
    in
    div [ HA.class "flex flex-wrap gap-1" ]
        (model.nodes
            |> List.filter (\n -> n.file == model.path)
            |> List.map
                (\n ->
                    button
                        [ HA.classList
                            [ ( "talk-node cursor-pointer rounded-sm px-1.5 font-mono text-[11px]", True )
                            , ( "bg-accent/30 text-ink", here == Just n.title )
                            , ( "bg-white/5 text-ink-soft hover:bg-white/10", here /= Just n.title )
                            ]
                        , HA.title (String.fromInt n.line ++ " 行目")
                        , HE.onClick (NodeClicked n)
                        ]
                        [ text n.title ]
                )
        )


{-| 行番号の溝・本文・誤りの一言(その行の右)を同じ行の高さで重ねる。本文は中身の行数ぶんの
高さに伸ばし、外の枠でスクロールする(溝と本文がずれない)。
-}
viewEditor : Model -> Html Msg
viewEditor model =
    let
        lineCount =
            List.length (String.lines model.text)

        px n =
            String.fromInt n ++ "px"

        marked =
            model.problems |> List.filter (\p -> p.file == model.path && p.line > 0)

        caret =
            D.map2 Tuple.pair (D.at [ "target", "value" ] D.string) (D.at [ "target", "selectionStart" ] D.int)

        onCaret event =
            HE.on event (D.map (\( v, o ) -> CaretMoved v o) caret)

        -- ⌘S / Ctrl+S だけ拾ってブラウザの「保存」を止める(ほかのキーは素通し)
        saveKey =
            HE.preventDefaultOn "keydown"
                (D.map3 (\key meta ctrl -> key == "s" && (meta || ctrl))
                    (D.field "key" D.string)
                    (D.field "metaKey" D.bool)
                    (D.field "ctrlKey" D.bool)
                    |> D.andThen
                        (\isSave ->
                            if isSave then
                                D.succeed ( SaveClicked, True )

                            else
                                D.fail "ふつうのキー"
                        )
                )
    in
    div [ HA.class "talk-editor relative min-h-0 flex-1 overflow-auto rounded-md border border-edge bg-well" ]
        [ div [ HA.class "relative flex", HA.style "min-height" "100%" ]
            [ div
                [ HA.class "shrink-0 select-none border-r border-edge px-2 py-2 text-right font-mono text-[11px] text-ink-faint" ]
                (List.range 1 lineCount
                    |> List.map
                        (\i ->
                            div
                                [ HA.style "height" (px lineHeight)
                                , HA.style "line-height" (px lineHeight)
                                , HA.classList
                                    [ ( "text-red-300", List.any (\p -> p.line == i) marked )
                                    , ( "text-ink", i == model.cursor )
                                    ]
                                ]
                                [ text
                                    (if List.any (\p -> p.line == i) marked then
                                        "● " ++ String.fromInt i

                                     else
                                        String.fromInt i
                                    )
                                ]
                        )
                )
            , div [ HA.class "relative min-w-0 flex-1" ]
                (textarea
                    [ HA.class "talk-text block w-full resize-none overflow-hidden whitespace-pre bg-transparent px-3 py-2 font-mono text-[12px] text-ink outline-none"
                    , HA.style "line-height" (px lineHeight)
                    , HA.style "height" (px (lineHeight * (lineCount + 1) + 16))
                    , HA.attribute "wrap" "off"
                    , HA.spellcheck False
                    , HA.value model.text
                    , HE.on "input" (D.map (\( v, o ) -> TextEdited v o) caret)
                    , onCaret "keyup"
                    , onCaret "click"
                    , saveKey
                    ]
                    []
                    :: List.map
                        (\p ->
                            div
                                [ HA.class "talk-problem pointer-events-none absolute right-2 max-w-[60%] truncate rounded-sm bg-red-900/80 px-1.5 text-[11px] text-red-100"
                                , HA.style "top" (px (8 + lineHeight * (p.line - 1)))
                                , HA.style "line-height" (px lineHeight)
                                ]
                                [ text p.msg ]
                        )
                        marked
                )
            ]
        ]


viewProblems : Model -> Html Msg
viewProblems model =
    let
        elsewhere =
            model.problems |> List.filter (\p -> p.file /= model.path || p.line <= 0)
    in
    div [ HA.class "flex flex-col gap-0.5" ]
        (List.map
            (\p ->
                div [ HA.class "text-[11px] text-red-300" ]
                    [ text
                        ((if p.file == "" then
                            ""

                          else
                            p.file ++ (if p.line > 0 then ":" ++ String.fromInt p.line else "") ++ ": "
                         )
                            ++ p.msg
                        )
                    ]
            )
            elsewhere
            ++ List.map (\p -> div [ HA.class "text-[11px] text-ink-faint" ] [ text ("注意: " ++ String.fromInt p.line ++ " 行目: " ++ p.msg) ])
                (model.notes |> List.filter (\p -> p.file == model.path))
            ++ [ if model.status == "" then
                    text ""

                 else
                    div [ HA.class "text-[11px] text-ink-soft" ] [ text model.status ]
               ]
        )


viewPreview : Model -> Html Msg
viewPreview model =
    let
        caption =
            case targetAt model.path model.nodes model.cursor of
                Just t ->
                    t.node ++ " · " ++ String.fromInt t.line ++ " 行目"

                Nothing ->
                    "ノードの外の行"

        picture shot dim =
            img
                [ HA.class "talk-shot block w-full rounded-md border border-edge bg-black"
                , HA.classList [ ( "opacity-50", dim ) ]
                , HA.src ("data:image/png;base64," ++ shot.png)
                , HA.alt (shot.target.node ++ ":" ++ String.fromInt shot.target.line)
                ]
                []

        busy =
            busyLabel (model.request /= Idle) model.headless
    in
    div [ HA.class "flex flex-col gap-2" ]
        [ div [ HA.class "flex items-center gap-2" ]
            [ span [ HA.class "font-mono text-xs text-ink" ] [ text caption ]
            , if busy /= "" then
                span [ HA.class "talk-busy text-[11px] text-ink-faint" ] [ text busy ]

              else
                text ""
            , button
                [ HA.class "talk-goto d-btn d-btn-xs ml-auto"
                , HA.title "起動中のゲームを、この行を出している所へ飛ばします(遊んでいる場面が差し替わります)"
                , HE.onClick GotoClicked
                ]
                [ text "この行へ飛ばす" ]
            ]
        , if not model.hasEntry then
            div [ HA.class "text-xs text-ink-soft" ]
                [ text "このゲームには窓なしの入口がありません。src に pub def serveHeadless() を置くと、ここにゲームの本物の画面が出ます(engine の App.Headless の冒頭)。" ]

          else
            case model.shown of
                Blank ->
                    div [ HA.class "flex h-48 items-center justify-center rounded-md border border-dashed border-edge text-xs text-ink-faint" ]
                        [ text
                            (if model.note == "" then
                                "カーソルを会話の行に置くと、ここにゲームの画面が出ます"

                             else
                                model.note
                            )
                        ]

                Shown shot ->
                    picture shot False

                Held shot why ->
                    div [ HA.class "flex flex-col gap-1" ]
                        [ div [ HA.class "talk-held rounded-sm bg-amber-500/20 px-2 py-1 text-[11px] text-ink" ] [ text why ]
                        , picture shot True
                        ]
        , viewHeadlessTrouble model
        , viewVariables model
        , div [ HA.class "text-[11px] text-ink-faint" ]
            [ text "右の絵はゲームが窓なしで描いた本物の画面です(起動中のゲームは動かしません)。保存すると撮り直します。" ]
        ]


{-| 変数の一覧(`<<declare>>` の名前・初めの値・プレビューの値)。値を変えるとその値で撮り直す。 -}
viewVariables : Model -> Html Msg
viewVariables model =
    if List.isEmpty model.tree.variables then
        text ""

    else
        div [ HA.class "talk-vars flex flex-col gap-1" ]
            [ div [ HA.class "flex items-center gap-2" ]
                [ span [ HA.class "text-xs font-semibold text-ink" ] [ text "変数" ]
                , span [ HA.class "text-[11px] text-ink-faint" ] [ text "値を変えると、その値で撮り直します(台本は変えません)" ]
                , if Dict.isEmpty model.overrides then
                    text ""

                  else
                    button [ HA.class "talk-vars-reset d-btn d-btn-ghost d-btn-xs ml-auto", HE.onClick VarsResetAll ] [ text "全部を初めの値に" ]
                ]
            , Html.table [ HA.class "w-full text-[11px]" ]
                [ Html.tbody []
                    (List.map (viewVariable model) model.tree.variables)
                ]
            ]


viewVariable : Model -> TalkTree.Variable -> Html Msg
viewVariable model var =
    let
        initial =
            TalkTree.valueText var.value

        current =
            Dict.get var.name model.overrides |> Maybe.withDefault initial

        changed =
            Dict.member var.name model.overrides

        field =
            case var.value of
                TalkTree.Flag _ ->
                    select [ HA.class "talk-var d-select d-select-xs font-mono", HE.onInput (VarEdited var.name) ]
                        (List.map (\v -> option [ HA.value v, HA.selected (v == current) ] [ text v ]) [ "true", "false" ])

                TalkTree.Number _ ->
                    input [ HA.class "talk-var d-input d-input-xs w-24 font-mono", HA.type_ "number", HA.value current, HE.onInput (VarEdited var.name) ] []

                TalkTree.Text _ ->
                    input [ HA.class "talk-var d-input d-input-xs w-40 font-mono", HA.value current, HE.onInput (VarEdited var.name) ] []
    in
    Html.tr [ HA.classList [ ( "bg-accent/10", changed ) ] ]
        [ Html.td [ HA.class "py-0.5 pr-2 font-mono text-ink", HA.title (var.file ++ ":" ++ String.fromInt var.line) ] [ text var.name ]
        , Html.td [ HA.class "py-0.5 pr-2 font-mono text-ink-faint" ] [ text ("初め " ++ initial) ]
        , Html.td [ HA.class "py-0.5 pr-2" ] [ field ]
        , Html.td [ HA.class "py-0.5" ]
            [ if changed then
                button [ HA.class "d-btn d-btn-ghost d-btn-xs", HA.title "初めの値に戻す", HE.onClick (VarReset var.name) ] [ text "↺" ]

              else
                text ""
            ]
        ]


{-| 窓なしのゲームが長く起きない・落ちた時: ログの末尾と「もう一度起こす」。理由の一言は上の Blank / Held に出ている。 -}
viewHeadlessTrouble : Model -> Html Msg
viewHeadlessTrouble model =
    let
        trouble log label =
            div [ HA.class "talk-headless flex flex-col gap-1" ]
                [ if List.isEmpty log then
                    text ""

                  else
                    Html.pre [ HA.class "talk-headless-log max-h-48 overflow-auto whitespace-pre-wrap rounded-sm bg-well px-2 py-1 font-mono text-[10px] text-ink-soft" ]
                        [ text (String.join "\n" log) ]
                , div [ HA.class "flex items-center gap-2" ]
                    [ button
                        [ HA.class "talk-restart d-btn d-btn-xs"
                        , HA.disabled (model.request /= Idle)
                        , HA.title "窓なしのゲームを止めて、もう一度起こします(1 本だけ。また落ちたら理由に戻ります)"
                        , HE.onClick RestartClicked
                        ]
                        [ text "もう一度起こす" ]
                    , span [ HA.class "text-[11px] text-ink-faint" ] [ text label ]
                    ]
                ]
    in
    case model.headless of
        HeadlessStalled log ->
            trouble log "待ち続けています(debug/headless.log)"

        HeadlessCrashed log ->
            trouble log "落ちたので、押すまで起こしません(debug/headless.log)"

        _ ->
            text ""

