module TalkView exposing
    ( Model
    , Msg(..)
    , Node
    , Out(..)
    , Problem
    , Request(..)
    , Shot
    , ShotAnswer(..)
    , Shown(..)
    , Target
    , buildDecoder
    , filesDecoder
    , gotBuild
    , gotFile
    , gotFiles
    , gotGoto
    , gotShot
    , init
    , lineAt
    , retryFired
    , shotAnswerDecoder
    , targetAt
    , update
    , view
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

答えの Target が今の頼みと違えば捨てる(ファイルを開き直した後に、前のファイルの答えが届いても混ぜない)。

保存で組み直し(dialogue.json が書き変わる)、今の行を撮り直す。打っている途中の壊れた台本は
ok:false で返り、前の絵を Held で出したまま誤りの行を示す。

「この行へ飛ばす」だけが起動中のゲームを動かす(遊んでいる World をその行の場面へ差し替える)。

サーバ往復は Main の仕事(Out で頼む)。

-}

import Html exposing (Html, button, div, img, option, select, span, text, textarea)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D


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


{-| 撮った答え。Starting = 窓なしのゲームを起こしている(少し後に頼み直す)。 -}
type ShotAnswer
    = ShotOk String
    | ShotStarting String
    | ShotFailed String


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

    -- 絵が無い時に出す一言(Blank の理由)・保存や飛ばした結果の一言
    , note : String
    , status : String

    -- 頼み直しの予約の照合キー(古い予約は捨てる)
    , retrySeq : Int

    -- 保存して組んでいる最中の中身(答えが来たら saved になる)
    , saving : Maybe String
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
    , note = ""
    , status = ""
    , retrySeq = 0
    , saving = Nothing
    }


type Msg
    = FilePicked String
    | TextEdited String Int
    | CaretMoved String Int
    | SaveClicked
    | NodeClicked Node
    | GotoClicked


type Out
    = OutNone
    | OutRead String
    | OutBuild String (Maybe String)
    | OutShot Target
    | OutGoto Target
    | OutRetryLater Int



-- 決まり(純粋)


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


moveCaret : Int -> Model -> ( Model, Out )
moveCaret offset model =
    let
        line =
            lineAt model.text offset
    in
    if line == model.cursor then
        ( model, OutNone )

    else
        askCursor False { model | cursor = line }


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
                    ( { m | shown = Shown { target = t, png = png }, note = "" }, OutNone )

                ShotStarting why ->
                    ( hold why { m | retrySeq = m.retrySeq + 1 }, OutRetryLater (m.retrySeq + 1) )

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


{-| 頼み直しの予約が来た(古い予約は捨てる)。 -}
retryFired : Int -> Model -> ( Model, Out )
retryFired seq model =
    if seq /= model.retrySeq || model.request /= Idle then
        ( model, OutNone )

    else
        askCursor True model


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
                    D.map2
                        (\starting why ->
                            if starting then
                                ShotStarting why

                            else
                                ShotFailed why
                        )
                        (D.oneOf [ D.field "starting" D.bool, D.succeed False ])
                        (D.oneOf [ D.at [ "error", "message" ] D.string, D.succeed "撮れませんでした" ])
            )



-- 画面


lineHeight : Int
lineHeight =
    20


view : Model -> Html Msg
view model =
    div [ HA.class "talk flex min-h-0 flex-1 gap-4 px-6 pb-6 pt-4" ]
        [ div [ HA.class "flex min-h-0 min-w-0 flex-1 flex-col gap-2" ]
            [ viewHead model
            , viewNodes model
            , viewEditor model
            , viewProblems model
            ]
        , div [ HA.class "flex min-h-0 w-[48%] flex-col gap-2" ]
            [ viewPreview model ]
        ]


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
            model.request /= Idle
    in
    div [ HA.class "flex flex-col gap-2" ]
        [ div [ HA.class "flex items-center gap-2" ]
            [ span [ HA.class "font-mono text-xs text-ink" ] [ text caption ]
            , if busy then
                span [ HA.class "text-[11px] text-ink-faint" ] [ text "⏳ 撮っています…" ]

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
        , div [ HA.class "text-[11px] text-ink-faint" ]
            [ text "右の絵はゲームが窓なしで描いた本物の画面です(起動中のゲームは動かしません)。保存すると撮り直します。" ]
        ]
