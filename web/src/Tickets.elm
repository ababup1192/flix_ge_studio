module Tickets exposing
    ( Model
    , Msg(..)
    , Out(..)
    , Ticket
    , buildTicketPrompt
    , gotList
    , init
    , listDecoder
    , listFailed
    , update
    , view
    )

{-| 違和感チケット — ゲーム内で切った注釈(debug/annotations/)に一言を添えて AI に運ぶパネル。

チケットの中身(world.json 等)は解釈しない。読むのはタイトルと一言だけで、あとはパスを運ぶ。
サーバ往復は Main の仕事(Out で頼む)。

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Url


{-| チケット 1 枚。title は自動で付いた見出し、comment は人の一言(未記入は "")。
isSketch はラフと見比べて気づいた物(遊んでいて気づいた物と手がかりが違う)。
-}
type alias Ticket =
    { id : String
    , title : String
    , comment : String
    , hasShot : Bool
    , isSketch : Bool
    }


type alias Model =
    { tickets : List Ticket

    -- 口を持たないサーバではパネルごと畳む(fail-open)
    , available : Bool

    -- 書きかけの一言(id → 文言)。保存はフォーカスが外れた時
    , drafts : Dict String String

    -- 直前に依頼文をコピーしたチケット(ボタンに ✓ を出す)
    , copiedId : Maybe String

    -- スクショを拡大表示中のチケット(Nothing = 閉じている)
    , zoomId : Maybe String
    }


init : Model
init =
    { tickets = [], available = True, drafts = Dict.empty, copiedId = Nothing, zoomId = Nothing }


{-| 一覧が届いた。サーバの内容と同じになった書きかけは捨てる(保存済みの印)。
まだ違う書きかけは残す — 取得と入力が重なっても打ちかけを失わない。
-}
gotList : List Ticket -> Model -> Model
gotList tickets model =
    let
        savedIds =
            -- 保存はトリムして送るので、比べる側もトリムして揃える(空白差で draft が残留しない)
            tickets
                |> List.filter
                    (\t -> (Dict.get t.id model.drafts |> Maybe.map String.trim) == Just t.comment)
                |> List.map .id
    in
    { model
        | tickets = tickets
        , available = True
        , drafts = List.foldl Dict.remove model.drafts savedIds
    }


{-| 一覧が取れなかった(404 の旧サーバ等)。パネルを出さないだけ。 -}
listFailed : Model -> Model
listFailed model =
    { model | available = False, tickets = [] }


type Msg
    = CommentEdited String String
    | CommentBlurred Ticket
    | ReportClicked Ticket
    | ArchiveClicked Ticket
    | ShotClicked Ticket
    | ZoomClosed


type Out
    = OutNone
    | OutSaveComment { id : String, comment : String }
    | OutArchive String
    | OutCopyPrompt String


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        CommentEdited id draft ->
            ( { model | drafts = Dict.insert id draft model.drafts }, OutNone )

        CommentBlurred ticket ->
            let
                draft =
                    commentOf model ticket |> String.trim
            in
            if draft == ticket.comment then
                ( model, OutNone )

            else
                ( model, OutSaveComment { id = ticket.id, comment = draft } )

        ReportClicked ticket ->
            let
                comment =
                    commentOf model ticket |> String.trim
            in
            if comment == "" then
                ( model, OutNone )

            else
                ( { model | copiedId = Just ticket.id }
                , OutCopyPrompt
                    (buildTicketPrompt
                        { id = ticket.id, comment = comment, isSketch = ticket.isSketch }
                    )
                )

        ArchiveClicked ticket ->
            ( model, OutArchive ticket.id )

        ShotClicked ticket ->
            ( { model | zoomId = Just ticket.id }, OutNone )

        ZoomClosed ->
            ( { model | zoomId = Nothing }, OutNone )


{-| いま画面に見えている一言(書きかけがあればそちら、無ければ保存済み)。 -}
commentOf : Model -> Ticket -> String
commentOf model ticket =
    Dict.get ticket.id model.drafts |> Maybe.withDefault ticket.comment


{-| AI へ渡す依頼文。一言とチケット内ファイルの場所だけを運び、
直し先(コードか Doc か)の判断は AI に委ねる。

手がかりは切った場所で変わる — 遊んでいて切った物にはその瞬間のゲーム状態が付き、
ラフと見比べて切った物には見比べた 2 枚の置き場が付く。

-}
buildTicketPrompt : { id : String, comment : String, isSketch : Bool } -> String
buildTicketPrompt info =
    let
        dir =
            "debug/annotations/" ++ info.id

        ( lead, clues ) =
            if info.isSketch then
                ( "【見た目の直し】生成された絵をラフと見比べて気になった所があります。"
                , [ "- " ++ dir ++ "/README.md(見比べた 2 枚の置き場と、指したマス)"
                  , "- " ++ dir ++ "/screenshot.png(見比べていた生成された絵)"
                  ]
                )

            else
                ( "【違和感の直し】遊んでいて気になった所があります。"
                , [ "- " ++ dir ++ "/README.md(囲った場所と、そこに描かれていた物の一覧)"
                  , "- " ++ dir ++ "/highlighted.png(気になる場所を赤枠で囲ったスクショ)"
                  , "- " ++ dir ++ "/world.json(その瞬間のゲーム状態のダンプ)"
                  ]
                )
    in
    String.join "\n"
        ([ lead
         , ""
         , "ひとこと: " ++ info.comment
         , ""
         , "手がかり(まずこれを開いてください。無い物は飛ばして構いません):"
         ]
            ++ clues
            ++ [ ""
               , "直し先はあなたが選んでください(コードか Doc か、正しい層を)。"
               , "見た目や数値の調整なら Doc、ルールや生成の変更ならコードです。"
               , "直したら make check と make test を通してください。"
               ]
        )



{-| 見出しを本体と日時の 2 行に分ける。engine の自動タイトルは
「注釈: <題名> — frame N（日時）」の形なので、丸かっこの中身を日時の行にする。
形が違う見出し(手で直した等)はそのまま 1 行目に全部出す。
-}
titleLines : String -> { head : String, stamp : Maybe String }
titleLines title =
    case String.split "（" title of
        [ head, rest ] ->
            { head = String.trim head
            , stamp =
                rest
                    |> String.split "）"
                    |> List.head
                    |> Maybe.map String.trim
            }

        _ ->
            { head = title, stamp = Nothing }



-- デコーダ


listDecoder : D.Decoder (List Ticket)
listDecoder =
    D.field "tickets" (D.list ticketDecoder)


ticketDecoder : D.Decoder Ticket
ticketDecoder =
    D.map5 Ticket
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.oneOf [ D.field "comment" D.string, D.succeed "" ])
        (D.oneOf [ D.field "hasShot" D.bool, D.succeed False ])
        -- 印を持たない古いサーバでは、全部「遊んでいて気づいた物」として扱う
        (D.oneOf [ D.field "kind" D.string |> D.map (\kind -> kind == "sketch"), D.succeed False ])



-- 画面


{-| チケットが無ければ何も出さない(ホームを散らかさない)。 -}
view : { serverBase : String } -> Model -> Html Msg
view ctx model =
    if not model.available || List.isEmpty model.tickets then
        text ""

    else
        div [ HA.class "tickets mx-auto mt-6 w-full max-w-lg px-4" ]
            [ div [ HA.class "rounded-lg border border-edge bg-panel p-5" ]
                (div [ HA.class "flex items-baseline gap-2" ]
                    [ span [ HA.class "text-sm font-semibold text-ink" ]
                        [ text ("🎫 違和感チケット(" ++ String.fromInt (List.length model.tickets) ++ ")") ]
                    , span [ HA.class "text-[11px] text-ink-faint" ]
                        [ text "ゲーム内で一時停止 → 矩形で囲うと増えます" ]
                    ]
                    :: List.map (viewTicket ctx model) model.tickets
                )
            , viewZoom ctx model
            ]


viewTicket : { serverBase : String } -> Model -> Ticket -> Html Msg
viewTicket ctx model ticket =
    let
        comment =
            commentOf model ticket

        canReport =
            String.trim comment /= ""
    in
    div [ HA.class "mt-3 flex gap-3 rounded-md border border-edge p-3" ]
        [ viewShot ctx ticket
        , div [ HA.class "min-w-0 flex-1" ]
            [ -- 見出しは「どこの注釈か」と「いつ切ったか」の 2 行に分けて全文を見せる
              div [ HA.class "flex min-w-0 items-baseline gap-1.5" ]
                [ viewMark ticket
                , div [ HA.class "min-w-0 truncate text-xs font-semibold text-ink", HA.title ticket.title ]
                    [ text (titleLines ticket.title).head ]
                ]
            , case (titleLines ticket.title).stamp of
                Nothing ->
                    text ""

                Just stamp ->
                    div [ HA.class "text-[11px] text-ink-faint" ] [ text stamp ]
            , input
                [ HA.class "mt-1.5 w-full rounded border border-edge bg-transparent px-2 py-1 text-xs text-ink"
                , HA.placeholder "ここに不具合の内容を記述"
                , HA.value comment
                , HE.onInput (CommentEdited ticket.id)
                , HE.onBlur (CommentBlurred ticket)
                ]
                []
            , div [ HA.class "mt-2 flex items-center gap-3" ]
                [ button
                    [ HA.class "btn btn-primary text-xs"
                    , HA.disabled (not canReport)
                    , HA.title "一言とチケットの場所を依頼文にしてコピーします(Claude Code に貼ってください)"
                    , HE.onClick (ReportClicked ticket)
                    ]
                    [ text
                        (if model.copiedId == Just ticket.id then
                            "✓ コピーしました"

                         else
                            "📋 違和感を報告"
                        )
                    ]
                , button
                    [ HA.class "cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
                    , HA.title "済んだチケットを archive/ へ移して一覧から下げます(消しません)"
                    , HE.onClick (ArchiveClicked ticket)
                    ]
                    [ text "🗄 アーカイブ" ]
                ]
            ]
        ]


{-| どこで気づいたかの印。一覧に 2 種類が混ざるので、開く手がかりが違う事が
一目で分かるようにする。
-}
viewMark : Ticket -> Html Msg
viewMark ticket =
    if ticket.isSketch then
        span
            [ HA.class "ticket-mark shrink-0 rounded-sm bg-accent/20 px-1 text-[10px] text-ink-soft"
            , HA.title "ラフと見比べて気づいた事"
            ]
            [ text "ラフ比較" ]

    else
        span
            [ HA.class "ticket-mark shrink-0 rounded-sm bg-white/10 px-1 text-[10px] text-ink-faint"
            , HA.title "遊んでいて気づいた事"
            ]
            [ text "遊んで" ]


{-| チケットのスクショ(赤枠つき)。無いチケットは絵の枠ごと出さない。クリックで拡大。 -}
viewShot : { serverBase : String } -> Ticket -> Html Msg
viewShot ctx ticket =
    if ticket.hasShot then
        img
            [ HA.src (shotUrl ctx ticket.id)
            , HA.class "h-14 w-24 flex-none cursor-zoom-in rounded border border-edge object-cover"
            , HA.attribute "loading" "lazy"
            , HA.alt ""
            , HA.title "クリックで拡大"
            , HE.onClick (ShotClicked ticket)
            ]
            []

    else
        text ""


{-| スクショの拡大表示。どこをクリックしても閉じる。 -}
viewZoom : { serverBase : String } -> Model -> Html Msg
viewZoom ctx model =
    case model.zoomId of
        Nothing ->
            text ""

        Just ticketId ->
            div
                [ HA.class "ticket-zoom-layer fixed inset-0 z-50 flex cursor-zoom-out flex-col items-center justify-center gap-3 bg-black/85"
                , HE.onClick ZoomClosed
                ]
                [ img
                    [ HA.class "max-h-[82vh] max-w-[94vw] object-contain"
                    , HA.src (shotUrl ctx ticketId)
                    , HA.alt "チケットのスクショ"
                    ]
                    []
                , div [ HA.class "text-[11px] text-white/60" ] [ text "クリックで閉じる" ]
                ]


shotUrl : { serverBase : String } -> String -> String
shotUrl ctx ticketId =
    ctx.serverBase ++ "/annotations/shot?id=" ++ Url.percentEncode ticketId
