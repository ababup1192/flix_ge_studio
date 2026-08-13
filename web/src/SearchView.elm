module SearchView exposing
    ( Handlers
    , Model
    , Row(..)
    , activeRow
    , close
    , init
    , isOpen
    , moveSelection
    , open
    , plan
    , replacedValue
    , rowDomId
    , selectedDomId
    , typedQuery
    , typedReplacement
    , view
    , withResults
    )

{-| ⌘⇧F の横断検索と置換のパネル。

探すのはサーバの仕事(宣言済み Doc の文字列値を単純一致)。ここは
「打った物をいつ送るか」「見つかった物をどう見せるか」「置換すると何がどう
変わるか」を持つ。

置換の対象は**文字列値だけ**。キー名や数値は動かさない — キーを書き換えると
参照が宙に浮き、数値は「1」を含む値が芋づるで壊れる。文字列の中の一致だけなら、
変わるのは見えている文字そのものになる。

打鍵のたびに投げないのは、大きな文書を持つプロジェクトで打つたびに全走査が
走ると画面が固まるため(150ms 置いてから最後の 1 回だけ送る)。

結果の選び方は IDE の作法に合わせる: 探した直後は先頭が選ばれていて、
↓ / ↑ で動き、Enter でその行を開く(押した時と同じ動き)。マウスを乗せただけでは
選択は動かない — 目で追う位置とキーの位置が食い違うと、Enter の行き先が読めなくなる。

-}

import Api
import Edit exposing (Seg(..))
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { open : Bool
    , query : String
    , replacement : String

    -- デバウンスの世代。予約が発火した時、まだ最新かを確かめる照合キー
    , seq : Int
    , results : Api.SearchResults
    , searching : Bool

    -- ファイル群 → 中身群を 1 本に繋いだ列の何番目を選んでいるか
    , selected : Int
    }


type alias Handlers msg =
    { onQuery : String -> msg
    , onReplacement : String -> msg
    , onFile : String -> msg
    , onHit : Api.SearchHit -> msg
    , onMove : Int -> msg
    , onActivate : msg
    , onReplaceRun : msg
    , onClose : msg
    }


init : Model
init =
    { open = False
    , query = ""
    , replacement = ""
    , seq = 0
    , results = { files = [], filesTotal = 0, hits = [], total = 0, truncated = False }
    , searching = False
    , selected = 0
    }


isOpen : Model -> Bool
isOpen model =
    model.open


{-| 開く(前の結果は残す — 続きを探すことが多い)。 -}
open : Model -> Model
open model =
    { model | open = True }


close : Model -> Model
close model =
    { model | open = False }


{-| 打った(世代を進めて、その世代の予約だけが送る)。 -}
typedQuery : String -> Model -> Model
typedQuery query model =
    { model | query = query, seq = model.seq + 1, searching = query /= "", selected = 0 }


typedReplacement : String -> Model -> Model
typedReplacement replacement model =
    { model | replacement = replacement }


withResults : Api.SearchResults -> Model -> Model
withResults results model =
    -- 探し直したら選択は先頭へ(前の結果の 5 行目を選んだまま別の列を見せない)
    { model | results = results, searching = False, selected = 0 }


{-| 結果の 1 行。ファイル名の当たりと、中身の当たりを 1 本の列として扱う。 -}
type Row
    = FileRow String
    | HitRow Api.SearchHit


rows : Model -> List Row
rows model =
    List.map FileRow model.results.files ++ List.map HitRow model.results.hits


{-| ↓ / ↑ で選択を動かす(端で止まる — 巻き戻ると行き過ぎに気づけない)。 -}
moveSelection : Int -> Model -> Model
moveSelection dir model =
    let
        count =
            List.length (rows model)
    in
    if count == 0 then
        { model | selected = 0 }

    else
        { model | selected = clamp 0 (count - 1) (model.selected + dir) }


{-| Enter で開く行。選択が列の外(結果が入れ替わった直後等)なら先頭。 -}
activeRow : Model -> Maybe Row
activeRow model =
    let
        list =
            rows model
    in
    case List.drop model.selected list |> List.head of
        Just row ->
            Just row

        Nothing ->
            List.head list


{-| 選択行の名指し(追従スクロールの頼み事に使う)。 -}
rowDomId : Int -> String
rowDomId index =
    "search-row-" ++ String.fromInt index


selectedDomId : Model -> String
selectedDomId model =
    rowDomId model.selected


{-| 置換後の値。見つけた文字を、そのままの回数だけ置き換える。 -}
replacedValue : String -> String -> String -> String
replacedValue query replacement value =
    if query == "" then
        value

    else
        String.replace query replacement value


{-| 置換の書き戻し計画: ファイルごとに「その場所へ新しい文字列を書く」編集の列。
パスを持たない当たり(名前だけの照合)は対象外 — 中身を見ていない物は触らない。
順序は結果の順のまま(同じファイルの編集がばらけない)。
-}
plan : Model -> List { file : String, edits : List Edit.Payload }
plan model =
    model.results.hits
        |> List.filter (\hit -> not (List.isEmpty hit.path))
        |> List.foldl
            (\hit acc ->
                let
                    payload =
                        { op = Edit.SetOp
                        , path = hit.path
                        , value = E.string (replacedValue model.query model.replacement hit.value)
                        , isInt = False
                        }
                in
                if List.any (\group -> group.file == hit.file) acc then
                    acc
                        |> List.map
                            (\group ->
                                if group.file == hit.file then
                                    { group | edits = group.edits ++ [ payload ] }

                                else
                                    group
                            )

                else
                    acc ++ [ { file = hit.file, edits = [ payload ] } ]
            )
            []


view : Handlers msg -> Model -> Html msg
view handlers model =
    if not model.open then
        text ""

    else
        div [ HA.class "search-panel fixed top-12 left-1/2 z-50 w-[42rem] max-w-[92vw] -translate-x-1/2 rounded-lg border border-edge bg-panel p-3 shadow-[0_8px_32px_rgb(0_0_0/0.5)]" ]
            [ div [ HA.class "flex items-center gap-2" ]
                [ input
                    [ HA.class "search-query field min-w-0 flex-1"
                    , HA.type_ "text"
                    , HA.placeholder "すべての Doc から探す(⌘⇧F)"
                    , HA.value model.query
                    , HA.autofocus True
                    , HE.onInput handlers.onQuery
                    , onListKeys handlers
                    ]
                    []
                , button
                    [ HA.class "search-close btn btn-ghost btn-mini shrink-0"
                    , HA.title "閉じる(Esc)"
                    , HE.onClick handlers.onClose
                    ]
                    [ text "✕" ]
                ]
            , div [ HA.class "mt-1.5 flex items-center gap-2" ]
                [ input
                    [ HA.class "search-replacement field min-w-0 flex-1"
                    , HA.type_ "text"
                    , HA.placeholder "置き換える文字(空のままなら探すだけ)"
                    , HA.value model.replacement
                    , HE.onInput handlers.onReplacement
                    ]
                    []
                , button
                    [ HA.class "search-replace btn shrink-0"
                    , HA.disabled (model.query == "" || List.isEmpty (plan model))
                    , HA.title "一覧の全部を置き換える(⌘Z で 1 回で戻せる)"
                    , HE.onClick handlers.onReplaceRun
                    ]
                    [ text "すべて置換" ]
                ]
            , viewCount model
            , div [ HA.class "search-results mt-1.5 max-h-[50vh] overflow-y-auto" ]
                (viewFileGroup handlers model
                    ++ (model.results.hits
                            |> List.indexedMap (\i hit -> viewHit handlers model (List.length model.results.files + i) hit)
                       )
                )
            ]


{-| 検索欄に居たまま結果を選ぶ(↓ / ↑)・開く(Enter)。
preventDefault は ↓↑ で欄のカーソルが端へ飛ぶのを止めるため。
-}
onListKeys : Handlers msg -> Html.Attribute msg
onListKeys handlers =
    HE.custom "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    case key of
                        "ArrowDown" ->
                            D.succeed { message = handlers.onMove 1, stopPropagation = True, preventDefault = True }

                        "ArrowUp" ->
                            D.succeed { message = handlers.onMove -1, stopPropagation = True, preventDefault = True }

                        "Enter" ->
                            D.succeed { message = handlers.onActivate, stopPropagation = True, preventDefault = True }

                        _ ->
                            D.fail "他のキーは素通し"
                )
        )


{-| 選択行の装い(押せる行はどれも同じ土台にこれを足す)。 -}
rowAttrs : Model -> Int -> List (Html.Attribute msg)
rowAttrs model index =
    [ HA.id (rowDomId index)
    , HA.classList
        [ ( "search-row block w-full cursor-pointer rounded px-1.5 py-1 text-left", True )
        , ( "on bg-accent/20", model.selected == index )
        , ( "hover:bg-white/5", model.selected /= index )
        ]
    ]


viewCount : Model -> Html msg
viewCount model =
    let
        shownHits =
            List.length model.results.hits

        counts =
            "ファイル "
                ++ String.fromInt model.results.filesTotal
                ++ " 件 / 中身 "
                ++ String.fromInt model.results.total
                ++ " 件"
    in
    div [ HA.class "search-count mt-1.5 text-[11px] text-ink-faint" ]
        [ if model.query == "" then
            text "文字を打つと探します(ファイル名と、文字列の値・大文字小文字は区別します)"

          else if model.searching then
            text "探しています…"

          else if model.results.total == 0 && model.results.filesTotal == 0 then
            text "見つかりませんでした"

          else if model.results.truncated then
            text (counts ++ "(表示は中身 " ++ String.fromInt shownHits ++ " 件まで)")

          else
            text counts
        ]


{-| ファイル名が一致したファイルの組。中身の当たりより先に、まとめて出す —
「map」で探した人が真っ先に見たいのは \*.map.json の一群だから。
押すとそのファイルを開くだけ(飛ぶ欄は無い)。
-}
viewFileGroup : Handlers msg -> Model -> List (Html msg)
viewFileGroup handlers model =
    if List.isEmpty model.results.files then
        []

    else
        div [ HA.class "search-group mt-0.5 mb-1 text-[10px] font-semibold tracking-[0.14em] text-ink-faint uppercase" ]
            [ text "ファイル" ]
            :: (model.results.files
                    |> List.indexedMap
                        (\index path ->
                            button
                                (HA.class "search-file"
                                    :: HE.onClick (handlers.onFile path)
                                    :: rowAttrs model index
                                )
                                [ span [ HA.class "font-mono text-[11px] text-ink" ] [ text path ] ]
                        )
               )
            ++ [ if List.isEmpty model.results.hits then
                    text ""

                 else
                    div [ HA.class "search-group mt-2 mb-1 text-[10px] font-semibold tracking-[0.14em] text-ink-faint uppercase" ]
                        [ text "中身" ]
               ]


viewHit : Handlers msg -> Model -> Int -> Api.SearchHit -> Html msg
viewHit handlers model index hit =
    let
        after =
            replacedValue model.query model.replacement hit.value
    in
    button
        (HA.class "search-hit"
            :: HE.onClick (handlers.onHit hit)
            :: rowAttrs model index
        )
        [ div [ HA.class "flex items-baseline gap-2" ]
            [ span [ HA.class "min-w-0 truncate font-mono text-[11px] text-ink" ] [ text hit.file ]
            , span [ HA.class "min-w-0 flex-1 truncate font-mono text-[10px] text-ink-faint" ]
                [ text (Edit.pathKey hit.path) ]
            ]
        , div [ HA.class "truncate text-[11px] text-ink-soft" ] [ text hit.excerpt ]
        , if model.replacement /= "" && after /= hit.value then
            -- 実行前のプレビュー: この欄がどう変わるか
            div [ HA.class "search-after truncate text-[11px] text-ok" ] [ text ("→ " ++ after) ]

          else
            text ""
        ]
