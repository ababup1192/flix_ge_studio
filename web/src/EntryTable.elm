module EntryTable exposing
    ( Handlers
    , State
    , UsageDicts
    , usageDicts
    , view
    , viewBox
    , viewCrudBar
    , viewUsagePop
    )

{-| 一覧セクション(catalog / list)の表ビュー。

並べ替え・絞り込みは表示の行リストにだけ効く。各行が論理の指し先(rowId)を
持っているので、クリック時はそこから EntrySel を作る — 表示位置は使わない。
フォーム(選択 1 件)は表示と独立で、絞り込みで行が隠れても選択は生きる。

画面(Main)からは Handlers(押された時に投げる便り)と State(表に効く
選択・並び・絞り込み)だけを受け取る。Model 丸ごとを知らないので、
2 ペイン常設や別画面から同じ表を出しても、ここは書き換わらない。

-}

import Dict
import Doc
import EntryOps
import Html exposing (Html, button, div, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Refs
import Schema
import Selection exposing (EntrySel(..))
import Sources
import Table


{-| 表の操作が投げる便り。Main の Msg をここへ持ち込まないための受け口。 -}
type alias Handlers msg =
    { onSelect : EntrySel -> msg
    , onSort : String -> msg
    , onFilter : String -> msg
    , onAdd : msg
    , onDuplicate : msg
    , onDelete : msg
    , onRowOp : String -> EntryOps.RowEdit -> msg
    , onRowDelete : String -> Int -> msg
    , onUsagesToggle : String -> msg
    , onUsageJump : { sectionKey : String, entry : Maybe EntrySel } -> msg
    , onExternalJump : { path : String, sectionKey : String, entry : EntrySel } -> msg
    }


{-| 表の見え方を決める今の状態(Model のうち表に効く 4 つだけ)。 -}
type alias State =
    { entrySel : Maybe EntrySel
    , sort : Maybe Table.SortState
    , filter : String
    , usagesOpenFor : Maybe String
    }


{-| 使用数の 2 冊: own = 開いている文書の中から、ext = 他ファイルから。 -}
type alias UsageDicts =
    { own : Dict.Dict String (List Refs.UsageSite)
    , ext : Dict.Dict String (List Sources.ExternalUsage)
    }


{-| 開いている文書のスキーマから両冊を組む(view のたびに全計算 — 問題一覧と同じ流儀)。 -}
usageDicts : Schema.Schema -> D.Value -> List Sources.SourceDoc -> UsageDicts
usageDicts schema doc others =
    { own = Refs.usages schema doc
    , ext = Sources.externalUsages others
    }


{-| 操作列+表+使用箇所の一覧(分割モードの 1 セクションぶん)。 -}
view : Handlers msg -> State -> D.Value -> String -> Schema.Section -> Maybe UsageDicts -> List (Html msg)
view handlers state doc key section usageDict =
    [ viewCrudBar handlers state doc key section
    , viewBox handlers state "max-h-64" doc key section usageDict
    ]
        ++ viewUsagePop handlers state key usageDict


{-| テーブル上部の操作列: 追加・複製・削除+絞り込み。ビジュアル完結の入口。
複製と削除は、このセクションに実在する行が選ばれている時だけ生きる。
-}
viewCrudBar : Handlers msg -> State -> D.Value -> String -> Schema.Section -> Html msg
viewCrudBar handlers state doc key section =
    let
        hasSelection =
            case ( section.kind, state.entrySel ) of
                ( Schema.Catalog, Just (ByKey name) ) ->
                    List.member name (Doc.catalogKeys key doc)

                ( Schema.ListKind, Just (ByIndex i) ) ->
                    i < List.length (Doc.list key doc)

                _ ->
                    False
    in
    -- ボタンの格: 追加=secondary / 複製・削除=tertiary(テキスト系)。
    -- 削除の確定(danger)はダイアログ側の持ち場
    div [ HA.class "crud-bar mb-1.5 flex shrink-0 items-center gap-1.5" ]
        [ button [ HA.class "crud-add btn", HE.onClick handlers.onAdd ] [ text "+ 追加" ]
        , button [ HA.class "crud-dup btn btn-ghost", HE.onClick handlers.onDuplicate, HA.disabled (not hasSelection) ] [ text "複製" ]
        , button [ HA.class "crud-delete btn btn-ghost hover:text-danger", HE.onClick handlers.onDelete, HA.disabled (not hasSelection) ] [ text "削除" ]
        , input
            [ HA.class "table-filter field ml-auto w-40 min-w-0"
            , HA.type_ "text"
            , HA.placeholder "絞り込み"
            , HA.value state.filter
            , HE.onInput handlers.onFilter
            ]
            []
        ]


viewBox : Handlers msg -> State -> String -> D.Value -> String -> Schema.Section -> Maybe UsageDicts -> Html msg
viewBox handlers state heightClass doc key section usageDict =
    let
        cols =
            Table.columns section

        shownRows =
            Table.rows key section doc
                |> Table.filterRows state.filter
                |> Table.sortRows state.sort

        idLabel =
            case section.kind of
                Schema.Catalog ->
                    "id"

                _ ->
                    "#"

        numericCols =
            cols |> List.filter .numeric |> List.map .name

        -- 行操作は一覧(list)だけ。並べ替え表示中の ↑↓ は「見えている隣」と
        -- 動く先が食い違うので止める(絞り込みと違い、順序そのものが別物になる)
        rowOps =
            case section.kind of
                Schema.ListKind ->
                    Just
                        { key = key
                        , count = List.length (Doc.list key doc)
                        , ordered = state.sort == Nothing
                        }

                _ ->
                    Nothing

        rowOpsHeader =
            case rowOps of
                Just _ ->
                    [ th [ HA.class "sticky top-0 z-10 border-b border-edge bg-raised px-2 py-1 text-left font-normal text-ink-soft select-none" ] [ text "操作" ] ]

                Nothing ->
                    []

        -- 使用列は catalog だけ(list/record の行は id を持たず参照されない)
        usageHeader =
            case usageDict of
                Just _ ->
                    [ th [ HA.class "sticky top-0 z-10 border-b border-edge bg-raised px-2 py-1 text-left font-normal text-ink-soft select-none" ] [ text "使用" ] ]

                Nothing ->
                    []
    in
    div [ HA.class ("table-wrap mb-2.5 overflow-auto rounded border border-edge " ++ heightClass) ]
        [ table [ HA.class "entry-table w-full border-collapse font-mono text-[11px] tabular-nums whitespace-nowrap" ]
            [ thead []
                [ tr []
                    ((viewHeaderCell handlers state.sort Table.idColumn idLabel False
                        :: (cols |> List.map (\c -> viewHeaderCell handlers state.sort c.name c.label c.numeric))
                     )
                        ++ usageHeader
                        ++ rowOpsHeader
                    )
                ]
            , tbody []
                (shownRows
                    |> List.map (viewTableRow handlers state.entrySel numericCols (usageDict |> Maybe.map (\dict -> ( key, dict ))) rowOps)
                )
            ]
        ]


{-| 使用箇所の一覧(バッジをクリックで開く小さなリスト)。項目クリックで
問題ジャンプと同じ経路(セクション+エントリ選択)へ飛ぶ。
-}
viewUsagePop : Handlers msg -> State -> String -> Maybe UsageDicts -> List (Html msg)
viewUsagePop handlers state key usageDict =
    case ( state.usagesOpenFor, usageDict ) of
        ( Just openKey, Just dicts ) ->
            if String.startsWith (key ++ "/") openKey then
                let
                    id =
                        String.dropLeft (String.length key + 1) openKey

                    sites =
                        Dict.get openKey dicts.own |> Maybe.withDefault []

                    extSites =
                        Dict.get openKey dicts.ext |> Maybe.withDefault []

                    total =
                        List.length sites + List.length extSites
                in
                [ div [ HA.class "usage-pop mb-2.5 rounded border border-edge bg-raised p-2" ]
                    (div [ HA.class "usage-pop-head mb-1 text-[11px] text-ink-soft" ]
                        [ text ("\"" ++ id ++ "\" の使用箇所(" ++ String.fromInt total ++ ")") ]
                        :: (sites |> List.map (viewUsageSite handlers))
                        ++ (extSites |> List.map (viewExternalUsageSite handlers))
                    )
                ]

            else
                []

        _ ->
            []


viewUsageSite : Handlers msg -> Refs.UsageSite -> Html msg
viewUsageSite handlers site =
    button
        [ HA.class "usage-site block w-full cursor-pointer rounded px-1.5 py-0.5 text-left font-mono text-[11px] text-ink-soft hover:bg-white/5 hover:text-ink"
        , HE.onClick
            (handlers.onUsageJump
                { sectionKey = site.sectionKey
                , entry = site.entry |> Maybe.map Selection.fromRefsEntry
                }
            )
        ]
        [ text (Refs.siteLabel site) ]


{-| 他ファイルからの使用箇所 1 行。クリックでそのファイルを開いて該当エントリを選ぶ
(dirty の関所はジャンプ側が通す)。
-}
viewExternalUsageSite : Handlers msg -> Sources.ExternalUsage -> Html msg
viewExternalUsageSite handlers usage =
    button
        [ HA.class "usage-site usage-site-ext block w-full cursor-pointer rounded px-1.5 py-0.5 text-left font-mono text-[11px] text-ink-soft hover:bg-white/5 hover:text-ink"
        , HE.onClick
            (handlers.onExternalJump
                { path = usage.path
                , sectionKey = usage.site.sectionKey
                , entry =
                    usage.site.entry
                        |> Maybe.map Selection.fromRefsEntry
                        |> Maybe.withDefault (ByKey "")
                }
            )
        ]
        [ text (usage.path ++ ": " ++ Refs.siteLabel usage.site) ]


viewHeaderCell : Handlers msg -> Maybe Table.SortState -> String -> String -> Bool -> Html msg
viewHeaderCell handlers sort column label numeric =
    let
        marker =
            case sort of
                Just st ->
                    if st.column == column then
                        case st.dir of
                            Table.Asc ->
                                " ▲"

                            Table.Desc ->
                                " ▼"

                    else
                        ""

                Nothing ->
                    ""
    in
    th
        [ HA.classList
            [ ( "sticky top-0 z-10 cursor-pointer border-b border-edge bg-raised px-2 py-1 font-normal text-ink-soft select-none hover:text-ink", True )
            , ( "text-right", numeric )
            , ( "text-left", not numeric )
            ]
        , HE.onClick (handlers.onSort column)
        ]
        [ text (label ++ marker) ]


{-| 一覧の行操作(↑ ↓ 複製 ✕)を出すための材料。catalog の行には出さない
(並び順が意味を持たず、id 付きの追加・複製は上の操作列の持ち場)。
-}
type alias RowOps =
    { key : String
    , count : Int
    , ordered : Bool
    }


viewTableRow : Handlers msg -> Maybe EntrySel -> List String -> Maybe ( String, UsageDicts ) -> Maybe RowOps -> Table.TableRow -> Html msg
viewTableRow handlers entrySel numericCols catalogUsage rowOps row =
    let
        sel =
            case row.rowId of
                Table.ById name ->
                    ByKey name

                Table.ByIdx i ->
                    ByIndex i

        isOn =
            entrySel == Just sel
    in
    tr
        [ HA.classList
            [ ( "cursor-pointer", True )

            -- 選択は hover と見間違えない濃さ+左の色帯で示す
            , ( "on bg-accent/20 shadow-[inset_2px_0_0_var(--color-accent)]", isOn )
            , ( "hover:bg-white/5", not isOn )
            ]
        , HE.onClick (handlers.onSelect sel)
        ]
        ((td
            [ HA.classList
                [ ( "id-cell border-t border-edge px-2 py-0.5", True )
                , ( "text-accent", isOn )
                , ( "text-ink-faint", not isOn )
                ]
            ]
            (text row.idText :: viewRowSummary rowOps row.summary)
            :: (row.cells
                    |> List.map
                        (\c ->
                            td
                                [ HA.classList
                                    [ ( "border-t border-edge px-2 py-0.5", True )
                                    , ( "text-right", List.member c.column numericCols )
                                    ]
                                ]
                                [ text c.text ]
                        )
               )
         )
            ++ viewUsageCell handlers catalogUsage row.rowId
            ++ viewRowOpsCell handlers rowOps row.rowId
        )


{-| 番号だけの行に中身の見当を添える(盤面の行一覧と同じ規則の 1 行見出し)。
id を持つ catalog の行には足さない — 名前が既に見出しなので。
-}
viewRowSummary : Maybe RowOps -> String -> List (Html msg)
viewRowSummary rowOps summary =
    case ( rowOps, summary ) of
        ( Just _, "" ) ->
            []

        ( Just _, _ ) ->
            [ span
                [ HA.class "row-summary ml-1.5 inline-block max-w-40 truncate align-bottom text-ink-faint"
                , HA.title summary
                ]
                [ text summary ]
            ]

        ( Nothing, _ ) ->
            []


{-| 行操作のセル。クリックは行選択に食わせない(順番を直す操作と行を選ぶ操作は別)。
どのボタンも呼び側の編集直列(EditPayload の一本道)へ流れる。
-}
viewRowOpsCell : Handlers msg -> Maybe RowOps -> Table.RowId -> List (Html msg)
viewRowOpsCell handlers rowOps rowId =
    case ( rowOps, rowId ) of
        ( Just ops, Table.ByIdx i ) ->
            let
                opButton className title_ enabled msg label =
                    button
                        [ HA.class ("btn btn-ghost btn-mini shrink-0 " ++ className)
                        , HA.title title_
                        , HA.disabled (not enabled)
                        , HE.stopPropagationOn "click" (D.succeed ( msg, True ))
                        ]
                        [ text label ]
            in
            [ td [ HA.class "row-ops border-t border-edge px-1 py-0.5 whitespace-nowrap" ]
                [ opButton "row-up text-ink-faint hover:text-ink" "1 つ上へ" (ops.ordered && i > 0) (handlers.onRowOp ops.key (EntryOps.MoveRow i -1)) "↑"
                , opButton "row-down text-ink-faint hover:text-ink" "1 つ下へ" (ops.ordered && i < ops.count - 1) (handlers.onRowOp ops.key (EntryOps.MoveRow i 1)) "↓"
                , opButton "row-dup text-ink-faint hover:text-ink" "この行の直後へ複製" True (handlers.onRowOp ops.key (EntryOps.DuplicateRow i)) "複製"
                , opButton "row-delete text-ink-faint hover:text-danger" "この行を削除" True (handlers.onRowDelete ops.key i) "✕"
                ]
            ]

        _ ->
            []


{-| 使用数のセル。0 は薄い数字、1 以上はクリックで一覧が開くバッジ。
クリックは行選択に食わせない — 数を見る操作と行を選ぶ操作は別。
-}
viewUsageCell : Handlers msg -> Maybe ( String, UsageDicts ) -> Table.RowId -> List (Html msg)
viewUsageCell handlers catalogUsage rowId =
    case ( catalogUsage, rowId ) of
        ( Just ( key, dicts ), Table.ById name ) ->
            let
                ukey =
                    Refs.usageKey key name

                count =
                    (Dict.get ukey dicts.own |> Maybe.map List.length |> Maybe.withDefault 0)
                        + (Dict.get ukey dicts.ext |> Maybe.map List.length |> Maybe.withDefault 0)
            in
            [ td [ HA.class "usage-cell border-t border-edge px-2 py-0.5" ]
                [ if count == 0 then
                    span [ HA.class "usage-zero text-ink-faint" ] [ text "0" ]

                  else
                    button
                        [ HA.class "usage-badge badge cursor-pointer bg-accent/15 font-semibold text-accent hover:bg-accent/25"
                        , HE.stopPropagationOn "click" (D.succeed ( handlers.onUsagesToggle ukey, True ))
                        ]
                        [ text (String.fromInt count) ]
                ]
            ]

        _ ->
            []
