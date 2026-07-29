module GoldenView exposing
    ( Handlers
    , Mode(..)
    , Model
    , close
    , diffCanvasId
    , init
    , isOpen
    , open
    , select
    , selectedItem
    , setMode
    , setOpacity
    , view
    , withStatus
    )

{-| 焼き直した絵と音が「前と同じか」を見る窓(golden 差分ビューア)。

正(golden)と今(gallery / assets)を並べ、1 枚ずつ確かめて祝福する。
自動では 1 枚も直さない — 「変わってよかった変化」かどうかは人にしか決められない。

見比べ方は 3 つ: 並置(離れた 2 枚)・重ね(透過スライダで往復)・差分(画素の違いを
色で塗る)。どれも同じ 2 枚を指しているだけなので、行き来しても選び直しは要らない。

音は波形を持たないので、正と今をそれぞれ ▶ で聴き比べる。

-}

import Api
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes as HA
import Html.Events as HE


type Mode
    = SideBySide
    | Overlay
    | Diff


type alias Model =
    { open : Bool
    , status : Maybe Api.GoldenStatus
    , selected : Maybe String
    , mode : Mode

    -- 重ねの透過(0 = 正だけ・1 = 今だけ)
    , opacity : Float
    }


type alias Handlers msg =
    { onSelect : String -> msg
    , onMode : Mode -> msg
    , onOpacity : String -> msg
    , onBless : Api.GoldenItem -> msg
    , onPlay : { name : String, dir : String } -> msg
    , onClose : msg
    }


init : Model
init =
    { open = False, status = Nothing, selected = Nothing, mode = SideBySide, opacity = 0.5 }


isOpen : Model -> Bool
isOpen model =
    model.open


open : Model -> Model
open model =
    { model | open = True }


close : Model -> Model
close model =
    { model | open = False }


{-| 一覧が届いた。選びっぱなしの名前が消えていたら、割れている先頭へ移す。 -}
withStatus : Api.GoldenStatus -> Model -> Model
withStatus status model =
    let
        names =
            status.items |> List.map .name

        keep =
            case model.selected of
                Just name ->
                    if List.member name names then
                        Just name

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    { model
        | status = Just status
        , selected =
            case keep of
                Just name ->
                    Just name

                Nothing ->
                    status.items |> List.filter (\item -> not item.match) |> List.head |> Maybe.map .name
    }


select : String -> Model -> Model
select name model =
    { model | selected = Just name }


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


setOpacity : String -> Model -> Model
setOpacity text_ model =
    { model | opacity = String.toFloat text_ |> Maybe.withDefault model.opacity }


selectedItem : Model -> Maybe Api.GoldenItem
selectedItem model =
    case ( model.status, model.selected ) of
        ( Just status, Just name ) ->
            status.items |> List.filter (\item -> item.name == name) |> List.head

        _ ->
            Nothing


{-| 差分を塗る画布の名指し(絵を重ねて画素の違いを出すのは JS の仕事)。 -}
diffCanvasId : String
diffCanvasId =
    "golden-diff-canvas"


{-| 見比べの窓。images は「正の URL」「今の URL」を作る関数で受ける —
URL の組み方(サーバの口)を知るのは呼び側の仕事。
-}
view : Handlers msg -> { golden : Api.GoldenItem -> String, baked : Api.GoldenItem -> String } -> Model -> Html msg
view handlers urls model =
    if not model.open then
        text ""

    else
        div
            [ HA.class "golden-layer fixed inset-0 z-50 flex flex-col bg-app" ]
            [ viewHead handlers model
            , case model.status of
                Just status ->
                    if List.isEmpty status.items then
                        viewEmpty "見張る絵も音もありません(golden/ に焼き上がりを置くと、ここで見比べられます)"

                    else
                        div [ HA.class "flex min-h-0 flex-1" ]
                            [ viewList handlers status model
                            , viewCompare handlers urls model
                            ]

                Nothing ->
                    viewEmpty "読み込んでいます…"
            ]


viewHead : Handlers msg -> Model -> Html msg
viewHead handlers model =
    let
        summary =
            case model.status of
                Just status ->
                    if status.broken == 0 then
                        String.fromInt status.total ++ " / " ++ String.fromInt status.total ++ " 一致"

                    else
                        String.fromInt status.broken
                            ++ " 件 割れています("
                            ++ String.fromInt (status.total - status.broken)
                            ++ " / "
                            ++ String.fromInt status.total
                            ++ " 一致)"

                Nothing ->
                    ""
    in
    div [ HA.class "flex h-9 shrink-0 items-center gap-3 border-b border-edge bg-panel px-3" ]
        [ span [ HA.class "text-xs font-semibold text-ink" ] [ text "焼き上がりの見比べ" ]
        , span
            [ HA.classList
                [ ( "golden-summary text-[11px]", True )
                , ( "text-ok", (model.status |> Maybe.map (\s -> s.broken == 0)) == Just True )
                , ( "text-amber-300", (model.status |> Maybe.map (\s -> s.broken > 0)) == Just True )
                ]
            ]
            [ text summary ]
        , span [ HA.class "flex-1" ] []
        , button [ HA.class "golden-close btn btn-mini", HE.onClick handlers.onClose ] [ text "閉じる(Esc)" ]
        ]


viewEmpty : String -> Html msg
viewEmpty message =
    div [ HA.class "m-auto text-[11px] text-ink-faint" ] [ text message ]


{-| 割れている物を先に、一致は後ろに。日数は「見つけてから」で数える。 -}
viewList : Handlers msg -> Api.GoldenStatus -> Model -> Html msg
viewList handlers status model =
    let
        row item =
            button
                [ HA.classList
                    [ ( "golden-row flex w-full cursor-pointer items-center gap-1.5 px-3 py-1 text-left font-mono text-[11px]", True )
                    , ( "on bg-accent/15 text-ink", model.selected == Just item.name )
                    , ( "text-ink-soft hover:bg-white/5", model.selected /= Just item.name )
                    ]
                , HE.onClick (handlers.onSelect item.name)
                ]
                [ span
                    [ HA.class
                        (if item.match then
                            "text-ok"

                         else
                            "text-amber-300"
                        )
                    ]
                    [ text
                        (if item.match then
                            "✓"

                         else
                            "⚠"
                        )
                    ]
                , span [ HA.class "min-w-0 flex-1 truncate" ] [ text item.name ]
                , if item.match then
                    text ""

                  else
                    span [ HA.class "shrink-0 text-[10px] text-ink-faint" ]
                        [ text (brokenFor status.now item) ]
                ]

        ( broken, same ) =
            List.partition (\item -> not item.match) status.items
    in
    div [ HA.class "golden-list w-64 shrink-0 overflow-y-auto border-r border-edge bg-panel py-1" ]
        (List.map row broken ++ List.map row same)


{-| 割れて何日目か。1 日に満たない間は「今日」。 -}
brokenFor : Int -> Api.GoldenItem -> String
brokenFor now item =
    case item.since of
        Just since ->
            let
                days =
                    (now - since) // 86400
            in
            if days <= 0 then
                "今日から"

            else
                "割れて " ++ String.fromInt days ++ " 日"

        Nothing ->
            ""


viewCompare : Handlers msg -> { golden : Api.GoldenItem -> String, baked : Api.GoldenItem -> String } -> Model -> Html msg
viewCompare handlers urls model =
    case selectedItem model of
        Nothing ->
            viewEmpty "左の一覧から選んでください"

        Just item ->
            div [ HA.class "golden-compare flex min-w-0 flex-1 flex-col gap-2 overflow-auto p-3" ]
                (viewCompareHead handlers item
                    :: (if item.kind == "sound" then
                            viewSound handlers item

                        else
                            viewImage handlers urls model item
                       )
                )


viewCompareHead : Handlers msg -> Api.GoldenItem -> Html msg
viewCompareHead handlers item =
    div [ HA.class "flex flex-wrap items-center gap-2" ]
        [ span [ HA.class "font-mono text-xs text-ink" ] [ text item.name ]
        , if item.match then
            span [ HA.class "text-[11px] text-ok" ] [ text "一致しています" ]

          else
            button
                [ HA.class "golden-bless btn btn-mini"
                , HA.title "今の焼き上がりを新しい正にする(元には戻せません)"
                , HE.onClick (handlers.onBless item)
                ]
                [ text "この姿を正にする(祝福)" ]
        ]


{-| 絵の見比べ 3 通り。どれも同じ 2 枚を指しているだけ。 -}
viewImage : Handlers msg -> { golden : Api.GoldenItem -> String, baked : Api.GoldenItem -> String } -> Model -> Api.GoldenItem -> List (Html msg)
viewImage handlers urls model item =
    let
        tab mode label =
            button
                [ HA.classList
                    [ ( "golden-mode btn btn-mini", True )
                    , ( "bg-accent text-white hover:bg-accent", model.mode == mode )
                    ]
                , HE.onClick (handlers.onMode mode)
                ]
                [ text label ]

        shot label url =
            div [ HA.class "min-w-0 flex-1" ]
                [ div [ HA.class "mb-1 text-[10px] text-ink-faint" ] [ text label ]
                , img [ HA.class "golden-shot block w-full rounded border border-edge bg-well", HA.src url ] []
                ]
    in
    [ div [ HA.class "flex items-center gap-1.5" ]
        [ tab SideBySide "並べる"
        , tab Overlay "重ねる"
        , tab Diff "違いを塗る"
        , case model.mode of
            Overlay ->
                input
                    [ HA.class "golden-opacity ml-2 w-40"
                    , HA.type_ "range"
                    , HA.min "0"
                    , HA.max "1"
                    , HA.step "0.01"
                    , HA.value (String.fromFloat model.opacity)
                    , HE.onInput handlers.onOpacity
                    ]
                    []

            _ ->
                text ""
        ]
    , case model.mode of
        SideBySide ->
            div [ HA.class "flex gap-3" ]
                [ shot "正(golden)" (urls.golden item)
                , shot "今(焼き上がり)" (urls.baked item)
                ]

        Overlay ->
            div [ HA.class "relative" ]
                [ img [ HA.class "block w-full rounded border border-edge bg-well", HA.src (urls.golden item) ] []
                , img
                    [ HA.class "golden-overlay absolute inset-0 block w-full rounded"
                    , HA.src (urls.baked item)
                    , HA.style "opacity" (String.fromFloat model.opacity)
                    ]
                    []
                ]

        Diff ->
            div []
                [ Html.node "canvas"
                    [ HA.class "golden-diff block w-full rounded border border-edge bg-well"
                    , HA.id diffCanvasId
                    ]
                    []
                , div [ HA.class "mt-1 text-[10px] text-ink-faint" ]
                    [ text "違うところだけを色で塗ります(同じところは暗いまま)" ]
                ]
    ]


{-| 音は波形を持たないので、正と今をそれぞれ鳴らして聴き比べる。 -}
viewSound : Handlers msg -> Api.GoldenItem -> List (Html msg)
viewSound handlers item =
    let
        play label dir =
            button
                [ HA.class "golden-play btn btn-mini"
                , HE.onClick (handlers.onPlay { name = item.name, dir = dir })
                ]
                [ text label ]
    in
    [ div [ HA.class "flex items-center gap-2" ]
        [ play "▶ 正(golden)" "golden/sfx"
        , play "▶ 今(焼き上がり)" "assets/sfx"
        ]
    , div [ HA.class "text-[10px] text-ink-faint" ]
        [ text "同じ名前の音を、置き場を分けて聴き比べます(素の WAV のまま)" ]
    ]
