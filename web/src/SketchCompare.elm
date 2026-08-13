module SketchCompare exposing
    ( Draft(..)
    , Handlers
    , Mode(..)
    , Model
    , Post(..)
    , buildTicket
    , canSubmit
    , close
    , draftOf
    , hasSketches
    , init
    , isOpen
    , loadFailed
    , loaded
    , noteOf
    , open
    , pathOf
    , pendingPath
    , pickCell
    , pickedCell
    , postOf
    , selectScene
    , selectSketch
    , selectVersion
    , selectedScene
    , selectedSketch
    , selectedVersion
    , setMode
    , setNote
    , setOpacity
    , submitFailed
    , submitted
    , submitting
    , view
    , versionsOf
    , withScenes
    , withSketches
    )

{-| 生成された絵と、その元になったラフを人が選んで見比べるビュー。

どの絵がどのラフから来たかは絵に書かれていないので、機械では結び付けない —
左の 2 つの一覧から人が 1 枚ずつ指し、並べるか重ねるかで見比べるだけ。

「違いを塗る」は入れない。ラフはセルの塗り絵で、生成された絵と画素の位置が
そもそも合わないため、色の差を出しても読み取れる物にならない。

塗るための操作(Msg)は持たない。ラフの絵は読み取り専用にここで描く。

見比べて気づいた事は、セルを 1 つ指してひとことを添え、遊んでいて切った物と
同じやること一覧へ並べる(ここ専用の一覧は作らない)。書けるのは見た目の話だけ。

-}

import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import SketchPad


type Mode
    = SideBySide
    | Overlay


{-| 選んだラフの中身。読みに行っている間・読めなかった時も画面に出したいので、
Maybe ではなく 4 つの姿で持つ。
-}
type Draft
    = DraftNone
    | DraftLoading
    | DraftReady SketchPad.Model
    | DraftBroken String


{-| ひとことをやること一覧へ並べる途中の姿。並べ終わった印は次に書き始めるまで残す。 -}
type Post
    = PostIdle
    | PostSending
    | PostDone
    | PostFailed String


type alias Model =
    { open : Bool

    -- 生成された絵の名前(gallery/ の PNG)
    , scenes : List String
    , sketches : List SketchPad.Sketch
    , scene : Maybe String
    , sketch : Maybe String
    , version : Maybe Int
    , draft : Draft
    , mode : Mode

    -- 重ねの透過(0 = 生成された絵だけ・1 = ラフだけ)
    , opacity : Float

    -- 指したセル(ラフの左上から数えた 0 始まりの位置)
    , cell : Maybe { x : Int, y : Int }
    , note : String
    , post : Post
    }


type alias Handlers msg =
    { onScene : String -> msg
    , onSketch : String -> msg
    , onVersion : Int -> msg
    , onMode : Mode -> msg
    , onOpacity : String -> msg
    , onCell : { x : Int, y : Int } -> msg
    , onNote : String -> msg
    , onSubmit : msg
    , onClose : msg
    }


init : Model
init =
    { open = False
    , scenes = []
    , sketches = []
    , scene = Nothing
    , sketch = Nothing
    , version = Nothing
    , draft = DraftNone
    , mode = SideBySide
    , opacity = 0.5
    , cell = Nothing
    , note = ""
    , post = PostIdle
    }


isOpen : Model -> Bool
isOpen model =
    model.open


open : Model -> Model
open model =
    { model | open = True }


close : Model -> Model
close model =
    { model | open = False }


hasSketches : Model -> Bool
hasSketches model =
    not (List.isEmpty model.sketches)


selectedScene : Model -> Maybe String
selectedScene model =
    model.scene


selectedSketch : Model -> Maybe String
selectedSketch model =
    model.sketch


selectedVersion : Model -> Maybe Int
selectedVersion model =
    model.version


draftOf : Model -> Draft
draftOf model =
    model.draft


{-| そのラフの持つバージョンを新しい順に。一覧の並びは当てにしない。 -}
versionsOf : String -> Model -> List Int
versionsOf name model =
    model.sketches
        |> List.filter (\sketch -> sketch.name == name)
        |> List.head
        |> Maybe.map (\sketch -> List.sortBy negate sketch.versions)
        |> Maybe.withDefault []


{-| 生成された絵の一覧が届いた。選びっぱなしの名前が消えていたら選び直しへ戻す —
勝手に別の絵へ移すと、見比べているつもりの物が黙って入れ替わる。
-}
withScenes : List String -> Model -> Model
withScenes scenes model =
    { model
        | scenes = scenes
        , scene =
            case model.scene of
                Just name ->
                    if List.member name scenes then
                        Just name

                    else
                        Nothing

                Nothing ->
                    Nothing
    }


{-| ラフの一覧が届いた。選びっぱなしのラフが消えていたら選び直しへ戻し、
バージョンだけが消えていたら最新へ寄せて読み直す(ラフ自体はまだ在るので)。
-}
withSketches : List SketchPad.Sketch -> Model -> Model
withSketches sketches model =
    let
        next =
            { model | sketches = sketches }
    in
    case model.sketch of
        Nothing ->
            next

        Just name ->
            let
                versions =
                    versionsOf name next
            in
            if List.isEmpty versions then
                { next | sketch = Nothing, version = Nothing, draft = DraftNone }

            else if List.member (model.version |> Maybe.withDefault 0) versions then
                next

            else
                forgetCell { next | version = List.head versions, draft = DraftLoading }


selectScene : String -> Model -> Model
selectScene name model =
    { model | scene = Just name }


{-| ラフを選ぶと最新のバージョンから見る。中身は親が読みに行く(pendingPath)。 -}
selectSketch : String -> Model -> Model
selectSketch name model =
    case List.head (versionsOf name model) of
        Just newest ->
            forgetCell { model | sketch = Just name, version = Just newest, draft = DraftLoading }

        Nothing ->
            forgetCell { model | sketch = Just name, version = Nothing, draft = DraftNone }


selectVersion : Int -> Model -> Model
selectVersion version model =
    forgetCell { model | version = Just version, draft = DraftLoading }


{-| 指したセルを忘れる。セルの数はラフごと・バージョンごとに違うので、
別のラフへ移った後も同じ位置を指したままだと、別の場所を指してしまう。
-}
forgetCell : Model -> Model
forgetCell model =
    { model | cell = Nothing, post = PostIdle }


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


setOpacity : String -> Model -> Model
setOpacity text_ model =
    { model | opacity = String.toFloat text_ |> Maybe.withDefault model.opacity }


{-| 今選んでいるラフの置き場。 -}
pathOf : Model -> Maybe String
pathOf model =
    case ( model.sketch, model.version ) of
        ( Just name, Just version ) ->
            Just ("draft/sketch/" ++ name ++ "/v" ++ String.fromInt version ++ ".json")

        _ ->
            Nothing


{-| これから読みに行く置き場(読み待ちの時だけ)。親はこれを見て 1 回だけ取りに行く。 -}
pendingPath : Model -> Maybe String
pendingPath model =
    if model.draft == DraftLoading then
        pathOf model

    else
        Nothing


{-| ラフの中身が届いた。読めない中身は空のパネルでなく理由を出す。 -}
loaded : String -> Model -> Model
loaded content model =
    case SketchPad.decode content of
        Just pad ->
            { model | draft = DraftReady pad }

        Nothing ->
            { model | draft = DraftBroken "ラフの中身が読めませんでした" }


loadFailed : String -> Model -> Model
loadFailed reason model =
    { model | draft = DraftBroken reason }



-- ひとことをやること一覧へ並べる


{-| ラフのセルを 1 つ指す。同じセルをもう一度押したら指すのをやめる
(押した所が合っているか、消して確かめられるように)。
-}
pickCell : { x : Int, y : Int } -> Model -> Model
pickCell cell model =
    if model.cell == Just cell then
        { model | cell = Nothing, post = PostIdle }

    else
        { model | cell = Just cell, post = PostIdle }


pickedCell : Model -> Maybe { x : Int, y : Int }
pickedCell model =
    model.cell


setNote : String -> Model -> Model
setNote note model =
    { model | note = note, post = PostIdle }


noteOf : Model -> String
noteOf model =
    model.note


postOf : Model -> Post
postOf model =
    model.post


submitting : Model -> Model
submitting model =
    { model | post = PostSending }


{-| 並べ終わった。ひとことは棚へ移ったので画面からは消す(同じ言葉の二重投稿を防ぐ)。 -}
submitted : Model -> Model
submitted model =
    { model | post = PostDone, note = "" }


submitFailed : String -> Model -> Model
submitFailed reason model =
    { model | post = PostFailed reason }


{-| 並べられるのは、絵とラフとセルが揃っていて、ひとことが書いてあるときだけ。 -}
canSubmit : Model -> Bool
canSubmit model =
    model.post /= PostSending && buildTicket model /= Nothing


{-| やること一覧へ並べる 1 枚ぶんの言葉。中身は見比べた物の置き場とひとことだけで、
どこをどう直すかは書かない(直し方を決めるのはいつも人と AI の側)。
-}
buildTicket : Model -> Maybe { title : String, comment : String, body : String, scene : String }
buildTicket model =
    case ( model.scene, model.cell, model.draft ) of
        ( Just scene, Just cell, DraftReady pad ) ->
            let
                note =
                    String.trim model.note

                where_ =
                    "左から " ++ String.fromInt (cell.x + 1) ++ " セル目・上から " ++ String.fromInt (cell.y + 1) ++ " セル目"
            in
            if note == "" || model.sketch == Nothing then
                Nothing

            else
                Just
                    { title = "ラフと見比べ: " ++ scene ++ " ↔ " ++ draftName model ++ " — " ++ where_
                    , comment = note
                    , scene = scene
                    , body =
                        String.join "\n"
                            [ "## 見比べた物"
                            , ""
                            , "- 生成された絵: gallery/" ++ scene
                            , "- ラフ: " ++ (pathOf model |> Maybe.withDefault "")
                            , "- 指した場所: " ++ where_ ++ "(ラフ全体は " ++ String.fromInt pad.size.w ++ "×" ++ String.fromInt pad.size.h ++ " セル)"
                            , "- そのセルにラフで置いてあった物: " ++ cellName pad cell
                            , ""
                            , "見た目の話だけを書いています(遊んでみないと分からない事はここに書きません)。"
                            ]
                    }

        _ ->
            Nothing


{-| ラフの名前とバージョンをひと続きに。 -}
draftName : Model -> String
draftName model =
    case ( model.sketch, model.version ) of
        ( Just name, Just version ) ->
            name ++ " v" ++ String.fromInt version

        ( Just name, Nothing ) ->
            name

        _ ->
            "ラフ"


{-| 指したセルにラフで塗ってあった物の名前(凡例の名前)。塗っていなければ「何も置いていない」。 -}
cellName : SketchPad.Model -> { x : Int, y : Int } -> String
cellName pad cell =
    SketchPad.flatRows pad
        |> List.drop cell.y
        |> List.head
        |> Maybe.andThen (\line -> line |> String.toList |> List.drop cell.x |> List.head)
        |> Maybe.andThen (\char -> pad.legend |> List.filter (\entry -> entry.char == char) |> List.head)
        |> Maybe.map .name
        |> Maybe.withDefault "何も置いていない所"



-- 画面


{-| 見比べのビュー。sceneUrl は「生成された絵の URL」を作る関数で受ける —
サーバの口の組み方を知るのは呼び側の仕事。
-}
view : Handlers msg -> { sceneUrl : String -> String } -> Model -> Html msg
view handlers urls model =
    if not model.open then
        text ""

    else
        div [ HA.class "sketch-compare-layer fixed inset-0 z-50 flex flex-col bg-app" ]
            [ viewHead handlers
            , div [ HA.class "flex min-h-0 flex-1" ]
                [ viewPicks handlers model
                , viewCompare handlers urls model
                ]
            ]


viewHead : Handlers msg -> Html msg
viewHead handlers =
    div [ HA.class "flex h-9 shrink-0 items-center gap-3 border-b border-edge bg-panel px-3" ]
        [ span [ HA.class "text-xs font-semibold text-ink" ] [ text "ラフと見比べ" ]
        , span [ HA.class "text-[11px] text-ink-faint" ] [ text "生成された絵とラフを 1 枚ずつ選んで見比べます" ]
        , span [ HA.class "flex-1" ] []
        , button [ HA.class "sketch-compare-close btn btn-mini", HE.onClick handlers.onClose ] [ text "閉じる(Esc)" ]
        ]


viewEmpty : String -> Html msg
viewEmpty message =
    div [ HA.class "m-auto text-[11px] text-ink-faint" ] [ text message ]


{-| 左の一覧。生成された絵とラフを別々に選ぶ。 -}
viewPicks : Handlers msg -> Model -> Html msg
viewPicks handlers model =
    div [ HA.class "sketch-compare-picks w-64 shrink-0 overflow-y-auto border-r border-edge bg-panel py-1" ]
        (List.concat
            [ [ viewPickHead "生成された絵" ]
            , if List.isEmpty model.scenes then
                [ viewPickNote "gallery/ に絵がありません" ]

              else
                model.scenes
                    |> List.map (\name -> viewRow (model.scene == Just name) (handlers.onScene name) name)
            , [ viewPickHead "ラフ" ]
            , if List.isEmpty model.sketches then
                [ viewPickNote "draft/sketch/ にラフがありません" ]

              else
                model.sketches
                    |> List.map (\sketch -> viewSketchRow handlers model sketch)
            ]
        )


viewPickHead : String -> Html msg
viewPickHead label =
    div [ HA.class "px-3 pb-0.5 pt-2 text-[10px] text-ink-faint" ] [ text label ]


viewPickNote : String -> Html msg
viewPickNote message =
    div [ HA.class "px-3 py-1 text-[11px] text-ink-faint" ] [ text message ]


viewRow : Bool -> msg -> String -> Html msg
viewRow on onClick label =
    button
        [ HA.classList
            [ ( "sketch-compare-row flex w-full cursor-pointer items-center gap-1.5 px-3 py-1 text-left font-mono text-[11px]", True )
            , ( "on bg-accent/15 text-ink", on )
            , ( "text-ink-soft hover:bg-white/5", not on )
            ]
        , HE.onClick onClick
        ]
        [ span [ HA.class "min-w-0 flex-1 truncate" ] [ text label ] ]


{-| ラフ 1 本。選んでいる間だけバージョンの列を開く —
畳んでおかないと、ラフが増えたとき一覧が番号で埋まる。
-}
viewSketchRow : Handlers msg -> Model -> SketchPad.Sketch -> Html msg
viewSketchRow handlers model sketch =
    let
        on =
            model.sketch == Just sketch.name
    in
    div []
        (viewRow on (handlers.onSketch sketch.name) sketch.name
            :: (if on then
                    [ div [ HA.class "flex flex-wrap gap-1 px-3 pb-1" ]
                        (versionsOf sketch.name model
                            |> List.map
                                (\version ->
                                    button
                                        [ HA.classList
                                            [ ( "sketch-compare-version btn btn-mini", True )
                                            , ( "bg-accent text-white hover:bg-accent", model.version == Just version )
                                            ]
                                        , HE.onClick (handlers.onVersion version)
                                        ]
                                        [ text ("v" ++ String.fromInt version) ]
                                )
                        )
                    ]

                else
                    []
               )
        )


viewCompare : Handlers msg -> { sceneUrl : String -> String } -> Model -> Html msg
viewCompare handlers urls model =
    case ( model.scene, model.sketch ) of
        ( Just scene, Just _ ) ->
            div [ HA.class "sketch-compare-body flex min-w-0 flex-1 flex-col gap-2 overflow-auto p-3" ]
                (viewModes handlers model
                    :: viewPair handlers urls model scene
                    ++ [ viewNote handlers model ]
                )

        _ ->
            viewEmpty "左の一覧から、生成された絵とラフを 1 枚ずつ選んでください"


viewModes : Handlers msg -> Model -> Html msg
viewModes handlers model =
    let
        tab mode label =
            button
                [ HA.classList
                    [ ( "sketch-compare-mode btn btn-mini", True )
                    , ( "bg-accent text-white hover:bg-accent", model.mode == mode )
                    ]
                , HE.onClick (handlers.onMode mode)
                ]
                [ text label ]
    in
    div [ HA.class "flex items-center gap-1.5" ]
        [ tab SideBySide "並べる"
        , tab Overlay "重ねる"
        , case model.mode of
            Overlay ->
                input
                    [ HA.class "sketch-compare-opacity ml-2 w-40"
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


viewPair : Handlers msg -> { sceneUrl : String -> String } -> Model -> String -> List (Html msg)
viewPair handlers urls model scene =
    let
        shot =
            img [ HA.class "sketch-compare-shot block w-full rounded border border-edge bg-well", HA.src (urls.sceneUrl scene) ] []
    in
    case model.mode of
        SideBySide ->
            [ div [ HA.class "flex gap-3" ]
                [ div [ HA.class "min-w-0 flex-1" ]
                    [ div [ HA.class "mb-1 text-[10px] text-ink-faint" ] [ text "生成された絵" ]
                    , shot
                    ]
                , div [ HA.class "min-w-0 flex-1" ]
                    [ div [ HA.class "mb-1 text-[10px] text-ink-faint" ] [ text (draftLabel model) ]
                    , viewDraft handlers model
                    ]
                ]
            , viewLegend model
            ]

        Overlay ->
            [ div [ HA.class "relative" ]
                [ shot
                , div
                    [ HA.class "sketch-compare-overlay absolute inset-0"
                    , HA.style "opacity" (String.fromFloat model.opacity)
                    ]
                    [ viewDraft handlers model ]
                ]
            , viewLegend model
            ]


draftLabel : Model -> String
draftLabel model =
    case ( model.sketch, model.version ) of
        ( Just name, Just version ) ->
            "ラフ " ++ name ++ " v" ++ String.fromInt version

        ( Just name, Nothing ) ->
            "ラフ " ++ name

        _ ->
            "ラフ"


{-| ラフの絵(塗れない)。重ねたとき生成された絵と縦横の割合がずれないよう、
横幅いっぱいに伸ばしてセルの縦横比で高さを決める。セルを押すと場所を指せる。
-}
viewDraft : Handlers msg -> Model -> Html msg
viewDraft handlers model =
    case model.draft of
        DraftReady pad ->
            let
                colorOf char =
                    pad.legend
                        |> List.filter (\entry -> entry.char == char)
                        |> List.head
                        |> Maybe.map .fill

                row y line =
                    div [ HA.class "flex min-h-0 flex-1" ]
                        (line
                            |> String.toList
                            |> List.indexedMap (\x char -> viewDraftCell handlers model colorOf { x = x, y = y } char)
                        )
            in
            div
                [ HA.class "sketch-compare-draft w-full rounded border border-edge bg-well"
                , HA.style "aspect-ratio" (String.fromInt (max 1 pad.size.w) ++ " / " ++ String.fromInt (max 1 pad.size.h))
                ]
                -- WhyNot: 1 枚のレイヤーだけを描かない — 分けて描いたラフでは選んでいる 1 枚しか入っておらず、
                -- 奥と手前が黙って消える。
                [ div [ HA.class "flex h-full w-full flex-col" ] (List.indexedMap row (SketchPad.flatRows pad)) ]

        DraftLoading ->
            div [ HA.class "rounded border border-edge bg-well p-4 text-[11px] text-ink-faint" ] [ text "読み込んでいます…" ]

        DraftBroken reason ->
            div [ HA.class "rounded border border-edge bg-well p-4 text-[11px] text-ink-faint" ] [ text reason ]

        DraftNone ->
            div [ HA.class "rounded border border-edge bg-well p-4 text-[11px] text-ink-faint" ] [ text "バージョンを選んでください" ]


viewDraftCell : Handlers msg -> Model -> (Char -> Maybe String) -> { x : Int, y : Int } -> Char -> Html msg
viewDraftCell handlers model colorOf cell char =
    let
        -- 空きセルは塗らない。重ねたとき塗っていない所で下の絵を隠さないため
        fill =
            case colorOf char of
                Just color ->
                    [ HA.style "background-color" color ]

                Nothing ->
                    []

        -- 指したセルは内側の枠で示す。塗りつぶすと、そのセルのラフの色が見えなくなる
        picked =
            if model.cell == Just cell then
                [ HA.style "box-shadow" "inset 0 0 0 2px #f43f5e" ]

            else
                []
    in
    div
        (HA.class "sketch-compare-cell min-w-0 flex-1 cursor-crosshair"
            :: HE.onClick (handlers.onCell cell)
            :: fill
            ++ picked
        )
        []


{-| 指した場所へのひとこと。書けるのは見た目の話だけ — 遊ばないと分からない事は
このビューでは確かめようがないので、書く前に断っておく。
-}
viewNote : Handlers msg -> Model -> Html msg
viewNote handlers model =
    div [ HA.class "sketch-compare-note flex flex-wrap items-center gap-2 rounded border border-edge bg-panel p-2" ]
        [ span [ HA.class "text-[10px] text-ink-faint" ] [ text (pickLabel model) ]
        , input
            [ HA.class "sketch-compare-note-input min-w-0 flex-1 rounded border border-edge bg-transparent px-2 py-1 text-xs text-ink"
            , HA.placeholder "見た目で気になった事を書く(例: 空が明るすぎる)"
            , HA.value model.note
            , HE.onInput handlers.onNote
            ]
            []
        , button
            [ HA.class "sketch-compare-submit btn btn-primary text-xs"
            , HA.disabled (not (canSubmit model))
            , HA.title "指したセルとひとことを、遊んでいて切った物と同じやること一覧へ並べます"
            , HE.onClick handlers.onSubmit
            ]
            [ text (submitLabel model) ]
        , case model.post of
            PostFailed reason ->
                span [ HA.class "text-[10px] text-ink-faint" ] [ text reason ]

            _ ->
                text ""
        ]


pickLabel : Model -> String
pickLabel model =
    case model.cell of
        Just cell ->
            "指した場所: 左から " ++ String.fromInt (cell.x + 1) ++ " ・上から " ++ String.fromInt (cell.y + 1) ++ " セル目"

        Nothing ->
            "ラフのセルを押して場所を指してください"


submitLabel : Model -> String
submitLabel model =
    case model.post of
        PostSending ->
            "並べています…"

        PostDone ->
            "✓ 並べました"

        _ ->
            "やること一覧へ並べる"


{-| どの色が何のつもりか。ラフだけでは色の意味が読めない。 -}
viewLegend : Model -> Html msg
viewLegend model =
    case model.draft of
        DraftReady pad ->
            div [ HA.class "sketch-compare-legend flex flex-wrap items-center gap-2" ]
                (pad.legend
                    |> List.map
                        (\entry ->
                            span [ HA.class "flex items-center gap-1 text-[10px] text-ink-faint" ]
                                [ span
                                    [ HA.class "inline-block h-3 w-3 rounded-sm border border-edge"
                                    , HA.style "background-color" entry.fill
                                    ]
                                    []
                                , text entry.name
                                ]
                        )
                )

        _ ->
            text ""
