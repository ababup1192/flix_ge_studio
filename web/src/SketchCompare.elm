module SketchCompare exposing
    ( Draft(..)
    , Handlers
    , Mode(..)
    , Model
    , close
    , draftOf
    , hasSketches
    , init
    , isOpen
    , loadFailed
    , loaded
    , open
    , pathOf
    , pendingPath
    , selectScene
    , selectSketch
    , selectVersion
    , selectedScene
    , selectedSketch
    , selectedVersion
    , setMode
    , setOpacity
    , view
    , versionsOf
    , withScenes
    , withSketches
    )

{-| 生成された絵と、その元になったラフを人が選んで見比べる窓。

どの絵がどのラフから来たかは絵に書かれていないので、機械では結び付けない —
左の 2 つの一覧から人が 1 枚ずつ指し、並べるか重ねるかで見比べるだけ。

「違いを塗る」は入れない。ラフはマスの塗り絵で、生成された絵と画素の位置が
そもそも合わないため、色の差を出しても読み取れる物にならない。

塗るための操作(Msg)は持たない。ラフの絵は読み取り専用にここで描く。

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
    }


type alias Handlers msg =
    { onScene : String -> msg
    , onSketch : String -> msg
    , onVersion : Int -> msg
    , onMode : Mode -> msg
    , onOpacity : String -> msg
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
                { next | version = List.head versions, draft = DraftLoading }


selectScene : String -> Model -> Model
selectScene name model =
    { model | scene = Just name }


{-| ラフを選ぶと最新のバージョンから見る。中身は親が読みに行く(pendingPath)。 -}
selectSketch : String -> Model -> Model
selectSketch name model =
    case List.head (versionsOf name model) of
        Just newest ->
            { model | sketch = Just name, version = Just newest, draft = DraftLoading }

        Nothing ->
            { model | sketch = Just name, version = Nothing, draft = DraftNone }


selectVersion : Int -> Model -> Model
selectVersion version model =
    { model | version = Just version, draft = DraftLoading }


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


{-| ラフの中身が届いた。読めない中身は空の窓でなく理由を出す。 -}
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



-- 画面


{-| 見比べの窓。sceneUrl は「生成された絵の URL」を作る関数で受ける —
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
                    :: viewPair urls model scene
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


viewPair : { sceneUrl : String -> String } -> Model -> String -> List (Html msg)
viewPair urls model scene =
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
                    , viewDraft model
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
                    [ viewDraft model ]
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


{-| ラフの絵(読み取り専用)。重ねたとき生成された絵と縦横の割合がずれないよう、
横幅いっぱいに伸ばしてマスの縦横比で高さを決める。
-}
viewDraft : Model -> Html msg
viewDraft model =
    case model.draft of
        DraftReady pad ->
            let
                colorOf char =
                    pad.legend
                        |> List.filter (\entry -> entry.char == char)
                        |> List.head
                        |> Maybe.map .fill

                row line =
                    div [ HA.class "flex min-h-0 flex-1" ]
                        (line |> String.toList |> List.map (viewDraftCell colorOf))
            in
            div
                [ HA.class "sketch-compare-draft w-full rounded border border-edge bg-well"
                , HA.style "aspect-ratio" (String.fromInt (max 1 pad.size.w) ++ " / " ++ String.fromInt (max 1 pad.size.h))
                ]
                [ div [ HA.class "flex h-full w-full flex-col" ] (List.map row pad.rows) ]

        DraftLoading ->
            div [ HA.class "rounded border border-edge bg-well p-4 text-[11px] text-ink-faint" ] [ text "読み込んでいます…" ]

        DraftBroken reason ->
            div [ HA.class "rounded border border-edge bg-well p-4 text-[11px] text-ink-faint" ] [ text reason ]

        DraftNone ->
            div [ HA.class "rounded border border-edge bg-well p-4 text-[11px] text-ink-faint" ] [ text "バージョンを選んでください" ]


viewDraftCell : (Char -> Maybe String) -> Char -> Html msg
viewDraftCell colorOf char =
    case colorOf char of
        Just fill ->
            div [ HA.class "sketch-compare-cell min-w-0 flex-1", HA.style "background-color" fill ] []

        Nothing ->
            -- 空きマスは透かす。重ねたとき塗っていない所で下の絵を隠さないため
            div [ HA.class "sketch-compare-cell min-w-0 flex-1" ] []


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
