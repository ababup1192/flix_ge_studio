module GalleryView exposing
    ( Model
    , Msg(..)
    , Out(..)
    , Scene
    , gotList
    , init
    , listDecoder
    , listFailed
    , loading
    , update
    , view
    )

{-| ギャラリー — ゲームが描き出した場面の絵(gallery/ の PNG)を一覧で眺める画面。

読むだけ。リファレンス画像の更新は ReferenceView の仕事で、ここは触らない。

1 枚のカードに出るのは 4 つ。

  - 絵(クリックで拡大)
  - 題と一言 … ゲーム側の場面の説明 Doc(assets/\*.scenes.json)のラベル。無ければ
    ファイル名で代用し、「説明がまだありません」と静かに添える
  - タグ
  - 印 … リファレンス画像と違う / まだ登録が無い / 今回の焼き直しから外れている

サーバ往復は Main の仕事(Out で頼む)。

-}

import Html exposing (Html, button, div, img, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import SceneView


{-| 場面 1 つぶん。name は PNG のファイル名("s3\_clear.png")で、
title / desc / tags はラベル(無ければ空)。reference は
"ok" / "diff" / "missingReference" のどれか。
-}
type alias Scene =
    { name : String
    , title : String
    , desc : String
    , tags : List String
    , mtime : Int
    , reference : String
    }


type alias Model =
    { scenes : List Scene

    -- 取得中(開いた直後)
    , busy : Bool

    -- 口を持たないサーバでは案内だけ出す(fail-open)
    , available : Bool

    -- 拡大表示中の場面(Nothing = 閉じている)
    , zoom : Maybe String
    }


init : Model
init =
    { scenes = [], busy = False, available = True, zoom = Nothing }


{-| 取りに行った。 -}
loading : Model -> Model
loading model =
    { model | busy = True }


{-| 一覧が届いた。 -}
gotList : List Scene -> Model -> Model
gotList scenes model =
    { model | scenes = scenes, busy = False, available = True }


{-| 一覧が取れなかった(口を持たない旧サーバ等)。案内に落として空にする。 -}
listFailed : Model -> Model
listFailed model =
    { model | scenes = [], busy = False, available = False }


type Msg
    = ShotClicked String
    | ZoomClosed
    | RefreshClicked


type Out
    = OutNone
    | OutRefresh


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        ShotClicked name ->
            ( { model | zoom = Just name }, OutNone )

        ZoomClosed ->
            ( { model | zoom = Nothing }, OutNone )

        RefreshClicked ->
            ( { model | busy = True }, OutRefresh )



-- デコーダ


listDecoder : D.Decoder (List Scene)
listDecoder =
    D.field "items" (D.list sceneDecoder)


{-| 全部の欄に既定値を当てる — ラベルを持たないゲーム(まだ Doc が無い)でも
名前だけで並べられるように。
-}
sceneDecoder : D.Decoder Scene
sceneDecoder =
    D.map6 Scene
        (D.field "name" D.string)
        (withDefault "" (D.field "title" D.string))
        (withDefault "" (D.field "desc" D.string))
        (withDefault [] (D.field "tags" (D.list D.string)))
        (withDefault 0 (D.field "mtime" D.int))
        (withDefault "ok" (D.field "reference" D.string))


withDefault : a -> D.Decoder a -> D.Decoder a
withDefault fallback decoder =
    D.oneOf [ decoder, D.succeed fallback ]



-- 画面


type alias Ctx =
    { serverBase : String, root : String }


view : Ctx -> Model -> Html Msg
view ctx model =
    div [ HA.class "gallery flex min-h-0 flex-1 flex-col overflow-y-auto px-6 pb-8 pt-5" ]
        [ viewHead model
        , viewBody ctx model
        , viewZoom ctx model
        ]


viewHead : Model -> Html Msg
viewHead model =
    div [ HA.class "mb-4 flex items-baseline gap-3" ]
        [ span [ HA.class "text-sm font-semibold text-ink" ]
            [ text ("場面 " ++ String.fromInt (List.length model.scenes)) ]
        , span [ HA.class "text-[11px] text-ink-faint" ]
            [ text "ゲームが描き出した絵(gallery/)です。クリックで拡大します" ]
        , button
            [ HA.class "gallery-refresh ml-auto cursor-pointer text-[11px] text-ink-faint hover:text-ink-soft"
            , HE.onClick RefreshClicked
            ]
            [ text
                (if model.busy then
                    "⏳ 読み込んでいます…"

                 else
                    "更新"
                )
            ]
        ]


viewBody : Ctx -> Model -> Html Msg
viewBody ctx model =
    if not model.available then
        viewNote "このサーバはギャラリーの口を持っていません。Studio を新しくしてください。"

    else if List.isEmpty model.scenes then
        if model.busy then
            viewNote "読み込んでいます…"

        else
            viewNote "まだ場面がありません。ゲームで絵を描き出す(make render-all)と、ここに並びます。"

    else
        div [ HA.class "grid grid-cols-[repeat(auto-fill,minmax(220px,1fr))] gap-4" ]
            (List.map (viewCard ctx (staleBefore model.scenes)) model.scenes)


viewNote : String -> Html Msg
viewNote message =
    div [ HA.class "text-xs text-ink-soft" ] [ text message ]


{-| 「今回の焼き直しから外れている」とみなす境目。いちばん新しい絵の 1 分前より
古い物を古いと呼ぶ — 全場面は 1 回の make render-all でまとめて出るので、
ここから外れた場面は前回の焼きが残っている。
-}
staleBefore : List Scene -> Int
staleBefore scenes =
    (scenes |> List.map .mtime |> List.maximum |> Maybe.withDefault 0) - 60000


viewCard : Ctx -> Int -> Scene -> Html Msg
viewCard ctx stale scene =
    div [ HA.class "gallery-card flex flex-col overflow-hidden rounded-lg border border-edge bg-panel" ]
        [ img
            [ HA.class "scene-shot block w-full cursor-zoom-in bg-well"
            , HA.src (shotUrl ctx scene)
            , HA.alt scene.name
            , HA.attribute "loading" "lazy"
            , HA.title "クリックで拡大"
            , HE.onClick (ShotClicked scene.name)
            ]
            []
        , div [ HA.class "flex min-w-0 flex-col gap-1 px-3 py-2" ]
            [ div [ HA.class "truncate text-xs font-semibold text-ink", HA.title scene.name ]
                [ text (headingOf scene) ]
            , viewDesc scene
            , viewTags scene
            , viewMarks stale scene
            ]
        ]


{-| カードの見出し。ラベルの題があればそれ、無ければファイル名(拡張子は落とす)。 -}
headingOf : Scene -> String
headingOf scene =
    if scene.title == "" then
        SceneView.sceneLabel scene.name

    else
        scene.title


viewDesc : Scene -> Html Msg
viewDesc scene =
    if scene.desc == "" then
        div
            [ HA.class "text-[11px] text-ink-faint"
            , HA.title "ゲームの assets/*.scenes.json に name と title と desc を足すと、ここに出ます"
            ]
            [ text "説明がまだありません" ]

    else
        div [ HA.class "text-[11px] leading-snug text-ink-soft" ] [ text scene.desc ]


viewTags : Scene -> Html Msg
viewTags scene =
    if List.isEmpty scene.tags then
        text ""

    else
        div [ HA.class "flex flex-wrap gap-1" ]
            (scene.tags
                |> List.map
                    (\tag ->
                        span [ HA.class "scene-tag rounded-sm bg-white/10 px-1 text-[10px] text-ink-faint" ]
                            [ text tag ]
                    )
            )


{-| 気に留めてほしい事だけ出す。基準どおりで新しい絵には何も付けない
(健康なときは目に入らなくてよい)。
-}
viewMarks : Int -> Scene -> Html Msg
viewMarks stale scene =
    let
        marks =
            List.filterMap identity
                [ case scene.reference of
                    "diff" ->
                        Just ( "リファレンスと違う", "リファレンス画像と 1 バイトでも違います" )

                    "missingReference" ->
                        Just ( "リファレンス未登録", "この場面のリファレンス画像がまだありません" )

                    _ ->
                        Nothing
                , if scene.mtime < stale then
                    Just ( "前回の焼きのまま", "いちばん新しい絵より古いままです(描き出しから外れた場面)" )

                  else
                    Nothing
                ]
    in
    if List.isEmpty marks then
        text ""

    else
        div [ HA.class "flex flex-wrap gap-1" ]
            (marks
                |> List.map
                    (\( label, why ) ->
                        span
                            [ HA.class "scene-mark rounded-sm bg-accent/20 px-1 text-[10px] text-ink-soft"
                            , HA.title why
                            ]
                            [ text label ]
                    )
            )


{-| 絵の拡大。ミニプレイヤーの拡大と同じ流儀(オーバーレイ・どこを押しても閉じる)。 -}
viewZoom : Ctx -> Model -> Html Msg
viewZoom ctx model =
    case model.zoom |> Maybe.andThen (sceneNamed model) of
        Nothing ->
            text ""

        Just scene ->
            div
                [ HA.class "gallery-zoom fixed inset-0 z-50 flex cursor-zoom-out flex-col items-center justify-center gap-3 bg-black/85"
                , HE.onClick ZoomClosed
                ]
                [ img
                    [ HA.class "scene-shot h-[85vh] max-w-[90vw] object-contain"
                    , HA.src (shotUrl ctx scene)
                    , HA.alt scene.name
                    ]
                    []
                , div [ HA.class "flex flex-col items-center gap-1" ]
                    [ div [ HA.class "text-xs text-white" ] [ text (headingOf scene) ]
                    , div [ HA.class "font-mono text-[11px] text-white/60" ] [ text scene.name ]
                    ]
                ]


sceneNamed : Model -> String -> Maybe Scene
sceneNamed model name =
    model.scenes |> List.filter (\scene -> scene.name == name) |> List.head


{-| 絵の URL。焼き直しで中身が入れ替わるファイルなので mtime でキャッシュを破る
(同じ URL のまま古い絵を出さない)。
-}
shotUrl : Ctx -> Scene -> String
shotUrl ctx scene =
    SceneView.galleryImageUrl ctx.serverBase ctx.root "gallery" scene.name
        ++ ("&t=" ++ String.fromInt scene.mtime)
