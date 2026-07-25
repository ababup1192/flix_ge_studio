module PixelEditor exposing
    ( Doc
    , Edit
    , Model
    , Msg(..)
    , Out(..)
    , floodAt
    , fromDoc
    , goldenHue
    , init
    , isPlaying
    , paintAt
    , palette
    , pickAt
    , release
    , strokeActive
    , transparentChar
    , update
    , view
    )

{-| ドット絵(*.sprite.json)の手直し。

左に道具(ペン・消しゴム・バケツ・スポイト・戻す/やり直す)、中央にセルの
グリッド、右にパレット(legend から導く)。

文書の正本はここに置かない — 描画は毎回親から渡される Doc(パース済み
docText)から導き、一筆(pointerdown〜up)の途中だけ作業コピー(working)が
表示に勝つ。一筆の確定は rows 丸ごと 1 本の Edit として親へ返し、親の
編集直列(docEdit → dirty → 保存)に乗せる — 並行の保存経路は作らない。
戻す/やり直すもスナップショットの Edit を同じ経路で流す。

legend の値は意味色キー(テーマが解く名前)のことがあり、ここでは実色に
解けない。サーバの解決表(POST /sprite/colors の「値 → #rrggbb」)があれば
それを使い、無い・欠けるキーは #rrggbb 素通し → legend 内の並び順から導く
仮色、の順に倒す(表示のためだけ。保存される文字には関与しない)。

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Set exposing (Set)
import Svg
import Svg.Attributes as SA



-- 文書(親が渡す読み取り結果)


type alias Doc =
    { legend : List ( Char, String )
    , sprites : List Sprite
    }


type alias Sprite =
    { name : String

    -- 種別(*.sprite.json の sprites[].group)。19 体を 1 列に並べると探せないので、
    -- 束ねて見出しを付ける。宣言が無ければ「その他」へ。
    , group : String
    , frames : List ( String, List String )
    }


defaultGroup : String
defaultGroup =
    "その他"


{-| 編集 1 件 = 1 コマの rows 丸ごと。 -}
type alias Edit =
    { sprite : String
    , frame : String
    , rows : List String
    }


{-| パース済み sprite Doc の読み取り。legend が読めない・絵が 1 枚も残らない
文書は Nothing — 呼び側は従来のフォーム/コード表示へ倒す(壊さない)。
崩れた rows(空・行の長さ不揃い)はコマ単位で黙って除く。
-}
fromDoc : D.Value -> Maybe Doc
fromDoc value =
    case ( D.decodeValue legendDecoder value, D.decodeValue spritesDecoder value ) of
        ( Ok legend, Ok raw ) ->
            let
                sprites =
                    List.filterMap cleanSprite raw
            in
            if List.isEmpty sprites then
                Nothing

            else
                Just { legend = legend, sprites = sprites }

        _ ->
            Nothing


legendDecoder : D.Decoder (List ( Char, String ))
legendDecoder =
    D.field "legend" (D.keyValuePairs D.string)
        |> D.map
            (List.filterMap
                (\( key, name ) ->
                    case String.toList key of
                        [ ch ] ->
                            Just ( ch, name )

                        _ ->
                            Nothing
                )
            )


spritesDecoder : D.Decoder (List ( String, D.Value ))
spritesDecoder =
    D.field "sprites" (D.keyValuePairs D.value)


cleanSprite : ( String, D.Value ) -> Maybe Sprite
cleanSprite ( name, value ) =
    if String.startsWith "//" name then
        Nothing

    else
        D.decodeValue (D.field "frames" (D.keyValuePairs D.value)) value
            |> Result.toMaybe
            |> Maybe.map (List.filterMap cleanFrame)
            |> Maybe.andThen
                (\frames ->
                    if List.isEmpty frames then
                        Nothing

                    else
                        Just
                            { name = name
                            , group =
                                D.decodeValue (D.field "group" D.string) value
                                    |> Result.withDefault defaultGroup
                            , frames = frames
                            }
                )


cleanFrame : ( String, D.Value ) -> Maybe ( String, List String )
cleanFrame ( name, value ) =
    if String.startsWith "//" name then
        Nothing

    else
        D.decodeValue (D.list D.string) value
            |> Result.toMaybe
            |> Maybe.andThen
                (\rows ->
                    if rectangular rows then
                        Just ( name, rows )

                    else
                        Nothing
                )


rectangular : List String -> Bool
rectangular rows =
    case rows of
        [] ->
            False

        first :: rest ->
            let
                w =
                    String.length first
            in
            w > 0 && List.all (\row -> String.length row == w) rest



-- 状態


type Tool
    = Pen
    | Eraser
    | Bucket
    | Dropper


type alias Snapshot =
    { sprite : String
    , frame : String
    , before : List String
    , after : List String
    }


type alias Model =
    { tool : Tool

    -- いまの色(legend の文字)。Nothing は「まだ選んでいない」= legend の先頭
    , color : Maybe Char

    -- 選択中の絵とコマ。文書から消えていたら先頭へ倒す(view/update で毎回解決)
    , spriteKey : Maybe String
    , frameKey : Maybe String
    , cellPx : Int
    , hover : Maybe ( Int, Int )

    -- 一筆の途中(Just = 描いている)。before は戻す用の一筆前
    , stroke : Maybe { ch : Char, before : List String }

    -- 表示が文書に勝つ作業コピー(一筆の途中と、確定後のエコー待ちの間)
    , working : Maybe Edit
    , undo : List Snapshot
    , redo : List Snapshot

    -- コマ送りで動きを見せている最中か。編集を始めたら止める
    , playing : Bool
    }


init : Model
init =
    { tool = Pen
    , color = Nothing
    , spriteKey = Nothing
    , frameKey = Nothing
    , cellPx = 24
    , hover = Nothing
    , stroke = Nothing
    , working = Nothing
    , undo = []
    , redo = []
    , playing = False
    }


{-| 一筆の途中か(親がグローバル mouseup を購読する間だけ True)。 -}
strokeActive : Model -> Bool
strokeActive model =
    model.stroke /= Nothing


{-| 一筆・作業コピーを手放して文書の値へ戻る(モード切替等、テキスト側で
文書が動き得る時に親が呼ぶ)。道具・ズーム・履歴は保つ。
-}
release : Model -> Model
release model =
    { model | stroke = Nothing, working = Nothing, hover = Nothing, playing = False }


{-| コマ送り中か(親が時計を購読する間だけ True)。 -}
isPlaying : Model -> Bool
isPlaying model =
    model.playing


{-| いまの絵の次のコマ。最後まで来たら先頭へ戻る。 -}
nextFrameKey : Doc -> Model -> Maybe String
nextFrameKey doc model =
    case selectedSprite doc model of
        Nothing ->
            model.frameKey

        Just sprite ->
            let
                names =
                    List.map Tuple.first sprite.frames

                current =
                    model.frameKey |> Maybe.withDefault (List.head names |> Maybe.withDefault "")

                after =
                    names
                        |> List.drop 1
                        |> List.map2 (\a b -> ( a, b )) names
                        |> List.filter (\( a, _ ) -> a == current)
                        |> List.head
                        |> Maybe.map Tuple.second
            in
            case after of
                Just next ->
                    Just next

                Nothing ->
                    List.head names



-- 更新


type Msg
    = ToolChosen Tool
    | ColorChosen Char
    | AddColorPressed
    | SpriteChosen String
    | FrameChosen String
    | PlayToggled
    | PlayTicked
    | ZoomStepped Int
    | CellPressed Int Int Int
    | CellEntered Int Int
    | GridLeft
    | StrokeEnded
    | UndoPressed
    | RedoPressed
      -- contextmenu を抑えるためだけの空打ち(右クリック=消しゴムを活かす)
    | Swallowed


type Out
    = Silent
    | Edited Edit
    | Noticed String


update : Doc -> Msg -> Model -> ( Model, Out )
update doc msg model =
    case msg of
        Swallowed ->
            ( model, Silent )

        ToolChosen tool ->
            ( { model | tool = tool }, Silent )

        ColorChosen ch ->
            -- 消しゴム・スポイト中の色選びは「その色で描きたい」なのでペンへ
            ( { model
                | color = Just ch
                , tool =
                    if model.tool == Eraser || model.tool == Dropper then
                        Pen

                    else
                        model.tool
              }
            , Silent
            )

        AddColorPressed ->
            ( model, Noticed "これから: legend の編集として追加予定です" )

        SpriteChosen name ->
            ( { model | spriteKey = Just name, frameKey = Nothing, working = Nothing, stroke = Nothing, hover = Nothing }
            , Silent
            )

        PlayToggled ->
            ( { model | playing = not model.playing }, Silent )

        PlayTicked ->
            -- 次のコマへ。最後まで来たら先頭へ戻る(輪にして動きとして見せる)
            ( { model | frameKey = nextFrameKey doc model, working = Nothing }, Silent )

        FrameChosen name ->
            ( { model | frameKey = Just name, working = Nothing, stroke = Nothing }, Silent )

        ZoomStepped dir ->
            ( { model | cellPx = zoomStep dir model.cellPx }, Silent )

        CellEntered x y ->
            let
                m1 =
                    { model | hover = Just ( x, y ) }
            in
            case ( m1.stroke, m1.working ) of
                ( Just stroke, Just w ) ->
                    ( { m1 | working = Just { w | rows = paintAt stroke.ch ( x, y ) w.rows } }, Silent )

                _ ->
                    ( m1, Silent )

        GridLeft ->
            ( { model | hover = Nothing }, Silent )

        CellPressed x y buttonId ->
            -- 動いている絵に描くと狙いが外れるので、描き始めたらコマ送りを止める
            press doc ( x, y ) buttonId { model | playing = False }

        StrokeEnded ->
            endStroke model

        UndoPressed ->
            case model.undo of
                [] ->
                    ( model, Silent )

                snap :: rest ->
                    applySnapshot snap.before { model | undo = rest, redo = snap :: model.redo } snap

        RedoPressed ->
            case model.redo of
                [] ->
                    ( model, Silent )

                snap :: rest ->
                    applySnapshot snap.after { model | redo = rest, undo = snap :: model.undo } snap


{-| 押した瞬間の分岐。右ボタンは道具に依らず消しゴム(定番ツールの作法)。 -}
press : Doc -> ( Int, Int ) -> Int -> Model -> ( Model, Out )
press doc cell buttonId model =
    case shownGrid doc model of
        Nothing ->
            ( model, Silent )

        Just grid ->
            if buttonId == 2 then
                ( startStroke (transparentChar doc) cell grid model, Silent )

            else if buttonId /= 0 then
                ( model, Silent )

            else
                case model.tool of
                    Pen ->
                        ( startStroke (paintChar doc model) cell grid model, Silent )

                    Eraser ->
                        ( startStroke (transparentChar doc) cell grid model, Silent )

                    Bucket ->
                        commitRows grid (floodAt (paintChar doc model) cell grid.rows) model

                    Dropper ->
                        case pickAt cell grid.rows of
                            Nothing ->
                                ( model, Silent )

                            Just ch ->
                                if List.any (\( c, _ ) -> c == ch) doc.legend then
                                    ( { model | color = Just ch, tool = Pen }, Silent )

                                else
                                    -- 透明を吸ったら消しゴム(透明色は「無い」ので)
                                    ( { model | tool = Eraser }, Silent )


startStroke : Char -> ( Int, Int ) -> Edit -> Model -> Model
startStroke ch cell grid model =
    { model
        | stroke = Just { ch = ch, before = grid.rows }
        , working = Just { grid | rows = paintAt ch cell grid.rows }
    }


endStroke : Model -> ( Model, Out )
endStroke model =
    case ( model.stroke, model.working ) of
        ( Just stroke, Just w ) ->
            if w.rows == stroke.before then
                ( { model | stroke = Nothing }, Silent )

            else
                ( { model
                    | stroke = Nothing
                    , undo = pushHistory { sprite = w.sprite, frame = w.frame, before = stroke.before, after = w.rows } model.undo
                    , redo = []
                  }
                , Edited w
                )

        _ ->
            ( { model | stroke = Nothing }, Silent )


{-| 一筆を介さない即時確定(バケツ)。 -}
commitRows : Edit -> List String -> Model -> ( Model, Out )
commitRows grid rows model =
    if rows == grid.rows then
        ( model, Silent )

    else
        ( { model
            | working = Just { grid | rows = rows }
            , undo = pushHistory { sprite = grid.sprite, frame = grid.frame, before = grid.rows, after = rows } model.undo
            , redo = []
          }
        , Edited { grid | rows = rows }
        )


{-| 戻す/やり直すの適用。対象のコマへ選択も移す — どの絵が戻ったのか見えるように。 -}
applySnapshot : List String -> Model -> Snapshot -> ( Model, Out )
applySnapshot rows model snap =
    ( { model
        | spriteKey = Just snap.sprite
        , frameKey = Just snap.frame
        , working = Just { sprite = snap.sprite, frame = snap.frame, rows = rows }
        , stroke = Nothing
      }
    , Edited { sprite = snap.sprite, frame = snap.frame, rows = rows }
    )


pushHistory : Snapshot -> List Snapshot -> List Snapshot
pushHistory snap history =
    -- 覚えるのは直近 50 筆まで(スナップショットを無限に抱えない)
    snap :: List.take 49 history



-- 純ロジック(塗り・バケツ・スポイト)


{-| (x, y) のセルを 1 文字置き換える。範囲外は何もしない。 -}
paintAt : Char -> ( Int, Int ) -> List String -> List String
paintAt ch ( x, y ) rows =
    List.indexedMap
        (\ry row ->
            if ry == y && x >= 0 && x < String.length row then
                String.left x row ++ String.fromChar ch ++ String.dropLeft (x + 1) row

            else
                row
        )
        rows


{-| バケツ: (x, y) と同じ文字の連結領域(上下左右)だけを ch へ。 -}
floodAt : Char -> ( Int, Int ) -> List String -> List String
floodAt ch start rows =
    case pickAt start rows of
        Nothing ->
            rows

        Just target ->
            if target == ch then
                rows

            else
                let
                    region =
                        grow target rows (Set.fromList [ start ]) [ start ]
                in
                rows
                    |> List.indexedMap
                        (\y row ->
                            row
                                |> String.toList
                                |> List.indexedMap
                                    (\x c ->
                                        if Set.member ( x, y ) region then
                                            ch

                                        else
                                            c
                                    )
                                |> String.fromList
                        )


grow : Char -> List String -> Set ( Int, Int ) -> List ( Int, Int ) -> Set ( Int, Int )
grow target rows visited frontier =
    case frontier of
        [] ->
            visited

        ( x, y ) :: rest ->
            let
                nexts =
                    [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
                        |> List.filter (\p -> not (Set.member p visited) && pickAt p rows == Just target)
            in
            grow target rows (List.foldl Set.insert visited nexts) (nexts ++ rest)


{-| スポイト: (x, y) のセルの文字。範囲外は Nothing。 -}
pickAt : ( Int, Int ) -> List String -> Maybe Char
pickAt ( x, y ) rows =
    if x < 0 || y < 0 then
        Nothing

    else
        rows
            |> List.drop y
            |> List.head
            |> Maybe.andThen (\row -> row |> String.dropLeft x |> String.uncons |> Maybe.map Tuple.first)



-- パレットの導出


type alias Swatch =
    { ch : Char
    , name : String
    , css : String
    }


{-| legend の並びのまま升目にする。名前は legend の値そのもの(名詞を発明しない)。
resolved はサーバの解決表(legend の値 → #rrggbb)。
-}
palette : Dict String String -> Doc -> List Swatch
palette resolved doc =
    doc.legend
        |> List.map (\( ch, name ) -> { ch = ch, name = name, css = colorCss resolved (legendValues doc) name })


{-| 透明を表す文字。実データが使っている非 legend 文字に合わせる('.' 優先)。 -}
transparentChar : Doc -> Char
transparentChar doc =
    let
        legendChars =
            Set.fromList (List.map Tuple.first doc.legend)

        used =
            doc.sprites
                |> List.concatMap .frames
                |> List.concatMap Tuple.second
                |> List.concatMap String.toList
                |> List.filter (\c -> not (Set.member c legendChars))
    in
    if List.member '.' used then
        '.'

    else if List.member ' ' used then
        ' '

    else
        case used of
            c :: _ ->
                c

            [] ->
                if Set.member '.' legendChars then
                    ' '

                else
                    '.'


colorCss : Dict String String -> List String -> String -> String
colorCss resolved values value =
    case Dict.get value resolved of
        Just hex ->
            -- サーバがゲームと同じ色解決で導いた実色(ゲーム画面と一致する)
            hex

        Nothing ->
            if isHexColor value then
                value

            else
                -- 解決表に無い意味色キーは実色に解けない。並び順から導く仮色で
                -- 名前ごとに区別がつけば編集には足りる
                "hsl(" ++ String.fromInt (goldenHue (indexOf value values)) ++ " 60% 55%)"


{-| legend の値を並び順のまま(仮色の index の種)。 -}
legendValues : Doc -> List String
legendValues doc =
    List.map Tuple.second doc.legend


{-| 最初に一致する位置。無ければ 0(仮色の種に使うだけなので倒れてよい)。 -}
indexOf : String -> List String -> Int
indexOf target values =
    values
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( i, v ) ->
                if v == target then
                    Just i

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault 0


isHexColor : String -> Bool
isHexColor value =
    case String.uncons value of
        Just ( '#', rest ) ->
            (String.length rest == 3 || String.length rest == 6)
                && List.all Char.isHexDigit (String.toList rest)

        _ ->
            False


{-| パレット内の並び順(index)から仮色の色相を導く。黄金角(約137.5°)ずつ
離すので、少数の候補どうしが必ず大きく離れる(ハッシュ由来の偶然の近さがない)。
-}
goldenHue : Int -> Int
goldenHue index =
    modBy 360 (round (toFloat index * 137.508))



-- 選択の解決(消えたキーは先頭へ倒す)


shownGrid : Doc -> Model -> Maybe Edit
shownGrid doc model =
    docGrid doc model
        |> Maybe.map
            (\grid ->
                case model.working of
                    Just w ->
                        if w.sprite == grid.sprite && w.frame == grid.frame then
                            w

                        else
                            grid

                    Nothing ->
                        grid
            )


docGrid : Doc -> Model -> Maybe Edit
docGrid doc model =
    selectedSprite doc model
        |> Maybe.andThen
            (\sprite ->
                selectedFrame sprite model
                    |> Maybe.map (\( frameName, rows ) -> { sprite = sprite.name, frame = frameName, rows = rows })
            )


selectedSprite : Doc -> Model -> Maybe Sprite
selectedSprite doc model =
    or
        (model.spriteKey
            |> Maybe.andThen (\key -> doc.sprites |> List.filter (\s -> s.name == key) |> List.head)
        )
        (List.head doc.sprites)


selectedFrame : Sprite -> Model -> Maybe ( String, List String )
selectedFrame sprite model =
    or
        (model.frameKey
            |> Maybe.andThen (\key -> sprite.frames |> List.filter (\( n, _ ) -> n == key) |> List.head)
        )
        (List.head sprite.frames)


{-| いま塗る文字。選んだ色が legend から消えていたら先頭の色へ倒す。 -}
paintChar : Doc -> Model -> Char
paintChar doc model =
    let
        valid =
            model.color
                |> Maybe.andThen
                    (\c ->
                        if List.any (\( ch, _ ) -> ch == c) doc.legend then
                            Just c

                        else
                            Nothing
                    )
    in
    case valid of
        Just c ->
            c

        Nothing ->
            doc.legend
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault (transparentChar doc)


or : Maybe a -> Maybe a -> Maybe a
or first second =
    case first of
        Just _ ->
            first

        Nothing ->
            second


zoomLevels : List Int
zoomLevels =
    [ 16, 20, 24, 28, 34 ]


zoomStep : Int -> Int -> Int
zoomStep dir current =
    let
        index =
            zoomLevels
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, v ) -> v == current)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 2
    in
    zoomLevels
        |> List.drop (clamp 0 (List.length zoomLevels - 1) (index + dir))
        |> List.head
        |> Maybe.withDefault current



-- 表示


view : Dict String String -> Doc -> Model -> Html Msg
view resolved doc model =
    div [ HA.class "pixel-editor flex min-w-0 flex-1 bg-app" ]
        (case shownGrid doc model of
            Nothing ->
                -- fromDoc が絵の存在を保証するので普段は来ない(保険)
                [ div [ HA.class "empty m-auto text-ink-faint" ] [ text "この文書には絵がありません" ] ]

            Just grid ->
                [ viewTools model
                , viewCenter resolved doc model grid
                , viewPalette resolved doc model
                ]
        )


viewTools : Model -> Html Msg
viewTools model =
    div [ HA.class "px-tools flex w-14 shrink-0 flex-col items-center gap-1.5 border-r border-edge bg-panel py-2" ]
        [ toolButton model Pen "ペン(P)" iconPen
        , toolButton model Eraser "消しゴム(E)" iconEraser
        , toolButton model Bucket "バケツ(B)" iconBucket
        , toolButton model Dropper "スポイト(I)" iconDropper
        , div [ HA.class "my-1 h-px w-8 shrink-0 bg-edge" ] []
        , historyButton "戻す(⌘Z / Ctrl+Z)" UndoPressed (List.isEmpty model.undo) iconUndo
        , historyButton "やり直す(⇧⌘Z / Ctrl+Y)" RedoPressed (List.isEmpty model.redo) iconRedo
        ]


toolButton : Model -> Tool -> String -> Html Msg -> Html Msg
toolButton model tool label body =
    button
        [ HA.classList
            [ ( "px-tool flex h-10 w-10 shrink-0 cursor-pointer items-center justify-center rounded border", True )
            , ( "border-accent bg-accent/10 text-accent", model.tool == tool )
            , ( "border-transparent text-ink-soft hover:bg-white/5 hover:text-ink", model.tool /= tool )
            ]
        , HA.title label
        , HE.onClick (ToolChosen tool)
        ]
        [ body ]


historyButton : String -> Msg -> Bool -> Html Msg -> Html Msg
historyButton label msg disabled body =
    button
        [ HA.class "px-tool flex h-10 w-10 shrink-0 cursor-pointer items-center justify-center rounded border border-transparent text-ink-soft hover:bg-white/5 hover:text-ink disabled:cursor-default disabled:opacity-40 disabled:hover:bg-transparent"
        , HA.title label
        , HA.disabled disabled
        , HE.onClick msg
        ]
        [ body ]


viewCenter : Dict String String -> Doc -> Model -> Edit -> Html Msg
viewCenter resolved doc model grid =
    let
        cols =
            grid.rows |> List.head |> Maybe.map String.length |> Maybe.withDefault 0

        rowCount =
            List.length grid.rows
    in
    div [ HA.class "px-center flex min-w-0 flex-1 flex-col" ]
        [ viewChips doc model grid
        , div
            [ HA.class "px-stage flex min-h-0 flex-1 cursor-crosshair overflow-auto p-4 select-none"

            -- ショートカットはグリッドが焦点の時だけ(入力欄の ⌘Z を横取りしない)
            , HA.tabindex 0
            , HE.custom "keydown" keyDecoder
            , HE.custom "contextmenu"
                (D.succeed { message = Swallowed, stopPropagation = True, preventDefault = True })
            , HE.onMouseLeave GridLeft
            ]
            [ div [ HA.class "m-auto" ] [ viewGrid resolved doc model grid ] ]
        , viewStatus model cols rowCount
        ]


{-| 絵の名前チップ列(+コマが複数ある絵はコマのチップ列)。 -}
viewChips : Doc -> Model -> Edit -> Html Msg
viewChips doc model grid =
    let
        spriteChip sprite =
            chip (sprite.name == grid.sprite) (SpriteChosen sprite.name) sprite.name

        frameChips =
            selectedSprite doc model
                |> Maybe.map .frames
                |> Maybe.withDefault []

        groupRow ( name, sprites ) =
            div [ HA.class "flex items-start gap-2" ]
                [ span [ HA.class "mt-1 w-14 shrink-0 text-right text-[10px] text-ink-faint" ] [ text name ]
                , div [ HA.class "flex flex-wrap items-center gap-1" ] (List.map spriteChip sprites)
                ]
    in
    div [ HA.class "px-chips shrink-0 border-b border-edge bg-panel px-3 py-1.5" ]
        (div [ HA.class "flex flex-col gap-1" ] (groupsOf doc |> List.map groupRow)
            :: (if List.length frameChips > 1 then
                    [ div [ HA.class "mt-1 flex flex-wrap items-center gap-1" ]
                        (span [ HA.class "w-14 shrink-0 text-right text-[10px] text-ink-faint" ] [ text "コマ" ]
                            :: (frameChips
                                    |> List.map (\( name, _ ) -> chip (name == grid.frame) (FrameChosen name) name)
                               )
                            ++ [ viewPlayButton model ]
                        )
                    ]

                else
                    []
               )
        )


{-| 種別 → その中のスプライト。並びは *.sprite.json に書いた順(発明しない)。 -}
groupsOf : Doc -> List ( String, List Sprite )
groupsOf doc =
    doc.sprites
        |> List.foldl
            (\sprite acc ->
                if List.any (\( name, _ ) -> name == sprite.group) acc then
                    acc |> List.map
                        (\( name, xs ) ->
                            if name == sprite.group then
                                ( name, sprite :: xs )

                            else
                                ( name, xs )
                        )

                else
                    ( sprite.group, [ sprite ] ) :: acc
            )
            []
        |> List.map (\( name, xs ) -> ( name, List.reverse xs ))
        |> List.reverse


{-| コマ送りの入/切。動きは 1 コマずつ眺めても分からないので、並べて見るのではなく
実際に回して確かめられるようにする。編集を始めたら自動で止まる。
-}
viewPlayButton : Model -> Html Msg
viewPlayButton model =
    button
        [ HA.classList
            [ ( "chip btn rounded-full", True )
            , ( "border-transparent bg-accent text-white hover:bg-accent", model.playing )
            ]
        , HA.title "コマ送りで動きを確かめる"
        , HE.onClick PlayToggled
        ]
        [ text
            (if model.playing then
                "⏸ 止める"

             else
                "▶ 動かす"
            )
        ]


chip : Bool -> Msg -> String -> Html Msg
chip selected msg label =
    button
        [ HA.classList
            [ ( "chip btn rounded-full font-mono", True )
            , ( "border-transparent bg-accent text-white hover:bg-accent", selected )
            ]
        , HE.onClick msg
        ]
        [ text label ]


viewGrid : Dict String String -> Doc -> Model -> Edit -> Html Msg
viewGrid resolved doc model grid =
    let
        colors =
            palette resolved doc
                |> List.map (\sw -> ( sw.ch, sw.css ))
                |> Dict.fromList
    in
    div [ HA.class "px-grid border border-edge shadow-[0_2px_10px_rgb(0_0_0/0.4)]" ]
        (grid.rows |> List.indexedMap (viewGridRow colors model.cellPx))


viewGridRow : Dict Char String -> Int -> Int -> String -> Html Msg
viewGridRow colors cellPx y row =
    div [ HA.class "flex" ]
        (row |> String.toList |> List.indexedMap (\x ch -> viewCell colors cellPx x y ch))


viewCell : Dict Char String -> Int -> Int -> Int -> Char -> Html Msg
viewCell colors cellPx x y ch =
    div
        ([ HA.class "px-cell shrink-0"
         , HA.style "width" (String.fromInt cellPx ++ "px")
         , HA.style "height" (String.fromInt cellPx ++ "px")
         , HE.custom "mousedown"
            (D.field "button" D.int
                |> D.map (\b -> { message = CellPressed x y b, stopPropagation = True, preventDefault = False })
            )
         , HE.onMouseEnter (CellEntered x y)
         ]
            ++ (case Dict.get ch colors of
                    Just css ->
                        [ HA.style "background-color" css ]

                    Nothing ->
                        -- legend に無い文字 = 透明(市松)
                        [ HA.class "px-checker" ]
               )
        )
        []


viewStatus : Model -> Int -> Int -> Html Msg
viewStatus model cols rowCount =
    div [ HA.class "px-status flex h-8 shrink-0 items-center gap-2 border-t border-edge bg-panel px-3 text-[11px] text-ink-soft" ]
        [ button
            [ HA.class "btn btn-mini"
            , HA.title "ズーム −"
            , HA.disabled (model.cellPx <= smallestZoom)
            , HE.onClick (ZoomStepped -1)
            ]
            [ text "−" ]
        , span [ HA.class "w-10 text-center font-mono" ]
            [ text (String.fromInt (model.cellPx * 100 // 16) ++ "%") ]
        , button
            [ HA.class "btn btn-mini"
            , HA.title "ズーム +"
            , HA.disabled (model.cellPx >= largestZoom)
            , HE.onClick (ZoomStepped 1)
            ]
            [ text "+" ]
        , span [ HA.class "ml-2 font-mono text-ink-faint" ]
            [ text (String.fromInt cols ++ "×" ++ String.fromInt rowCount) ]
        , span [ HA.class "spacer flex-1" ] []
        , span [ HA.class "w-16 text-right font-mono text-ink-faint" ]
            [ text
                (case model.hover of
                    Just ( x, y ) ->
                        String.fromInt x ++ ", " ++ String.fromInt y

                    Nothing ->
                        ""
                )
            ]
        ]


smallestZoom : Int
smallestZoom =
    List.head zoomLevels |> Maybe.withDefault 16


largestZoom : Int
largestZoom =
    List.head (List.reverse zoomLevels) |> Maybe.withDefault 34


viewPalette : Dict String String -> Doc -> Model -> Html Msg
viewPalette resolved doc model =
    div [ HA.class "px-palette w-56 shrink-0 overflow-y-auto border-l border-edge bg-panel p-3" ]
        ([ div [ HA.class "mb-1.5 text-[11px] text-ink-faint" ] [ text "いまの色" ]
         , viewCurrentColor resolved doc model
         , div [ HA.class "mt-3 flex flex-wrap gap-1.5" ]
            ((palette resolved doc |> List.map (viewSwatch doc model))
                ++ [ button
                        [ HA.class "btn h-7 rounded-full"
                        , HE.onClick AddColorPressed
                        ]
                        [ text "◇ 色を足す" ]
                   ]
            )
         ]
        )


viewCurrentColor : Dict String String -> Doc -> Model -> Html Msg
viewCurrentColor resolved doc model =
    if model.tool == Eraser then
        div [ HA.class "px-checker flex h-14 items-center justify-center rounded border border-edge" ]
            [ span [ HA.class "rounded bg-app/70 px-1.5 py-0.5 text-[11px] text-ink" ] [ text "透明(消す)" ] ]

    else
        let
            ch =
                paintChar doc model

            name =
                doc.legend
                    |> List.filter (\( c, _ ) -> c == ch)
                    |> List.head
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault ""
        in
        div []
            [ div
                [ HA.class "h-14 rounded border border-edge"
                , HA.style "background-color" (colorCss resolved (legendValues doc) name)
                ]
                []
            , div [ HA.class "mt-1 truncate font-mono text-[11px] text-ink-soft" ]
                [ text (String.fromChar ch ++ "  " ++ name) ]
            ]


viewSwatch : Doc -> Model -> Swatch -> Html Msg
viewSwatch doc model swatch =
    let
        selected =
            model.tool /= Eraser && paintChar doc model == swatch.ch
    in
    button
        [ HA.classList
            [ ( "h-7 w-7 shrink-0 cursor-pointer rounded-sm border", True )
            , ( "border-accent ring-2 ring-accent/60", selected )
            , ( "border-edge hover:border-ink-faint", not selected )
            ]
        , HA.style "background-color" swatch.css
        , HA.title swatch.name
        , HE.onClick (ColorChosen swatch.ch)
        ]
        []



-- ショートカット(グリッドが焦点の時だけ届く keydown)


keyDecoder : D.Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
keyDecoder =
    D.map4
        (\key meta ctrl shift -> { key = key, meta = meta, ctrl = ctrl, shift = shift })
        (D.field "key" D.string)
        (D.field "metaKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "shiftKey" D.bool)
        |> D.andThen
            (\k ->
                case shortcutMsg k of
                    Just msg ->
                        D.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    Nothing ->
                        D.fail "他のキーは素通し"
            )


shortcutMsg : { key : String, meta : Bool, ctrl : Bool, shift : Bool } -> Maybe Msg
shortcutMsg k =
    case ( String.toLower k.key, k.meta || k.ctrl, k.shift ) of
        ( "z", True, False ) ->
            Just UndoPressed

        ( "z", True, True ) ->
            Just RedoPressed

        ( "y", True, _ ) ->
            Just RedoPressed

        ( "p", False, _ ) ->
            Just (ToolChosen Pen)

        ( "e", False, _ ) ->
            Just (ToolChosen Eraser)

        ( "b", False, _ ) ->
            Just (ToolChosen Bucket)

        ( "i", False, _ ) ->
            Just (ToolChosen Dropper)

        _ ->
            Nothing



-- アイコン(SVG 線画・20px)


icon : List (Svg.Svg Msg) -> Html Msg
icon paths =
    Svg.svg
        [ SA.viewBox "0 0 24 24"
        , SA.width "20"
        , SA.height "20"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "1.6"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        paths


iconPen : Html Msg
iconPen =
    icon [ Svg.path [ SA.d "M17 3a2.85 2.83 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5Z" ] [] ]


iconEraser : Html Msg
iconEraser =
    icon
        [ Svg.path [ SA.d "m7 21-4.3-4.3c-1-1-1-2.5 0-3.4l9.6-9.6c1-1 2.5-1 3.4 0l5.6 5.6c1 1 1 2.5 0 3.4L13 21" ] []
        , Svg.path [ SA.d "M22 21H7" ] []
        , Svg.path [ SA.d "m5 11 9 9" ] []
        ]


iconBucket : Html Msg
iconBucket =
    icon
        [ Svg.path [ SA.d "m19 11-8-8-8.6 8.6a2 2 0 0 0 0 2.8l5.2 5.2c.8.8 2 .8 2.8 0L19 11Z" ] []
        , Svg.path [ SA.d "m5 2 5 5" ] []
        , Svg.path [ SA.d "M2 13h15" ] []
        , Svg.path [ SA.d "M22 20a2 2 0 1 1-4 0c0-1.6 1.7-2.4 2-4 .3 1.6 2 2.4 2 4Z" ] []
        ]


iconDropper : Html Msg
iconDropper =
    icon
        [ Svg.path [ SA.d "m2 22 1-1h3l9-9" ] []
        , Svg.path [ SA.d "M3 21v-3l9-9" ] []
        , Svg.path [ SA.d "m15 6 3.4-3.4a2.1 2.1 0 1 1 3 3L18 9l.4.4a2.1 2.1 0 1 1-3 3l-3.8-3.8a2.1 2.1 0 1 1 3-3l.4.4Z" ] []
        ]


iconUndo : Html Msg
iconUndo =
    icon
        [ Svg.path [ SA.d "M9 14 4 9l5-5" ] []
        , Svg.path [ SA.d "M4 9h10.5a5.5 5.5 0 0 1 5.5 5.5 5.5 5.5 0 0 1-5.5 5.5H11" ] []
        ]


iconRedo : Html Msg
iconRedo =
    icon
        [ Svg.path [ SA.d "m15 14 5-5-5-5" ] []
        , Svg.path [ SA.d "M20 9H9.5A5.5 5.5 0 0 0 4 14.5 5.5 5.5 0 0 0 9.5 20H13" ] []
        ]
