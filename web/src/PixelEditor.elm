module PixelEditor exposing
    ( Colors
    , Doc
    , Edit
    , Model
    , Msg(..)
    , Out(..)
    , ellipseCells
    , floodAt
    , fromDoc
    , goldenHue
    , groupsOf
    , init
    , isPlaying
    , lineCells
    , paintAt
    , paintCells
    , rectCells
    , palette
    , pickAt
    , playIntervalMs
    , release
    , strokeActive
    , transparentChar
    , update
    , view
    )

{-| ドット絵(*.sprite.json)のクイック編集。

左に道具(ペン・消しゴム・バケツ・スポイト・戻す/やり直す)、中央にセルの
グリッド、右にパレット(legend から導く)。

文書の元データはここに置かない — 描画は毎回親から渡される Doc(パース済み
docText)から導き、一筆(pointerdown〜up)の途中だけ作業コピー(working)が
表示に勝つ。一筆の確定は rows 丸ごと 1 本の Edit として親へ返し、親の
編集直列(docEdit → dirty → 保存)に乗せる — 並行の保存経路は作らない。
戻す/やり直すもスナップショットの Edit を同じ経路で流す。

legend の値は意味色キー(テーマが解く名前)のことがあり、ここでは実色に
解けない。サーバの解決表(POST /sprite/colors の「値 → #rrggbb」)があれば
それを使い、無い・欠けるキーは #rrggbb 素通し → legend 内の並び順から導く
仮色、の順に倒す(表示のためだけ。保存される文字には関与しない)。
仮色に倒れた色はパレットに「?」を出す — 黙って倒れると「テーマの色が
出ていない」不具合と見分けが付かない。

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, option, select, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
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
    -- 束ねて見出しを付ける。宣言が無ければ名前から導く。空 = 見出しを付けない
    , group : String
    , frames : List ( String, List String )

    -- 動き。どのコマをどの順で回すか。frames には順番も時間も無いので、
    -- ここが無ければ再生できる物が何も無い
    , clips : List ( String, Clip )
    }


type alias Clip =
    { frames : List String
    , fps : Int
    , loop : String
    }


{-| group の宣言が無い絵の見出しを、名前から導く。`_` までの頭を見て、
同じ頭を持つ絵が 2 枚以上あればそれを見出しにする(`tile_grass_a` →
`tile`)。相手が居なければ空 = 見出しを付けない。

「その他」のような固定の見出しは付けない — 中身を何も言っていないうえ、
全部がそこへ落ちると見出しの意味が消える(タイル集では 41 枚全部が入った)。
-}
withDerivedGroups : List Sprite -> List Sprite
withDerivedGroups sprites =
    let
        counts =
            sprites
                |> List.foldl
                    (\sprite acc ->
                        if String.isEmpty sprite.group then
                            Dict.update (namePrefix sprite.name)
                                (\n -> Just (1 + Maybe.withDefault 0 n))
                                acc

                        else
                            acc
                    )
                    Dict.empty
    in
    sprites
        |> List.map
            (\sprite ->
                if String.isEmpty sprite.group then
                    let
                        prefix =
                            namePrefix sprite.name
                    in
                    if Maybe.withDefault 0 (Dict.get prefix counts) >= 2 then
                        { sprite | group = prefix }

                    else
                        sprite

                else
                    sprite
            )


namePrefix : String -> String
namePrefix name =
    case String.split "_" name of
        head :: _ :: _ ->
            head

        _ ->
            name


{-| 編集 1 件 = 1 フレームの rows 丸ごと。 -}
type alias Edit =
    { sprite : String
    , frame : String
    , rows : List String
    }


{-| パース済み sprite Doc の読み取り。legend が読めない・絵が 1 枚も残らない
文書は Nothing — 呼び側は従来のフォーム/コード表示へ倒す(壊さない)。
崩れた rows(空・行の長さ不揃い)はフレーム単位で黙って除く。
-}
fromDoc : D.Value -> Maybe Doc
fromDoc value =
    case ( D.decodeValue legendDecoder value, D.decodeValue spritesDecoder value ) of
        ( Ok legend, Ok raw ) ->
            let
                sprites =
                    List.filterMap cleanSprite raw |> withDerivedGroups
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
                                    |> Result.withDefault ""
                            , frames = frames
                            , clips = clipsOf value
                            }
                )


{-| clips: { "walk_down": { frames: [...], fps: 6, loop: "pingpong" } }。
コマを 1 枚も挙げていない動きは捨てる(再生しても何も起きないため)。
-}
clipsOf : D.Value -> List ( String, Clip )
clipsOf value =
    D.decodeValue (D.field "clips" (D.keyValuePairs clipDecoder)) value
        |> Result.withDefault []
        |> List.filter (\( name, clip ) -> not (String.startsWith "//" name) && not (List.isEmpty clip.frames))


clipDecoder : D.Decoder Clip
clipDecoder =
    D.map3 Clip
        (D.field "frames" (D.list D.string))
        (D.oneOf [ D.field "fps" D.int, D.succeed 8 ])
        (D.oneOf [ D.field "loop" D.string, D.succeed "forward" ])


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
    | Line
    | Rect
    | Ellipse


{-| 形の道具の一筆。押した所を覚えておいて、動かす間は毎回 before から
引き直す(前のプレビューの上に重ねると、引き直しの跡が残る)。
-}
type alias Shape =
    { tool : Tool
    , start : ( Int, Int )
    }


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

    -- 選択中の絵とフレーム。文書から消えていたら先頭へ倒す(view/update で毎回解決)
    , spriteKey : Maybe String
    , frameKey : Maybe String
    , cellPx : Int

    -- ピンチの溜め込み。1 回のピンチで何十回もイベントが来るので、
    -- しきい値を超えた時だけ 1 段動かす(段ごとに動かすと一気に端まで飛ぶ)
    , zoomAccum : Float
    , hover : Maybe ( Int, Int )

    -- 一筆の途中(Just = 描いている)。before は戻す用の一筆前。
    -- shape が立っていれば、動かす間は形のプレビューを before から引き直す
    , stroke : Maybe { ch : Char, before : List String, shape : Maybe Shape }

    -- 表示が文書に勝つ作業コピー(一筆の途中と、確定後のエコー待ちの間)
    , working : Maybe Edit
    , undo : List Snapshot
    , redo : List Snapshot

    -- コマ送りで動きを見せている最中か。編集を始めたら止める
    , playing : Bool

    -- 見ている動き(clips の名前)。Nothing はその絵の先頭の動き
    , clipKey : Maybe String
    }


init : Model
init =
    { tool = Pen
    , color = Nothing
    , spriteKey = Nothing
    , frameKey = Nothing
    , cellPx = 24
    , zoomAccum = 0
    , hover = Nothing
    , stroke = Nothing
    , working = Nothing
    , undo = []
    , redo = []
    , playing = False
    , clipKey = Nothing
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


{-| フレーム送り中か(親が時計を購読する間だけ True)。 -}
isPlaying : Model -> Bool
isPlaying model =
    model.playing


{-| コマ送りの間隔。速さは Doc の動きが持つ — ここで勝手に決めると、
Studio の見え方とゲームの動きが食い違う。
-}
playIntervalMs : Doc -> Model -> Float
playIntervalMs doc model =
    1000 / toFloat (max 1 (selectedClip doc model |> Maybe.map (Tuple.second >> .fps) |> Maybe.withDefault 8))


{-| 繰り返さない動きの最後のコマに居るか(そこで再生を止める)。 -}
isLastOfOnce : Doc -> Model -> Bool
isLastOfOnce doc model =
    (selectedClip doc model |> Maybe.map (Tuple.second >> .loop) |> Maybe.withDefault "forward")
        == "once"
        && frameIndexOf doc model + 1 >= frameCountOf doc model


{-| いま見ている動き。選んでいなければその絵の先頭の動き。 -}
selectedClip : Doc -> Model -> Maybe ( String, Clip )
selectedClip doc model =
    case selectedSprite doc model of
        Nothing ->
            Nothing

        Just sprite ->
            case model.clipKey |> Maybe.andThen (\key -> sprite.clips |> List.filter (\( n, _ ) -> n == key) |> List.head) of
                Just found ->
                    Just found

                Nothing ->
                    List.head sprite.clips


{-| 動きが並べるコマ。動きが無い絵では、その絵の全コマを並べる
(選んで描くためには要る。ただし「順に回す」意味は無いので再生はさせない)。
-}
clipFrameNames : Doc -> Model -> List String
clipFrameNames doc model =
    case selectedClip doc model of
        Just ( _, clip ) ->
            clip.frames

        Nothing ->
            selectedSprite doc model
                |> Maybe.map (.frames >> List.map Tuple.first)
                |> Maybe.withDefault []


{-| いま見ている動きの中でコマを dir だけ送った先。端は動きの回し方に従う。 -}
stepFrameKey : Int -> Doc -> Model -> Maybe String
stepFrameKey dir doc model =
    let
        names =
            clipFrameNames doc model

        count =
            List.length names

        loop =
            selectedClip doc model |> Maybe.map (Tuple.second >> .loop) |> Maybe.withDefault "forward"

        next =
            frameIndexOf doc model + dir
    in
    if count == 0 then
        model.frameKey

    else if loop == "once" && (next < 0 || next >= count) then
        model.frameKey

    else
        names |> List.drop (modBy count next) |> List.head


frameCountOf : Doc -> Model -> Int
frameCountOf doc model =
    clipFrameNames doc model |> List.length


{-| いま見ているコマが動きの中で何番目か。見つからなければ先頭とみなす。 -}
frameIndexOf : Doc -> Model -> Int
frameIndexOf doc model =
    case model.frameKey of
        Just current ->
            clipFrameNames doc model
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, name ) -> name == current)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        Nothing ->
            0



-- 更新


type Msg
    = ToolChosen Tool
    | ColorChosen Char
    | AddColorPressed
    | SpriteChosen String
    | FrameChosen String
    | PlayToggled
    | PlayTicked
    | ClipChosen String
    | FrameStepped Int
    | ZoomStepped Int
    | ZoomPinched Float
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
            if isLastOfOnce doc model then
                -- 繰り返さない設定では最後のコマで止める(見た形のまま残す)
                ( { model | playing = False }, Silent )

            else
                ( { model | frameKey = stepFrameKey 1 doc model, working = Nothing }, Silent )

        ClipChosen name ->
            -- 動きを変えたら、その動きの先頭のコマから見せる
            ( { model
                | clipKey = Just name
                , frameKey = selectedSprite doc model
                    |> Maybe.andThen (\sp -> sp.clips |> List.filter (\( n, _ ) -> n == name) |> List.head)
                    |> Maybe.andThen (Tuple.second >> .frames >> List.head)
                , working = Nothing
              }
            , Silent
            )

        FrameStepped dir ->
            -- 手で送る間は自動送りを止める(勝手に進むと狙ったコマで止まれない)
            ( { model | frameKey = stepFrameKey dir doc model, working = Nothing, playing = False }, Silent )

        FrameChosen name ->
            ( { model | frameKey = Just name, working = Nothing, stroke = Nothing }, Silent )

        ZoomStepped dir ->
            ( { model | cellPx = zoomStep dir model.cellPx, zoomAccum = 0 }, Silent )

        ZoomPinched deltaY ->
            let
                accum =
                    -- 向きが変わったら溜めを捨てる。残しておくと、指を戻した時に
                    -- 打ち消し合って動かない間が生まれる
                    if deltaY * model.zoomAccum < 0 then
                        deltaY

                    else
                        model.zoomAccum + deltaY
            in
            if abs accum < pinchStepThreshold then
                ( { model | zoomAccum = accum }, Silent )

            else
                -- 指を広げる(deltaY が負)= 拡大
                ( { model
                    | cellPx =
                        zoomStep
                            (if accum < 0 then
                                1

                             else
                                -1
                            )
                            model.cellPx
                    , zoomAccum = 0
                  }
                , Silent
                )

        CellEntered x y ->
            let
                m1 =
                    { model | hover = Just ( x, y ) }
            in
            case ( m1.stroke, m1.working ) of
                ( Just stroke, Just w ) ->
                    case stroke.shape of
                        Nothing ->
                            ( { m1 | working = Just { w | rows = paintAt stroke.ch ( x, y ) w.rows } }, Silent )

                        Just shape ->
                            ( { m1
                                | working =
                                    Just
                                        { w
                                            | rows =
                                                paintCells stroke.ch
                                                    (shapeCells shape.tool shape.start ( x, y ))
                                                    stroke.before
                                        }
                              }
                            , Silent
                            )

                _ ->
                    ( m1, Silent )

        GridLeft ->
            ( { model | hover = Nothing }, Silent )

        CellPressed x y buttonId ->
            -- 動いている絵に描くと狙いが外れるので、描き始めたらフレーム送りを止める
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

                    Line ->
                        ( startShape Line (paintChar doc model) cell grid model, Silent )

                    Rect ->
                        ( startShape Rect (paintChar doc model) cell grid model, Silent )

                    Ellipse ->
                        ( startShape Ellipse (paintChar doc model) cell grid model, Silent )

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
        | stroke = Just { ch = ch, before = grid.rows, shape = Nothing }
        , working = Just { grid | rows = paintAt ch cell grid.rows }
    }


startShape : Tool -> Char -> ( Int, Int ) -> Edit -> Model -> Model
startShape tool ch cell grid model =
    { model
        | stroke = Just { ch = ch, before = grid.rows, shape = Just { tool = tool, start = cell } }
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


{-| 戻す/やり直すの適用。対象のフレームへ選択も移す — どの絵が戻ったのか見えるように。 -}
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


{-| 一覧のセルをまとめて ch へ。行ごとに 1 回だけ書き換える —
paintAt を 1 セルずつ畳むと、セルの数 × 行の数だけ行を触ることになる。

触らなかった行はそのまま返す。格子の lazy が行の同一性で当たり外れを
決めるので、中身が同じでも作り直すと全升の組み直しになる。
-}
paintCells : Char -> List ( Int, Int ) -> List String -> List String
paintCells ch cells rows =
    let
        xsByRow =
            cells
                |> List.foldl
                    (\( x, y ) acc -> Dict.update y (\xs -> Just (x :: Maybe.withDefault [] xs)) acc)
                    Dict.empty
    in
    rows
        |> List.indexedMap
            (\y row ->
                case Dict.get y xsByRow of
                    Nothing ->
                        row

                    Just xs ->
                        let
                            marks =
                                Set.fromList xs
                        in
                        row
                            |> String.toList
                            |> List.indexedMap
                                (\x c ->
                                    if Set.member x marks then
                                        ch

                                    else
                                        c
                                )
                            |> String.fromList
            )


{-| 形の道具が塗るセルの一覧。ワイルドカードを使わず全部並べるのは、
Tool を増やしたときコンパイラに漏れを教えてもらうため。形の道具以外は
shape が立たないのでここへは来ないが、来ても直線で害がない。
-}
shapeCells : Tool -> ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
shapeCells tool start current =
    case tool of
        Line ->
            lineCells start current

        Rect ->
            rectCells start current

        Ellipse ->
            ellipseCells start current

        Pen ->
            lineCells start current

        Bucket ->
            lineCells start current

        Eraser ->
            lineCells start current

        Dropper ->
            lineCells start current


{-| 2 点を結ぶ直線のセル。長い方の軸の歩数で、2 点の間をなめらかにつなぐ。 -}
lineCells : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
lineCells ( x0, y0 ) ( x1, y1 ) =
    let
        steps =
            max (abs (x1 - x0)) (abs (y1 - y0))
    in
    if steps == 0 then
        [ ( x0, y0 ) ]

    else
        List.range 0 steps
            |> List.map
                (\i ->
                    let
                        t =
                            toFloat i / toFloat steps
                    in
                    ( round (toFloat x0 + toFloat (x1 - x0) * t)
                    , round (toFloat y0 + toFloat (y1 - y0) * t)
                    )
                )


{-| 2 点を対角とする四角の枠線のセル。中は塗らない —
塗り潰しはバケツで一撫でできるので、塗り潰し用の道具を別に増やさない。
-}
rectCells : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
rectCells ( x0, y0 ) ( x1, y1 ) =
    let
        left =
            min x0 x1

        right =
            max x0 x1

        top =
            min y0 y1

        bottom =
            max y0 y1
    in
    Set.toList
        (Set.fromList
            ((List.range left right |> List.concatMap (\x -> [ ( x, top ), ( x, bottom ) ]))
                ++ (List.range top bottom |> List.concatMap (\y -> [ ( left, y ), ( right, y ) ]))
            )
        )


{-| 外接矩形に内接する枠線の楕円のセル。列走査と行走査の両方を重ねるのは、
片方だけだと細長い楕円で急な曲がりの所に穴が開くため(Set で重複は消える)。
-}
ellipseCells : ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
ellipseCells ( x0, y0 ) ( x1, y1 ) =
    let
        left =
            min x0 x1

        right =
            max x0 x1

        top =
            min y0 y1

        bottom =
            max y0 y1

        cx =
            toFloat (left + right) / 2

        cy =
            toFloat (top + bottom) / 2

        rx =
            toFloat (right - left) / 2

        ry =
            toFloat (bottom - top) / 2
    in
    if rx == 0 || ry == 0 then
        lineCells ( x0, y0 ) ( x1, y1 )

    else
        let
            byColumn =
                List.range left right
                    |> List.concatMap
                        (\x ->
                            let
                                t =
                                    (toFloat x - cx) / rx

                                dy =
                                    ry * sqrt (max 0 (1 - t * t))
                            in
                            [ ( x, round (cy - dy) ), ( x, round (cy + dy) ) ]
                        )

            byRow =
                List.range top bottom
                    |> List.concatMap
                        (\y ->
                            let
                                t =
                                    (toFloat y - cy) / ry

                                dx =
                                    rx * sqrt (max 0 (1 - t * t))
                            in
                            [ ( round (cx - dx), y ), ( round (cx + dx), y ) ]
                        )
        in
        Set.toList (Set.fromList (byColumn ++ byRow))


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


{-| サーバの色解決の結果。table は「legend の値 → #rrggbb」、unresolved は実色に
解けなかった値。Api.SpriteColors と欄名を合わせてあるのでそのまま渡せる。
-}
type alias Colors =
    { table : Dict String String
    , unresolved : Set String
    }


type alias Swatch =
    { ch : Char
    , name : String
    , css : String

    -- 実色が分からず仮色で描いている升目(パレットに印を出す)
    , guessed : Bool
    }


{-| legend の並びのまま升目にする。名前は legend の値そのもの(名詞を発明しない)。 -}
palette : Colors -> Doc -> List Swatch
palette colors doc =
    doc.legend
        |> List.map
            (\( ch, name ) ->
                { ch = ch
                , name = name
                , css = colorCss colors.table (legendValues doc) name
                , guessed = isGuessed colors name
                }
            )


{-| 実色に解けていない値か。サーバの unresolved が正。
WhyNot: それだけに頼らない — unresolved を返さない古いサーバでも印が出るよう、
「表にも無く #rrggbb でもない」(= 仮色に倒れた) も同じ扱いにする。
-}
isGuessed : Colors -> String -> Bool
isGuessed colors name =
    Set.member name colors.unresolved
        || (Dict.get name colors.table == Nothing && not (isHexColor name))


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
    [ 8, 12, 16, 20, 24, 32, 44, 64 ]


{-| ピンチをこれだけ溜めたら 1 段動かす。トラックパッドのピンチは 1 回の
delta が数しか無いので、大きく取ると指を動かしても反応しない。マウスの
ホイールは 1 目盛りで 100 前後来るので、こちらは 1 目盛り 1 段になる。
-}
pinchStepThreshold : Float
pinchStepThreshold =
    8


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


view : Colors -> Doc -> Model -> Html Msg
view colors doc model =
    div [ HA.class "pixel-editor flex min-w-0 flex-1 bg-app" ]
        (case shownGrid doc model of
            Nothing ->
                -- fromDoc が絵の存在を保証するので普段は来ない(保険)
                [ div [ HA.class "empty m-auto text-ink-faint" ] [ text "この文書には絵がありません" ] ]

            Just grid ->
                [ viewTools model
                , viewSprites doc grid
                , viewCenter colors doc model grid
                , viewPalette colors doc model
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
        , toolButton model Line "直線(L) — 押した所から離した所まで" iconLine
        , toolButton model Rect "矩形(R) — 2 点を対角とする四角の枠" iconRect
        , toolButton model Ellipse "楕円(O) — 2 点を対角とするだ円の枠" iconEllipse
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


viewCenter : Colors -> Doc -> Model -> Edit -> Html Msg
viewCenter colors doc model grid =
    let
        cols =
            grid.rows |> List.head |> Maybe.map String.length |> Maybe.withDefault 0

        rowCount =
            List.length grid.rows
    in
    div [ HA.class "px-center flex min-w-0 flex-1 flex-col" ]
        [ div
            [ HA.class "px-stage flex min-h-0 flex-1 cursor-crosshair overflow-auto p-4 select-none"

            -- ショートカットはグリッドが焦点の時だけ(入力欄の ⌘Z を横取りしない)
            , HA.tabindex 0
            , HE.custom "keydown" keyDecoder
            , HE.custom "contextmenu"
                (D.succeed { message = Swallowed, stopPropagation = True, preventDefault = True })
            , HE.onMouseLeave GridLeft
            , HE.custom "wheel" wheelDecoder
            ]
            [ div
                [ HA.class "relative m-auto"

                -- 倍率はここ 1 つ。HA.style はカスタムプロパティを扱えないので属性で渡す
                , HA.attribute "style" ("--px-cell:" ++ String.fromInt model.cellPx ++ "px")
                ]
                [ viewGrid colors doc grid
                , viewCursor model.cellPx model.hover
                ]
            ]
        , viewFrames doc model grid
        , viewStatus model cols rowCount
        ]


{-| 絵の一覧。縦に積んで縦スクロールする — 横へ折り返して並べると、絵が
41 枚あるタイル集で 10 行に膨らみ、格子が画面の外へ出る。ドット絵ツールが
素材の一覧を脇の縦列に置いているのと同じ形。

**コマ(フレーム)とは別の場所に置く。** 同じ形のチップを 2 段並べると、
「別の絵に切り替える」と「同じ絵の別のコマを見る」が同じ操作に見える。
-}
viewSprites : Doc -> Edit -> Html Msg
viewSprites doc grid =
    div [ HA.class "px-sprites flex w-40 shrink-0 flex-col overflow-y-auto border-r border-edge bg-panel py-1" ]
        (groupsOf doc |> List.concatMap (spriteGroup grid))


spriteGroup : Edit -> ( String, List Sprite ) -> List (Html Msg)
spriteGroup grid ( name, sprites ) =
    (if String.isEmpty name then
        -- 束ねる相手が居ない絵。見出しを作らない — 中身を言い当てていない
        -- 見出し(「その他」)を出すと、名前を読む手がかりが 1 つ減る
        []

     else
        [ div
            [ HA.class "sticky top-0 z-10 bg-panel px-2 pt-1.5 pb-0.5 text-[10px] tracking-wide text-ink-faint" ]
            [ text name ]
        ]
    )
        ++ List.map (spriteRow grid) sprites


spriteRow : Edit -> Sprite -> Html Msg
spriteRow grid sprite =
    button
        [ HA.classList
            [ ( "px-sprite w-full shrink-0 cursor-pointer truncate px-2 py-1 text-left font-mono text-[11px]", True )
            , ( "bg-accent text-white", sprite.name == grid.sprite )
            , ( "text-ink-soft hover:bg-well", sprite.name /= grid.sprite )
            ]
        , HA.title sprite.name
        , HE.onClick (SpriteChosen sprite.name)
        ]
        [ text sprite.name ]


{-| コマ(フレーム)の帯。格子の真下に置く — 同じ絵の中の話なので、
絵を選ぶ列(脇の縦列)とは離す。1 コマしか無い絵では帯を出さない。
-}
viewFrames : Doc -> Model -> Edit -> Html Msg
viewFrames doc model grid =
    let
        frames =
            clipFrameNames doc model
    in
    if List.length frames <= 1 && List.isEmpty (selectedSprite doc model |> Maybe.map .clips |> Maybe.withDefault []) then
        text ""

    else
        -- 操作とコマを別の段に置く。同じ行に並べると、コマの多い絵で操作が
        -- 押し出されるうえ、「押す物」と「選ぶ物」が 1 列に混ざって読めない
        div [ HA.class "px-frames shrink-0 border-t border-edge bg-panel" ]
            [ div [ HA.class "flex h-9 items-center gap-2 px-2" ] [ viewTransport doc model ]
            , div [ HA.class "flex h-9 items-center gap-1 overflow-x-auto border-t border-edge/50 px-2" ]
                (span [ HA.class "shrink-0 pr-1 text-[10px] text-ink-faint" ] [ text "コマ" ]
                    :: (frames |> List.indexedMap (\i name -> frameTab (name == grid.frame) i name))
                )
            ]


{-| コマ送りの操作と、いま見ている動きの選択。左端に固定して、いつでも同じ場所にある。

動きが 1 つも書かれていない絵では再生を伏せる — frames は「名前の付いた姿勢の束」で
順番も時間も持たないので、並び順に回しても意味のある動きにならない
(向き違いの立ち絵が順番に出るだけになる)。
-}
viewTransport : Doc -> Model -> Html Msg
viewTransport doc model =
    case selectedClip doc model of
        Nothing ->
            div [ HA.class "flex shrink-0 items-center gap-2 text-[10px] text-ink-faint" ]
                [ text "この絵には動き(clips)がありません。コマを選んで描けます" ]

        Just ( clipName, clip ) ->
            viewTransportOf doc model clipName clip


viewTransportOf : Doc -> Model -> String -> Clip -> Html Msg
viewTransportOf doc model clipName clip =
    div [ HA.class "flex shrink-0 items-center gap-1" ]
        [ viewClipPicker (selectedSprite doc model |> Maybe.map .clips |> Maybe.withDefault []) clipName
        , transportButton "前のコマ" False (FrameStepped -1) iconStepBack
        , transportButton
            (if model.playing then
                "止める"

             else
                "動かす"
            )
            model.playing
            PlayToggled
            (if model.playing then
                iconPause

             else
                iconPlay
            )
        , transportButton "次のコマ" False (FrameStepped 1) iconStepForward

        -- 速さと回し方は Doc の値。ここで変えられるようにすると、Studio の
        -- 見え方とゲームの動きが黙って食い違う
        , span [ HA.class "shrink-0 pl-1 font-mono text-[10px] text-ink-faint" ]
            [ text (String.fromInt clip.fps ++ " fps・" ++ loopLabel clip.loop) ]
        ]


loopLabel : String -> String
loopLabel loop =
    case loop of
        "pingpong" ->
            "往復"

        "once" ->
            "1 回"

        _ ->
            "繰り返し"


{-| いま見ている絵が持つ動きだけを並べる。文書の全部の動きを並べると、
選んでも今の絵に無い名前なので何も起きず、表示だけ元へ戻る。
-}
viewClipPicker : List ( String, Clip ) -> String -> Html Msg
viewClipPicker clips current =
    select
        [ HA.class "h-6 shrink-0 cursor-pointer rounded border border-edge bg-well px-1 font-mono text-[11px] text-ink-soft"
        , HA.title "どの動きを確かめるか"
        , HA.value current
        , HE.onInput ClipChosen
        ]
        (clips |> List.map (\( name, _ ) -> option [ HA.value name, HA.selected (name == current) ] [ text name ]))


transportButton : String -> Bool -> Msg -> Html Msg -> Html Msg
transportButton label active msg body =
    button
        [ HA.classList
            [ ( "flex h-6 w-6 shrink-0 cursor-pointer items-center justify-center rounded border", True )
            , ( "border-accent bg-accent/15 text-accent", active )
            , ( "border-edge text-ink-soft hover:border-ink-faint", not active )
            ]
        , HA.title label
        , HA.attribute "aria-label" label
        , HE.onClick msg
        ]
        [ body ]


frameTab : Bool -> Int -> String -> Html Msg
frameTab selected index name =
    button
        [ HA.classList
            [ ( "px-frame flex h-7 shrink-0 cursor-pointer items-center gap-1.5 rounded-sm border px-2 text-[11px]", True )
            , ( "border-accent bg-accent/15 text-accent", selected )
            , ( "border-edge text-ink-soft hover:border-ink-faint", not selected )
            ]
        , HA.title name
        , HE.onClick (FrameChosen name)
        ]
        [ span [ HA.class "font-mono text-[10px] opacity-60" ] [ text (String.fromInt (index + 1)) ]
        , span [ HA.class "font-mono" ] [ text name ]
        ]


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


{-| カーソルの居るセルを囲む枠。格子の上に 1 枚だけ重ねる。

CSS の :hover では出さない — ブラウザは DOM が入れ替わっても、マウスが動くまで
:hover を付け替えない。undo のように指を止めたまま升の中身が入れ替わると、
もう指の下に無い升に枠が残る(複数同時に残ることもある)。

hover が Nothing の時も器は出して display で消す — 子の数が変われば格子の
ノードまで差分の対象になるので、数は 2 に固定しておく。
-}
viewCursor : Int -> Maybe ( Int, Int ) -> Html Msg
viewCursor cellPx hover =
    case hover of
        Nothing ->
            div [ HA.class "px-cursor hidden" ] []

        Just ( x, y ) ->
            div
                [ HA.class "px-cursor"

                -- +1 は格子の枠線の分
                , HA.style "left" (String.fromInt (x * cellPx + 1) ++ "px")
                , HA.style "top" (String.fromInt (y * cellPx + 1) ++ "px")
                , HA.style "width" (String.fromInt cellPx ++ "px")
                , HA.style "height" (String.fromInt cellPx ++ "px")
                ]
                []


{-| 格子は「色 × rows」だけで決まる。lazy に包んで、それ以外の動き
(hover の座標表示・道具の切り替え・ズーム)では作り直さない — 32×24 の絵で 768 升あり、
カーソルがセルを跨ぐたびに全升を組み直すと、一筆が目に見えて遅れる。

lazy の比較は参照なので、色の辞書はここでなく中で作る(毎回作ると必ず外れる)。
rows は文書か working からそのまま来るので、塗った時だけ参照が変わる。
倍率は引数に取らない — CSS の --px-cell で親から降ってくるので、ズームでは
升を 1 つも作り直さずに済む。
-}
viewGrid : Colors -> Doc -> Edit -> Html Msg
viewGrid colors doc grid =
    HL.lazy3 viewGridBody colors doc grid.rows


viewGridBody : Colors -> Doc -> List String -> Html Msg
viewGridBody colors doc rows =
    let
        cellCss =
            palette colors doc
                |> List.map (\sw -> ( sw.ch, sw.css ))
                |> Dict.fromList
    in
    div [ HA.class "px-grid border border-edge shadow-[0_2px_10px_rgb(0_0_0/0.4)]" ]
        (rows |> List.indexedMap (viewGridRow cellCss))


viewGridRow : Dict Char String -> Int -> String -> Html Msg
viewGridRow colors y row =
    div [ HA.class "flex" ]
        (row |> String.toList |> List.indexedMap (\x ch -> viewCell colors x y ch))


viewCell : Dict Char String -> Int -> Int -> Char -> Html Msg
viewCell colors x y ch =
    div
        ([ HA.class "px-cell shrink-0"
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
            , HA.title "縮小(ピンチイン・⌘ホイールでも)"
            , HA.disabled (model.cellPx <= smallestZoom)
            , HE.onClick (ZoomStepped -1)
            ]
            [ iconZoomOut ]
        , span [ HA.class "w-10 text-center font-mono" ]
            [ text (String.fromInt (model.cellPx * 100 // 16) ++ "%") ]
        , button
            [ HA.class "btn btn-mini"
            , HA.title "拡大(ピンチアウト・⌘ホイールでも)"
            , HA.disabled (model.cellPx >= largestZoom)
            , HE.onClick (ZoomStepped 1)
            ]
            [ iconZoomIn ]
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
    List.head (List.reverse zoomLevels) |> Maybe.withDefault 64


viewPalette : Colors -> Doc -> Model -> Html Msg
viewPalette colors doc model =
    let
        swatches =
            palette colors doc
    in
    div [ HA.class "px-palette w-56 shrink-0 overflow-y-auto border-l border-edge bg-panel p-3" ]
        ([ div [ HA.class "mb-1.5 text-[11px] text-ink-faint" ] [ text "いまの色" ]
         , viewCurrentColor colors doc model
         , div [ HA.class "mt-3 flex flex-wrap gap-1.5" ]
            ((swatches |> List.map (viewSwatch doc model))
                ++ [ button
                        [ HA.class "btn h-7 rounded-full"
                        , HE.onClick AddColorPressed
                        ]
                        [ text "◇ 色を足す" ]
                   ]
            )
         ]
            ++ viewGuessedNote swatches
        )


{-| 仮色の升目が 1 つでもあるときだけ出す説明。印の意味が分からないと
「なぜか色が違う」で終わってしまう。
-}
viewGuessedNote : List Swatch -> List (Html Msg)
viewGuessedNote swatches =
    if List.any .guessed swatches then
        [ div [ HA.class "mt-3 rounded border border-dashed border-ink-faint/60 p-2 text-[11px] leading-relaxed text-ink-faint" ]
            [ text "? の色はテーマに無く、仮の色で描いています。ゲームが場面ごとに決めている色(味方と敵で塗り分ける等)かもしれません。テーマに足すとゲームと同じ色になります。" ]
        ]

    else
        []


viewCurrentColor : Colors -> Doc -> Model -> Html Msg
viewCurrentColor colors doc model =
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
                , HA.style "background-color" (colorCss colors.table (legendValues doc) name)
                ]
                []
            , div [ HA.class "mt-1 truncate font-mono text-[11px] text-ink-soft" ]
                [ text (String.fromChar ch ++ "  " ++ name) ]
            ]


{-| 升目 1 つ。実色が分からない色は枠を点線にして「?」を重ねる。
WhyNot: 絵そのものは仮色のまま描く — 印のために塗るのをやめると、
色が解けないだけで絵が読めなくなり、編集が止まってしまう。
-}
viewSwatch : Doc -> Model -> Swatch -> Html Msg
viewSwatch doc model swatch =
    let
        selected =
            model.tool /= Eraser && paintChar doc model == swatch.ch
    in
    button
        [ HA.classList
            [ ( "h-7 w-7 shrink-0 cursor-pointer rounded-sm border flex items-center justify-center", True )
            , ( "border-accent ring-2 ring-accent/60", selected )
            , ( "border-edge hover:border-ink-faint", not selected && not swatch.guessed )
            , ( "border-dashed border-ink-faint hover:border-ink", swatch.guessed && not selected )
            ]
        , HA.style "background-color" swatch.css
        , HA.title
            (if swatch.guessed then
                swatch.name ++ " — テーマに無い色。仮の色で描いています"

             else
                swatch.name
            )
        , HE.onClick (ColorChosen swatch.ch)
        ]
        (if swatch.guessed then
            [ span [ HA.class "px-guessed text-[10px] font-bold text-app/80 drop-shadow-[0_0_2px_rgb(255_255_255/0.9)]" ] [ text "?" ] ]

         else
            []
        )



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


{-| トラックパッドのピンチと ⌘/Ctrl + ホイールでズーム。ブラウザ自身の拡大を
止めるので preventDefault が要る(そのため onWheel でなく custom)。

素のホイールは decoder を fail させて素通しにする — 掴んでしまうと絵が
画面より大きい時に上下へスクロールできなくなる。
-}
wheelDecoder : D.Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
wheelDecoder =
    D.map2 Tuple.pair (D.field "ctrlKey" D.bool) (D.field "deltaY" D.float)
        |> D.andThen
            (\( ctrl, deltaY ) ->
                if ctrl then
                    D.succeed { message = ZoomPinched deltaY, stopPropagation = True, preventDefault = True }

                else
                    D.fail "素のホイールはスクロールへ渡す"
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

        ( "l", False, _ ) ->
            Just (ToolChosen Line)

        ( "r", False, _ ) ->
            Just (ToolChosen Rect)

        ( "o", False, _ ) ->
            Just (ToolChosen Ellipse)

        _ ->
            Nothing



-- アイコン(SVG 線画・20px)


icon : List (Svg.Svg Msg) -> Html Msg
icon =
    iconSized 20


iconSized : Int -> List (Svg.Svg Msg) -> Html Msg
iconSized px paths =
    Svg.svg
        [ SA.viewBox "0 0 24 24"
        , SA.width (String.fromInt px)
        , SA.height (String.fromInt px)
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "1.6"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        paths


{-| コマ送りの操作。塗り(fill)で描く — 線画だと 14px では潰れて読めない。 -}
iconPlay : Html Msg
iconPlay =
    iconSized 14 [ Svg.path [ SA.d "M7 4 L20 12 L7 20 Z", SA.fill "currentColor", SA.stroke "none" ] [] ]


iconPause : Html Msg
iconPause =
    iconSized 14
        [ Svg.rect [ SA.x "6", SA.y "4", SA.width "4", SA.height "16", SA.fill "currentColor", SA.stroke "none" ] []
        , Svg.rect [ SA.x "14", SA.y "4", SA.width "4", SA.height "16", SA.fill "currentColor", SA.stroke "none" ] []
        ]


iconStepBack : Html Msg
iconStepBack =
    iconSized 14
        [ Svg.path [ SA.d "M18 5 L8 12 L18 19 Z", SA.fill "currentColor", SA.stroke "none" ] []
        , Svg.rect [ SA.x "4", SA.y "5", SA.width "3", SA.height "14", SA.fill "currentColor", SA.stroke "none" ] []
        ]


iconStepForward : Html Msg
iconStepForward =
    iconSized 14
        [ Svg.path [ SA.d "M6 5 L16 12 L6 19 Z", SA.fill "currentColor", SA.stroke "none" ] []
        , Svg.rect [ SA.x "17", SA.y "5", SA.width "3", SA.height "14", SA.fill "currentColor", SA.stroke "none" ] []
        ]


iconLoop : Html Msg
iconLoop =
    iconSized 14
        [ Svg.path [ SA.d "M4 9h13a3 3 0 0 1 3 3v0a3 3 0 0 1-3 3H4" ] []
        , Svg.path [ SA.d "m7 6-3 3 3 3" ] []
        ]


iconLine : Html Msg
iconLine =
    icon [ Svg.path [ SA.d "M4 20 L20 4" ] [] ]


iconRect : Html Msg
iconRect =
    icon [ Svg.rect [ SA.x "4", SA.y "6", SA.width "16", SA.height "12", SA.rx "1" ] [] ]


iconEllipse : Html Msg
iconEllipse =
    icon [ Svg.ellipse [ SA.cx "12", SA.cy "12", SA.rx "8.5", SA.ry "6" ] [] ]


{-| 虫眼鏡。ステータス帯は高さ 8(32px)なので、道具の 20px より小さく描く。 -}
iconZoomIn : Html Msg
iconZoomIn =
    iconSized 15
        [ Svg.circle [ SA.cx "10.5", SA.cy "10.5", SA.r "7.5" ] []
        , Svg.path [ SA.d "M16 16l5.5 5.5" ] []
        , Svg.path [ SA.d "M10.5 7v7M7 10.5h7" ] []
        ]


iconZoomOut : Html Msg
iconZoomOut =
    iconSized 15
        [ Svg.circle [ SA.cx "10.5", SA.cy "10.5", SA.r "7.5" ] []
        , Svg.path [ SA.d "M16 16l5.5 5.5" ] []
        , Svg.path [ SA.d "M7 10.5h7" ] []
        ]


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
