module SketchPad exposing
    ( Camera(..)
    , Edge(..)
    , Entry
    , LayerSheet
    , Model
    , Msg(..)
    , Out(..)
    , Sketch
    , Snapshot
    , Tool(..)
    , activeRows
    , cameraLabel
    , decode
    , encode
    , flatRows
    , gotList
    , init
    , isOpen
    , layerRows
    , listDecoder
    , maxLayers
    , nextChar
    , presets
    , promptSection
    , resizeActive
    , saveFailed
    , saved
    , sketchPath
    , spliceInto
    , strokeActive
    , update
    , view
    , viewInline
    , viewWindow
    )

{-| 画面のラフ塗り。「大体ここ壁・ここに人」をセル目に雑に塗って、
AI への依頼文に文字グリッドとして自動で挟むための下書き部品。

元データはこの Model が持つ(ゲームの Doc ではないので親の編集直列に乗せない)。
塗りの道具(ペン・バケツ・消しゴム)は PixelEditor の純関数を借りる。
ラベル(何色が何を表すか)は人間が名前・色・ひとことで自由に増やせて、
1 文字コードは Studio が自動で割り振る(人間には見せない)。

絵は最大 3 枚のレイヤーに分けて塗る。最初は 1 枚で、増やすかどうかは人が決める。
塗る先は選んでいる 1 枚だけで、ほかのレイヤーは薄く透けて見える。

依頼文への差し込みは promptSection / spliceInto の純関数で行い、
描き直せば導出し直すだけで追随する(失効の管理を持たない)。

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import PixelEditor exposing (floodAt, paintAt)
import Set exposing (Set)
import Svg
import Svg.Attributes as SA



-- データ


{-| ラベル 1 つ。char は保存と依頼文でだけ表に出る内部コード。
desc は AI への言葉の補足(空なら省かれる)。
-}
type alias Entry =
    { char : Char
    , name : String
    , fill : String
    , desc : String
    }


{-| 道具。Eraser は「空き(.)で塗るペン」。
Line/Rect/Ellipse は押した所から離した所までの形をまとめて塗る。
-}
type Tool
    = Pen
    | Bucket
    | Eraser
    | Line
    | Rect
    | Ellipse


{-| カメラアングル。グリッドの見た目は変えず、AI への意図伝えにだけ使う
(編集面まで傾けると塗る操作が難しくなるだけで得が無い)。
NoAngle は視点を決めない絵(1 枚絵・アドベンチャーの背景・ドット絵)用。
-}
type Camera
    = TopDown
    | SideView
    | Quarter
    | Behind
    | FirstPerson
    | NoAngle


{-| リサイズのつまみ。左右の端=列、上下の端=行、角=両方。
左・上のつまみで広げたときはセルが左・上に増える(見えている絵は動かさない)。
-}
type Edge
    = RightEdge
    | BottomEdge
    | CornerEdge
    | LeftEdge
    | TopEdge
    | TopLeftEdge


{-| レイヤー 1 枚。hidden は縮めて見えなくなった塗りの退避。
名前は人が自由に打ち替えるので、これを鍵にして探さない。
-}
type alias LayerSheet =
    { name : String
    , rows : List String
    , hidden : List String
    , visible : Bool
    }


{-| 戻す 1 本ぶんの控え。 -}
type alias Snapshot =
    { size : { w : Int, h : Int }
    , legend : List Entry
    , layers : List LayerSheet
    , layerIndex : Int
    }


{-| ラフ 1 本の既存バージョン一覧(GET /sketch/list の 1 要素)。新しい順とは
限らない — 使うのは最大値だけなので並びは問わない。
-}
type alias Sketch =
    { name : String
    , versions : List Int
    }


{-| 保存の進み。Flying/Done は「今どこへ書いているか」を運ぶ —
保存直後は同じ場所を指し続け、絵を触ったら次のバージョンへ動く(sketchPath 参照)。
失敗はサーバの理由をそのまま出す。
-}
type SaveState
    = SaveIdle
    | SaveFlying { path : String, content : String }
    | SaveDone { path : String, content : String }
    | SaveFailed String


type alias Model =
    { open : Bool
    , preset : String
    , size : { w : Int, h : Int }
    , legend : List Entry
    , note : String

    -- 保存名(拡張子抜き)。空のまま保存したら "screen" に倒す
    , name : String
    , tool : Tool

    -- 選択中ラベルの文字(ペン・バケツが塗る色)
    , active : Char

    -- その場編集を開いているチップの文字
    , editing : Maybe Char

    -- 一筆(mousedown〜up)の途中か。親はこの間だけグローバル mouseup を購読する
    , stroke : Bool

    -- 形の道具(直線・矩形・楕円)のドラッグ中だけ Just。離すまで rows は触らない
    , shape : Maybe { start : ( Int, Int ), current : ( Int, Int ) }

    -- 1 セルの画面上の大きさ(px)。保存には含めない — 表示倍率は絵ではない
    , cellPx : Int

    -- ラフの再現度(1=雰囲気 〜 4=セル単位で忠実)。絵に対する意図なので
    -- カメラと同じく保存にも依頼文にも載せる
    , fidelity : Int

    -- 戻す用のスナップショット(一筆・1 リサイズで 1 本・直近 20 本まで)。
    -- 絵だけ戻すとサイズと食い違うし、legend を外すとラベル削除を戻した
    -- とき凡例の無い文字がグリッドに残るし、hidden を外すと左シフトの undo 後に
    -- 広げたとき退避の塗りが列ずれで復活する。layerIndex まで戻すのは、
    -- レイヤーを消した操作を戻したとき選び先が別のレイヤーへずれないため
    , undo : List Snapshot
    , save : SaveState
    , camera : Camera

    -- 名前ごとの既存バージョン一覧(GET /sketch/list)。次のバージョン番号を
    -- 決める材料 — 名前を打ち替えたときに他のラフの番号を使わないよう、
    -- 選んでいるラフだけでなく全部を持つ
    , sketches : List Sketch

    -- カメラの絵柄一覧を開いているか(普段はチップ 1 枚に畳む)
    , cameraOpen : Bool

    -- レイヤーを奥から手前の順に。ここが絵の唯一の元データ — 選んでいる 1 枚だけ
    -- 別の場所へ写すと、どちらが正しいかで必ず食い違う。
    -- 各 hidden(縮めて見えなくなった塗りの退避)は保存に含めない —
    -- 保存物 = 見えている絵、という約束を守るため
    , layers : List LayerSheet

    -- いま塗っているレイヤーの位置(layers の何番目か)。
    -- 範囲外を指しても絵は失わない(読みは空きに倒れ、塗りは捨てられる)
    , layerIndex : Int

    -- グリッドのつまみをドラッグ中だけ Just。親はこの間だけグローバル mousemove/up を購読する。
    -- 開始時の状態を丸ごと持つのは、寸法を毎回ここから計算し直すため —
    -- 差分シフトの積み重ねだと、左・上へ縮めすぎた瞬間に列が落ちて
    -- 同じドラッグ内で戻しても失われる(右・下と手触りが非対称になる)
    , resize :
        Maybe
            { edge : Edge
            , start : { x : Float, y : Float }
            , startSize : { w : Int, h : Int }
            , startLayers : List LayerSheet
            }
    }


{-| 親への注文。保存だけは HTTP(putFile)が要るので親に頼む。 -}
type Out
    = OutNone
    | OutSave { path : String, content : String }
    | OutToast String


type Msg
    = ToggleOpen
    | PresetPicked String
    | ToolPicked Tool
    | ChipPicked Char
    | ChipNameEdited String
    | ChipFillEdited String
    | ChipDescEdited String
    | ChipEditClosed
    | ChipAdded
    | ChipDeleted
    | CellDown ( Int, Int )
    | CellEntered ( Int, Int )
    | StrokeEnded
    | ResizeStarted Edge { x : Float, y : Float }
    | ResizeMoved { x : Float, y : Float }
    | ResizeEnded
    | CameraToggled
    | CameraPicked Camera
    | LayerPicked Int
    | LayerAdded
    | LayerDeleted
    | LayerRenamed Int String
    | LayerVisibilityToggled Int
    | LayerRaised
    | LayerLowered
    | UndoClicked
    | NoteEdited String
    | NameEdited String
    | CellSizeEdited String
    | FidelityEdited String
    | SaveClicked


{-| 空きセルの文字。ラベルには使わせない予約。 -}
emptyChar : Char
emptyChar =
    '.'


{-| これ以上小さいと絵にならない下限。 -}
minSize : { w : Int, h : Int }
minSize =
    { w = 4, h = 3 }


{-| グリッドの上限。これ以上は塗るのも読むのも大変で、ラフの粒度を超える。 -}
maxSize : { w : Int, h : Int }
maxSize =
    { w = 75, h = 75 }


{-| 1 セルの画面上の大きさ(px)の初期値。 -}
defaultCellPx : Int
defaultCellPx =
    20


{-| レイヤーの上限。これ以上は下敷きが濁って、どこに塗っているのか読めなくなる。 -}
maxLayers : Int
maxLayers =
    3


init : Model
init =
    fromPreset "map"


{-| プリセットの初期状態(塗りは全部空き)。知らない id は map に倒す。 -}
fromPreset : String -> Model
fromPreset presetId =
    let
        preset =
            presets
                |> List.filter (\p -> p.id == presetId)
                |> List.head
                |> Maybe.withDefault fallbackPreset
    in
    { open = False
    , preset = preset.id
    , size = preset.size
    , legend = preset.legend
    , note = ""
    , name = ""
    , tool = Pen
    , active = preset.legend |> List.head |> Maybe.map .char |> Maybe.withDefault emptyChar
    , editing = Nothing
    , stroke = False
    , shape = Nothing
    , cellPx = defaultCellPx
    , fidelity = 1
    , undo = []
    , save = SaveIdle
    , camera = preset.camera
    , cameraOpen = False
    , layers = [ blankLayer 1 preset.size ]
    , layerIndex = 0
    , resize = Nothing
    , sketches = []
    }


{-| 全部空きの絵 1 枚。 -}
blankRows : { w : Int, h : Int } -> List String
blankRows size =
    List.repeat size.h (String.repeat size.w (String.fromChar emptyChar))


{-| 何も塗っていないレイヤー 1 枚。 -}
blankLayer : Int -> { w : Int, h : Int } -> LayerSheet
blankLayer number size =
    { name = defaultLayerName number
    , rows = blankRows size
    , hidden = []
    , visible = True
    }


{-| 増やしたレイヤーに最初に付く名前。人がすぐ打ち替えられるので、
中身を当てにいかず番号だけにする。
-}
defaultLayerName : Int -> String
defaultLayerName number =
    "レイヤー" ++ String.fromInt number



-- プリセット


{-| 塗り始めの品揃え。あくまで初期値 — ラベルは後からいくらでも
変えられる・増やせるので、ここを変えても保存済みファイルは壊れない。
-}
presets : List { id : String, label : String, size : { w : Int, h : Int }, camera : Camera, legend : List Entry }
presets =
    [ { id = "map"
      , label = "マップ"
      , size = { w = 16, h = 10 }
      , camera = TopDown
      , legend =
            [ { char = 'W', name = "壁", fill = "#8a6d3b", desc = "" }
            , { char = 'F', name = "床", fill = "#d9cfb8", desc = "" }
            , { char = 'D', name = "扉", fill = "#c07a3a", desc = "" }
            , { char = 'E', name = "敵", fill = "#d64550", desc = "" }
            , { char = 'G', name = "ゴール", fill = "#58b368", desc = "" }
            , { char = 'K', name = "鍵", fill = "#e3c14d", desc = "" }
            , { char = 'P', name = "主人公", fill = "#3b82f6", desc = "" }
            ]
      }
    , { id = "ui"
      , label = "UI"
      , size = { w = 12, h = 8 }
      , camera = TopDown
      , legend =
            [ { char = 'H', name = "見出し", fill = "#7c5cd6", desc = "" }
            , { char = 'B', name = "ボタン", fill = "#4f6df5", desc = "" }
            , { char = 'T', name = "文字", fill = "#9aa3ad", desc = "" }
            , { char = 'I', name = "絵", fill = "#d9a406", desc = "" }
            , { char = 'R', name = "枠", fill = "#5b6b7a", desc = "" }
            ]
      }
    , { id = "scenery"
      , label = "風景"
      , size = { w = 12, h = 8 }
      , camera = SideView
      , legend =
            [ { char = 'S', name = "空", fill = "#7ec8e3", desc = "" }
            , { char = 'G', name = "地面", fill = "#8a6d3b", desc = "" }
            , { char = 'M', name = "山", fill = "#6b7f5e", desc = "" }
            , { char = 'T', name = "木", fill = "#3f6b3a", desc = "" }
            , { char = 'A', name = "水", fill = "#4f8fd6", desc = "" }
            ]
      }
    , { id = "dot"
      , label = "ドット絵"
      , size = { w = 16, h = 16 }
      , camera = NoAngle

      -- ラベルは「何があるか」でなく「どんな面か」。ドット絵で迷うのは
      -- 物の置き場所でなく、面の明るさと材質(そこが決まれば色は AI が選べる)
      , legend =
            [ { char = 'L', name = "輪郭", fill = "#241a24", desc = "外周の濃い線" }
            , { char = 'H', name = "ハイライト", fill = "#fff3c4", desc = "光が一番当たる面" }
            , { char = 'S', name = "影", fill = "#3b3550", desc = "光が当たらない面" }
            , { char = 'B', name = "肌", fill = "#e8b48a", desc = "" }
            , { char = 'C', name = "布", fill = "#c05a6a", desc = "やわらかい面" }
            , { char = 'W', name = "木", fill = "#8a6d3b", desc = "木目のある面" }
            , { char = 'R', name = "石", fill = "#7c7f88", desc = "ざらついた硬い面" }
            , { char = 'K', name = "金属", fill = "#9fb3c8", desc = "つやのある硬い面" }
            ]
      }
    ]


fallbackPreset : { id : String, label : String, size : { w : Int, h : Int }, camera : Camera, legend : List Entry }
fallbackPreset =
    { id = "map", label = "マップ", size = { w = 16, h = 10 }, camera = TopDown, legend = [] }


{-| カメラの日本語名(チップ・ボタン・依頼文で共用)。 -}
cameraLabel : Camera -> String
cameraLabel camera =
    case camera of
        TopDown ->
            "見下ろし"

        SideView ->
            "サイドビュー"

        Quarter ->
            "クォーター"

        Behind ->
            "背後から"

        FirstPerson ->
            "一人称"

        NoAngle ->
            "画角なし"


{-| 保存 JSON に書く英語 id(日本語をファイルに書かない)。 -}
cameraId : Camera -> String
cameraId camera =
    case camera of
        TopDown ->
            "top-down"

        SideView ->
            "side"

        Quarter ->
            "quarter"

        Behind ->
            "behind"

        FirstPerson ->
            "first-person"

        NoAngle ->
            "none"


cameraFromId : String -> Maybe Camera
cameraFromId id =
    case id of
        "top-down" ->
            Just TopDown

        "side" ->
            Just SideView

        "quarter" ->
            Just Quarter

        "behind" ->
            Just Behind

        "first-person" ->
            Just FirstPerson

        "none" ->
            Just NoAngle

        _ ->
            Nothing



-- レイヤー


{-| 番号で 1 枚を取り出す。範囲外なら空きの 1 枚 — 呼び側で
Maybe を開かずに済ませ、番号がずれても絵を消さない。
-}
sheetAt : Int -> Model -> LayerSheet
sheetAt index model =
    model.layers
        |> List.drop index
        |> List.head
        |> Maybe.withDefault (blankLayer 1 model.size)


{-| レイヤー 1 枚の見えている絵。 -}
layerRows : Int -> Model -> List String
layerRows index model =
    (sheetAt index model).rows


{-| いま塗っているレイヤーの絵。 -}
activeRows : Model -> List String
activeRows model =
    layerRows model.layerIndex model


{-| いま塗っているレイヤーだけを書き換える。 -}
mapActive : (LayerSheet -> LayerSheet) -> Model -> Model
mapActive f model =
    { model
        | layers =
            model.layers
                |> List.indexedMap
                    (\index sheet ->
                        if index == model.layerIndex then
                            f sheet

                        else
                            sheet
                    )
    }


{-| いま塗っているレイヤーの絵を差し替える。 -}
setActiveRows : List String -> Model -> Model
setActiveRows rows model =
    mapActive (\sheet -> { sheet | rows = rows }) model


{-| 表示にしているレイヤーだけを奥から手前の順に。 -}
shownLayers : Model -> List LayerSheet
shownLayers model =
    model.layers |> List.filter .visible


{-| 表示にしている全レイヤーを 1 枚に畳んだ絵(手前のレイヤーが勝つ)。 -}
flatRows : Model -> List String
flatRows model =
    shownLayers model
        |> List.map .rows
        |> List.foldl (mergeRows model.size) (blankRows model.size)


{-| 何か塗ってあるか。 -}
isPainted : LayerSheet -> Bool
isPainted sheet =
    sheet.rows |> String.concat |> String.any (\c -> c /= emptyChar)


{-| 依頼文とレイヤーパネルで数える「塗ったセルの数」。 -}
paintedCells : LayerSheet -> Int
paintedCells sheet =
    sheet.rows
        |> String.concat
        |> String.toList
        |> List.filter (\c -> c /= emptyChar)
        |> List.length


{-| 上の絵を下の絵へ重ねる(上が空きのセルだけ下が見える)。 -}
mergeRows : { w : Int, h : Int } -> List String -> List String -> List String
mergeRows size upper lower =
    List.map2
        (\upperRow lowerRow ->
            List.map2
                (\up down ->
                    if up == emptyChar then
                        down

                    else
                        up
                )
                (String.toList upperRow)
                (String.toList lowerRow)
                |> String.fromList
        )
        (fitRows size upper)
        (fitRows size lower)


{-| 依頼文と保存でレイヤーごとに書き分けるか。表示していて塗ってあるものが
2 枚以上あるときだけ — 1 枚しか中身が無いなら見出しは邪魔なだけになる。
-}
isLayered : Model -> Bool
isLayered model =
    (shownLayers model |> List.filter isPainted |> List.length) > 1



-- 純ロジック(コード割り振り・JSON・依頼文)


{-| 自由追加のラベルに割り振る 1 文字。使用中と空き(.)を避けて
A-Z0-9 の先頭から。枯れたら Nothing(追加を断る)。
-}
nextChar : List Char -> Maybe Char
nextChar used =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        |> String.toList
        |> List.filter (\c -> c /= emptyChar && not (List.member c used))
        |> List.head


{-| sketch.json の中身。desc が空のラベルは desc ごと省く。

rows はレイヤーを畳んだ 1 枚。レイヤーを知らない読み手(比較の窓・古い Studio)が
これだけで絵を出せるようにするため、レイヤーが何枚あっても必ず書く。
layers は 2 枚以上あるときだけ足す — 1 枚で描いたラフの保存物を、
レイヤーの入れ物の分だけ膨らませない。

-}
encode : Model -> String
encode model =
    E.encode 2
        (E.object
            (List.concat
                [ [ ( "version", E.int 1 )
                  , ( "kind", E.string "sketch" )
                  , ( "preset", E.string model.preset )
                  , ( "size", E.object [ ( "w", E.int model.size.w ), ( "h", E.int model.size.h ) ] )
                  , ( "camera", E.string (cameraId model.camera) )
                  , ( "fidelity", E.int model.fidelity )
                  , ( "legend", E.list encodeEntry model.legend )
                  , ( "rows", E.list E.string (flatRows model) )
                  ]
                , if List.length model.layers > 1 then
                    [ ( "layers", E.list encodeLayer model.layers ) ]

                  else
                    []
                , [ ( "note", E.string model.note ) ]
                ]
            )
        )


{-| レイヤー 1 枚。visible は隠したときだけ書く(既定は表示)。 -}
encodeLayer : LayerSheet -> E.Value
encodeLayer sheet =
    E.object
        (List.concat
            [ [ ( "name", E.string sheet.name )
              , ( "rows", E.list E.string sheet.rows )
              ]
            , if sheet.visible then
                []

              else
                [ ( "visible", E.bool False ) ]
            ]
        )


encodeEntry : Entry -> E.Value
encodeEntry entry =
    E.object
        (List.concat
            [ [ ( "char", E.string (String.fromChar entry.char) )
              , ( "name", E.string entry.name )
              , ( "fill", E.string entry.fill )
              ]
            , if String.isEmpty entry.desc then
                []

              else
                [ ( "desc", E.string entry.desc ) ]
            ]
        )


{-| sketch.json を読み戻す(encode と対)。壊れた文字列は Nothing。 -}
decode : String -> Maybe Model
decode text =
    D.decodeString sketchDecoder text
        |> Result.toMaybe


sketchDecoder : D.Decoder Model
sketchDecoder =
    D.map8
        (\preset size cameraText fidelity legend rows note layers ->
            let
                base =
                    fromPreset preset

                sheets =
                    if List.isEmpty layers then
                        -- レイヤーの欄が無いラフは、畳んだ絵をそのまま 1 枚として読む。
                        -- ここでレイヤーに分け直そうとすると、どのセルがどのレイヤーかは
                        -- 元の絵に書かれていないので当て推量になる
                        [ { name = defaultLayerName 1, rows = fitRows size rows, hidden = [], visible = True } ]

                    else
                        layers
                            |> List.take maxLayers
                            |> List.indexedMap
                                (\index sheet ->
                                    { sheet | rows = fitRows size sheet.rows, name = orName (index + 1) sheet.name }
                                )
            in
            { base
                | size = size

                -- カメラの欄が無い・知らない値でも読める(fail-open)。
                -- 読めない Doc で全体を止めないため、プリセットの初期カメラに倒す
                , camera = cameraFromId cameraText |> Maybe.withDefault base.camera

                -- 再現度も同じく fail-open。範囲外は 1(雰囲気)に倒す —
                -- 勝手に忠実側へ寄せると AI への注文が意図より強くなるため
                , fidelity =
                    if fidelity >= 1 && fidelity <= 4 then
                        fidelity

                    else
                        1
                , legend = legend
                , layers = sheets

                -- 開いた直後に選ぶのは一番手前(パネルの一番上)。
                -- イラストソフトと同じで、続きを描く場所として当たりやすい
                , layerIndex = List.length sheets - 1
                , note = note
                , active = legend |> List.head |> Maybe.map .char |> Maybe.withDefault emptyChar
            }
        )
        (D.field "preset" D.string)
        (D.field "size" (D.map2 (\w h -> { w = w, h = h }) (D.field "w" D.int) (D.field "h" D.int)))
        (D.oneOf [ D.field "camera" D.string, D.succeed "" ])
        (D.oneOf [ D.field "fidelity" D.int, D.succeed 1 ])
        (D.field "legend" (D.list entryDecoder))
        (D.field "rows" (D.list D.string))
        (D.field "note" D.string)
        (D.oneOf [ D.field "layers" (D.list layerSheetDecoder), D.succeed [] ])


{-| layers の 1 要素。名前の欄が無い形(id で奥・主役・手前を指していた頃のラフ)も
読めるようにしておく — 読めないと絵が 1 枚に潰れて、分けて描いた形が失われる。
-}
layerSheetDecoder : D.Decoder LayerSheet
layerSheetDecoder =
    D.map3
        (\name sheetRows visible ->
            { name = name, rows = sheetRows, hidden = [], visible = visible }
        )
        (D.oneOf
            [ D.field "name" D.string
            , D.field "id" D.string |> D.map nameFromLayerId
            , D.succeed ""
            ]
        )
        (D.field "rows" (D.list D.string))
        (D.oneOf [ D.field "visible" D.bool, D.succeed True ])


{-| 名前の欄が空なら番号の名前に倒す。 -}
orName : Int -> String -> String
orName number name =
    if String.isEmpty (String.trim name) then
        defaultLayerName number

    else
        name


{-| id で重なりを指していた頃の呼び名。知らない id は空(番号の名前に倒れる)。 -}
nameFromLayerId : String -> String
nameFromLayerId id =
    case id of
        "back" ->
            "奥"

        "main" ->
            "主役"

        "front" ->
            "手前"

        _ ->
            ""


{-| 絵 1 枚を寸法どおりの形に整える。レイヤーごとに丈がまちまちだと、
重ねたときや切り出したときに行がずれて別の絵になる。
-}
fitRows : { w : Int, h : Int } -> List String -> List String
fitRows size rows =
    List.range 0 (size.h - 1)
        |> List.map
            (\y ->
                rows
                    |> List.drop y
                    |> List.head
                    |> Maybe.withDefault ""
                    |> String.padRight size.w emptyChar
                    |> String.left size.w
            )


{-| GET /sketch/list の "sketches" を読む。ok や個々の要素の欠けた欄には
寛容(fail-open)— 読めない要素は落とすのでなく、丸ごと失敗させて古い
一覧のまま据え置く(Tickets.listDecoder と同じ流儀)。
-}
listDecoder : D.Decoder (List Sketch)
listDecoder =
    D.field "sketches" (D.list sketchListEntryDecoder)


sketchListEntryDecoder : D.Decoder Sketch
sketchListEntryDecoder =
    D.map2 Sketch
        (D.field "name" D.string)
        (D.oneOf [ D.field "versions" (D.list D.int), D.succeed [] ])


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map4 Entry
        (D.field "char" D.string
            |> D.andThen
                (\s ->
                    case String.uncons s of
                        Just ( c, "" ) ->
                            D.succeed c

                        _ ->
                            D.fail "char は 1 文字"
                )
        )
        (D.field "name" D.string)
        (D.field "fill" D.string)
        (D.oneOf [ D.field "desc" D.string, D.succeed "" ])


{-| 保存先の相対パス。保存した直後は同じ場所を指し続け(絵を触っていなければ
SaveDone の内容と今の絵が一致する)、絵を触ったら次のバージョンへ動く。
「原本: 」行(promptSection)がこの値を依頼文に書き残すので、保存直後は
存在するファイルを指し続けなければならない。

名前も一致を見るのは、encode が name を含まない(name は保存の宛先を
決めるだけで、絵の中身ではない)ため — 絵はそのままで名前だけ打ち替えると
content の一致だけでは古い名前の場所を指し続けてしまい、そこへ保存すると
別のラフを上書きしてしまう。
-}
sketchPath : Model -> String
sketchPath model =
    case model.save of
        SaveDone done ->
            if done.content == encode model && nameFromPath done.path == Just (safeName model.name) then
                done.path

            else
                nextSketchPath model

        _ ->
            nextSketchPath model


{-| 名前が空なら "screen"。区切り文字はサーバの門番より先に UI で捨てる
(エラーで戸惑わせない)。
-}
nextSketchPath : Model -> String
nextSketchPath model =
    let
        name =
            safeName model.name
    in
    "draft/sketch/" ++ name ++ "/v" ++ String.fromInt (nextVersion name model.sketches) ++ ".json"


{-| そのラフの既存バージョンの最大 + 1(無ければ 1)。 -}
nextVersion : String -> List Sketch -> Int
nextVersion name sketches =
    sketches
        |> List.filter (\s -> s.name == name)
        |> List.concatMap .versions
        |> List.maximum
        |> Maybe.map (\v -> v + 1)
        |> Maybe.withDefault 1


{-| "draft/sketch/<name>/vN.json" からバージョン番号だけを取り出す。
形が違えば Nothing(壊れたパスで文言や集計を巻き込まないため)。
-}
versionFromPath : String -> Maybe Int
versionFromPath path =
    path
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.andThen
            (\fileName ->
                if String.startsWith "v" fileName && String.endsWith ".json" fileName then
                    fileName
                        |> String.dropLeft 1
                        |> String.dropRight 5
                        |> String.toInt

                else
                    Nothing
            )


{-| "draft/sketch/<name>/vN.json" から名前だけを取り出す(vN.json の 1 つ上の階層)。
形が違えば Nothing。
-}
nameFromPath : String -> Maybe String
nameFromPath path =
    path
        |> String.split "/"
        |> List.reverse
        |> List.drop 1
        |> List.head


safeName : String -> String
safeName raw =
    let
        cleaned =
            raw
                |> String.trim
                |> String.filter (\c -> c /= '/' && c /= '\\')
    in
    if String.isEmpty cleaned then
        "screen"

    else
        cleaned


{-| 依頼文に挟む「## 画面のラフ」の一節。何も塗っておらず補足も空なら
Nothing(= 描かなければ依頼文は今まで通り)。
凡例は塗りに使われているラベルだけ載せる(使っていない色で AI を迷わせない)。

1 枚で描いたラフはセル目を 1 枚だけ書く。レイヤーに分けて描いたときは
レイヤーごとに 1 枚ずつ書く — 畳んで注記だけにすると、手前の物に隠れた奥の形が
文字グリッドから消えてしまう。

-}
promptSection : Model -> Maybe String
promptSection model =
    let
        usedChars =
            flatRows model |> String.concat |> String.toList

        usedLegend =
            model.legend |> List.filter (\entry -> List.member entry.char usedChars)

        painted =
            usedChars |> List.any (\c -> c /= emptyChar)
    in
    if not painted && String.isEmpty (String.trim model.note) then
        Nothing

    else
        let
            legendLine =
                (usedLegend |> List.map legendTerm)
                    ++ [ String.fromChar emptyChar ++ "=空き" ]
                    |> String.join "  "

            noteLines =
                case String.trim model.note of
                    "" ->
                        []

                    written ->
                        [ "補足: " ++ written ]

            gridLines =
                if isLayered model then
                    "レイヤー: あとに書いたものほど手前に重なります。レイヤーごとに 1 枚ずつ書きます。"
                        :: (shownLayers model
                                |> List.filter isPainted
                                |> List.concatMap (\sheet -> ("### " ++ sheet.name) :: sheet.rows)
                           )

                else
                    flatRows model
        in
        Just
            (String.join "\n"
                (List.concat
                    [ [ "## " ++ sketchTitle model ++ "（カメラ: " ++ cameraLabel model.camera ++ "、" ++ String.fromInt model.size.w ++ "x" ++ String.fromInt model.size.h ++ "、再現度: " ++ fidelityLabel model.fidelity ++ "、1文字=" ++ cellWord model ++ "。塗りから自動生成）"
                      , "凡例: " ++ legendLine
                      ]
                    , gridLines
                    , noteLines
                    , [ "凡例のかっこ内は意図です。" ++ fidelityClause model.fidelity
                      , "原本: " ++ sketchPath model
                      ]
                    ]
                )
            )


{-| 依頼文の見出し。ドット絵だけは「画面」でなく絵そのものの下描きなので
言い方を変える(画面のつもりで背景まで足されると別物になる)。
-}
sketchTitle : Model -> String
sketchTitle model =
    if model.preset == "dot" then
        "ドット絵のラフ"

    else
        "画面のラフ"


{-| 1 セルが何を表すか。 -}
cellWord : Model -> String
cellWord model =
    if model.preset == "dot" then
        "1ドット"

    else
        "1セル"


{-| 再現度の段階名(スライダーの表示と依頼文の見出しで共用)。 -}
fidelityLabel : Int -> String
fidelityLabel level =
    case level of
        2 ->
            "まあまあ再現"

        3 ->
            "そこそこ忠実再現"

        4 ->
            "忠実再現"

        _ ->
            "雰囲気再現"


{-| 再現度を AI への注文の言葉に直す(依頼文の末尾)。 -}
fidelityClause : Int -> String
fidelityClause level =
    case level of
        2 ->
            "大きな配置関係(何がどの辺にあるか)は保ってください。細部は変えて構いません。"

        3 ->
            "配置と大きさの比率をできるだけ保ってください。多少の調整は構いません。"

        4 ->
            "セル単位でできるだけ忠実に再現してください。"

        _ ->
            "ラフなのでセル単位の忠実さは不要です。雰囲気が伝われば十分です。"


legendTerm : Entry -> String
legendTerm entry =
    if String.isEmpty (String.trim entry.desc) then
        String.fromChar entry.char ++ "=" ++ entry.name

    else
        String.fromChar entry.char ++ "=" ++ entry.name ++ "（" ++ String.trim entry.desc ++ "）"


{-| 一節を依頼文へ差し込む。依頼文は必ず「【やること】」の行を持つ
(サーバの固定構造)ので、その直前 = 説明の直後に入れる。
見つからないときだけ末尾に足す(構造が変わっても依頼が壊れない安全網)。
-}
spliceInto : String -> String -> String
spliceInto section prompt =
    case String.indexes "【やること】" prompt of
        index :: _ ->
            String.left index prompt ++ section ++ "\n\n" ++ String.dropLeft index prompt

        [] ->
            prompt ++ "\n\n" ++ section



-- 更新


update : Msg -> Model -> ( Model, Out )
update msg model =
    case msg of
        ToggleOpen ->
            ( { model | open = not model.open }, OutNone )

        PresetPicked presetId ->
            -- 塗りは捨てる(プリセット替え=描き直し)が、補足と保存名は人の言葉なので残す
            let
                fresh =
                    fromPreset presetId
            in
            -- セルの大きさ(見た目の好み)と再現度(人の意図)も残す。
            -- sketches(既存バージョン一覧)もプリセット替えでは消えない —
            -- 一覧はプロジェクトに紐づく物で、道具の選び直しとは無関係
            ( { fresh
                | open = True
                , note = model.note
                , name = model.name
                , cellPx = model.cellPx
                , fidelity = model.fidelity
                , sketches = model.sketches
              }
            , OutNone
            )

        ToolPicked tool ->
            ( { model | tool = tool }, OutNone )

        ChipPicked char ->
            -- 2 度目のクリックでその場編集を開く(道具を増やさない)
            if model.active == char && model.editing == Nothing then
                ( { model | editing = Just char }, OutNone )

            else
                ( { model | active = char, editing = Nothing, tool = pickTool model.tool }, OutNone )

        ChipNameEdited name ->
            ( mapEditing (\entry -> { entry | name = name }) model, OutNone )

        ChipFillEdited fill ->
            ( mapEditing (\entry -> { entry | fill = fill }) model, OutNone )

        ChipDescEdited desc ->
            ( mapEditing (\entry -> { entry | desc = desc }) model, OutNone )

        ChipEditClosed ->
            ( { model | editing = Nothing }, OutNone )

        ChipAdded ->
            case nextChar (List.map .char model.legend) of
                Nothing ->
                    ( model, OutToast "ラベルはこれ以上増やせません" )

                Just char ->
                    let
                        entry =
                            { char = char
                            , name = "ラベル"
                            , fill = freshFill (List.length model.legend)
                            , desc = ""
                            }
                    in
                    ( { model
                        | legend = model.legend ++ [ entry ]
                        , active = char
                        , editing = Just char
                        , tool = pickTool model.tool
                      }
                    , OutNone
                    )

        ChipDeleted ->
            case model.editing of
                Nothing ->
                    ( model, OutNone )

                Just char ->
                    let
                        erase =
                            String.map
                                (\c ->
                                    if c == char then
                                        emptyChar

                                    else
                                        c
                                )

                        remaining =
                            model.legend |> List.filter (\entry -> entry.char /= char)
                    in
                    ( { model
                        | legend = remaining
                        , layers =
                            model.layers
                                |> List.map (\sheet -> { sheet | rows = List.map erase sheet.rows, hidden = List.map erase sheet.hidden })
                        , editing = Nothing
                        , active =
                            if model.active == char then
                                remaining |> List.head |> Maybe.map .char |> Maybe.withDefault emptyChar

                            else
                                model.active
                        , undo = snapshot model :: List.take 19 model.undo
                        , save = SaveIdle
                      }
                    , OutNone
                    )

        CellDown point ->
            if isShapeTool model.tool then
                -- rows は離すまで触らないが、undo はここで積む
                -- (確定直前 = down 時点の絵で正しく、確定時に積み直す手間が要らない)
                ( { model
                    | shape = Just { start = point, current = point }
                    , stroke = True
                    , undo = snapshot model :: List.take 19 model.undo
                    , save = SaveIdle
                  }
                , OutNone
                )

            else
                let
                    brush =
                        brushChar model

                    next =
                        case model.tool of
                            Bucket ->
                                floodAt brush point (activeRows model)

                            _ ->
                                paintAt brush point (activeRows model)
                in
                ( setActiveRows next
                    { model
                        | stroke = model.tool /= Bucket
                        , undo = snapshot model :: List.take 19 model.undo
                        , save = SaveIdle
                    }
                , OutNone
                )

        CellEntered point ->
            case model.shape of
                Just going ->
                    ( { model | shape = Just { going | current = point } }, OutNone )

                Nothing ->
                    if model.stroke then
                        ( setActiveRows (paintAt (brushChar model) point (activeRows model)) model, OutNone )

                    else
                        ( model, OutNone )

        StrokeEnded ->
            case model.shape of
                Just going ->
                    ( setActiveRows
                        (shapeCells model.tool going.start going.current
                            |> List.foldl (paintAt (brushChar model)) (activeRows model)
                        )
                        { model | shape = Nothing, stroke = False }
                    , OutNone
                    )

                Nothing ->
                    ( { model | stroke = False }, OutNone )

        ResizeStarted edge point ->
            ( { model
                | resize =
                    Just
                        { edge = edge
                        , start = point
                        , startSize = model.size
                        , startLayers = model.layers
                        }
              }
            , OutNone
            )

        ResizeMoved point ->
            case model.resize of
                Nothing ->
                    ( model, OutNone )

                Just going ->
                    let
                        snap from delta =
                            from + round (delta / toFloat model.cellPx)

                        anchor =
                            edgeAnchor going.edge

                        -- 左・上のつまみは外(マイナス方向)へ引くほど広がるので符号を返す
                        dx =
                            if anchor.left then
                                going.start.x - point.x

                            else
                                point.x - going.start.x

                        dy =
                            if anchor.top then
                                going.start.y - point.y

                            else
                                point.y - going.start.y

                        newSize =
                            { w =
                                if going.edge == BottomEdge || going.edge == TopEdge then
                                    going.startSize.w

                                else
                                    clamp minSize.w maxSize.w (snap going.startSize.w dx)
                            , h =
                                if going.edge == RightEdge || going.edge == LeftEdge then
                                    going.startSize.h

                                else
                                    clamp minSize.h maxSize.h (snap going.startSize.h dy)
                            }
                    in
                    if newSize == model.size then
                        ( model, OutNone )

                    else
                        -- 現在の姿でなくドラッグ開始時の姿から毎回計算し直す。
                        -- 差分シフトの積み重ねだと縮めすぎた列がその場で失われ、
                        -- 同じドラッグ内で戻しても復元できない
                        ( applySize anchor
                            newSize
                            { model | size = going.startSize, layers = going.startLayers }
                        , OutNone
                        )

        ResizeEnded ->
            case model.resize of
                Nothing ->
                    ( model, OutNone )

                Just going ->
                    if going.startSize == model.size then
                        ( { model | resize = Nothing }, OutNone )

                    else
                        ( { model
                            | resize = Nothing
                            , undo =
                                { size = going.startSize
                                , legend = model.legend
                                , layers = going.startLayers
                                , layerIndex = model.layerIndex
                                }
                                    :: List.take 19 model.undo
                            , save = SaveIdle
                          }
                        , OutNone
                        )

        CameraToggled ->
            ( { model | cameraOpen = not model.cameraOpen }, OutNone )

        CameraPicked camera ->
            ( { model | camera = camera, cameraOpen = False, save = SaveIdle }, OutNone )

        LayerPicked index ->
            -- 絵は 1 セルも変わらないので save には触らない
            -- (保存済みの表示が、塗る場所を選び直しただけで消えると戸惑う)
            ( { model | layerIndex = clamp 0 (List.length model.layers - 1) index, editing = Nothing }, OutNone )

        LayerAdded ->
            if List.length model.layers >= maxLayers then
                ( model, OutToast "レイヤーはこれ以上増やせません" )

            else
                -- 足したレイヤーは一番手前に置き、そのまま選ぶ —
                -- 増やす動機はたいてい「この上に描き足す」ため
                ( { model
                    | layers = model.layers ++ [ blankLayer (List.length model.layers + 1) model.size ]
                    , layerIndex = List.length model.layers
                    , undo = snapshot model :: List.take 19 model.undo
                    , save = SaveIdle
                  }
                , OutNone
                )

        LayerDeleted ->
            if List.length model.layers <= 1 then
                ( model, OutToast "最後の 1 枚は削除できません" )

            else
                let
                    remaining =
                        model.layers
                            |> List.indexedMap Tuple.pair
                            |> List.filter (\( index, _ ) -> index /= model.layerIndex)
                            |> List.map Tuple.second
                in
                ( { model
                    | layers = remaining
                    , layerIndex = clamp 0 (List.length remaining - 1) model.layerIndex
                    , undo = snapshot model :: List.take 19 model.undo
                    , save = SaveIdle
                  }
                , OutNone
                )

        LayerRenamed index name ->
            ( { model
                | layers =
                    model.layers
                        |> List.indexedMap
                            (\at sheet ->
                                if at == index then
                                    { sheet | name = name }

                                else
                                    sheet
                            )
                , save = SaveIdle
              }
            , OutNone
            )

        LayerVisibilityToggled index ->
            ( { model
                | layers =
                    model.layers
                        |> List.indexedMap
                            (\at sheet ->
                                if at == index then
                                    { sheet | visible = not sheet.visible }

                                else
                                    sheet
                            )
                , save = SaveIdle
              }
            , OutNone
            )

        LayerRaised ->
            ( swapLayers model.layerIndex (model.layerIndex + 1) model, OutNone )

        LayerLowered ->
            ( swapLayers model.layerIndex (model.layerIndex - 1) model, OutNone )

        UndoClicked ->
            case model.undo of
                previous :: rest ->
                    ( { model
                        | size = previous.size
                        , legend = previous.legend
                        , layers = previous.layers
                        , layerIndex = previous.layerIndex
                        , undo = rest
                        , save = SaveIdle
                      }
                    , OutNone
                    )

                [] ->
                    ( model, OutNone )

        NoteEdited note ->
            ( { model | note = note, save = SaveIdle }, OutNone )

        NameEdited name ->
            ( { model | name = name, save = SaveIdle }, OutNone )

        CellSizeEdited raw ->
            case String.toInt raw of
                Just px ->
                    ( { model | cellPx = clamp 10 32 px }, OutNone )

                Nothing ->
                    ( model, OutNone )

        FidelityEdited raw ->
            case String.toInt raw of
                Just level ->
                    ( { model | fidelity = clamp 1 4 level, save = SaveIdle }, OutNone )

                Nothing ->
                    ( model, OutNone )

        SaveClicked ->
            -- path と content はここで 1 回だけ作り、飛ばす物(OutSave)と
            -- 控える物(SaveFlying)を必ず同じにする — 後から作り直すと
            -- 応答が届く頃には絵が動いていて食い違う恐れがある
            let
                flying =
                    { path = sketchPath model, content = encode model }
            in
            ( { model | save = SaveFlying flying }, OutSave flying )


{-| 2 枚の重なり順を入れ替え、選んだままにする。行き先が範囲外なら何もしない
(端で押しても選び先だけ飛ぶ、を防ぐ)。
-}
swapLayers : Int -> Int -> Model -> Model
swapLayers from to model =
    if to < 0 || to >= List.length model.layers || from == to then
        model

    else
        { model
            | layers =
                model.layers
                    |> List.indexedMap
                        (\index sheet ->
                            if index == from then
                                sheetAt to model

                            else if index == to then
                                sheetAt from model

                            else
                                sheet
                        )
            , layerIndex = to
            , undo = snapshot model :: List.take 19 model.undo
            , save = SaveIdle
        }


{-| つまみが左・上を動かすか(動かすなら絵ごと座標をずらす)。 -}
edgeAnchor : Edge -> { left : Bool, top : Bool }
edgeAnchor edge =
    { left = edge == LeftEdge || edge == TopLeftEdge
    , top = edge == TopEdge || edge == TopLeftEdge
    }


{-| グリッドの寸法替え。縮めて消えた塗りは hidden に取っておき、広げたら戻す。
重なる範囲は rows が勝つ — 見えている所で消した「.」は人の意思なので、
hidden の古い塗りで勝手に復活させない。

左・上アンカーのときは全描画履歴(merged)ごと列・行をずらしてから切る。
左・上へ縮めて merged から落ちた列・行は復元不能でよい(undo で戻れる —
逆向きの退避まで持つと hidden の意味が「どの向きに落ちたか」まで抱えて重くなる)。

-}
applySize : { left : Bool, top : Bool } -> { w : Int, h : Int } -> Model -> Model
applySize anchor newSize model =
    { model
        | size = newSize

        -- 全レイヤーを同じだけ切る・伸ばす。1 枚でも飛ばすと丈が食い違い、
        -- 重ねたときに行がずれる
        , layers = model.layers |> List.map (resizeSheet anchor model.size newSize)
    }


{-| 絵 1 枚の寸法替え。 -}
resizeSheet : { left : Bool, top : Bool } -> { w : Int, h : Int } -> { w : Int, h : Int } -> LayerSheet -> LayerSheet
resizeSheet anchor oldSize newSize sheet =
    let
        fullW =
            max oldSize.w
                (sheet.hidden |> List.map String.length |> List.maximum |> Maybe.withDefault 0)

        fullH =
            max oldSize.h (List.length sheet.hidden)

        pad width row =
            String.padRight width emptyChar row

        rowAt y rows =
            rows |> List.drop y |> List.head

        merged =
            List.range 0 (fullH - 1)
                |> List.map
                    (\y ->
                        let
                            behind =
                                pad fullW (rowAt y sheet.hidden |> Maybe.withDefault "")
                        in
                        case rowAt y sheet.rows of
                            Just visible ->
                                pad oldSize.w visible
                                    |> String.left oldSize.w
                                    |> (\front -> front ++ String.dropLeft oldSize.w behind)

                            Nothing ->
                                behind
                    )

        dW =
            newSize.w - oldSize.w

        dH =
            newSize.h - oldSize.h

        shiftRow row =
            if not anchor.left then
                row

            else if dW > 0 then
                String.repeat dW (String.fromChar emptyChar) ++ row

            else
                String.dropLeft (negate dW) row

        shiftedW =
            if anchor.left then
                fullW + dW

            else
                fullW

        shifted =
            let
                columnsDone =
                    List.map shiftRow merged
            in
            if not anchor.top then
                columnsDone

            else if dH > 0 then
                List.repeat dH (String.repeat (max shiftedW newSize.w) (String.fromChar emptyChar)) ++ columnsDone

            else
                List.drop (negate dH) columnsDone

        cut =
            List.range 0 (newSize.h - 1)
                |> List.map
                    (\y ->
                        rowAt y shifted
                            |> Maybe.withDefault ""
                            |> pad newSize.w
                            |> String.left newSize.w
                    )
    in
    { sheet | rows = cut, hidden = shifted }


{-| undo 1 本ぶんの控え。 -}
snapshot : Model -> Snapshot
snapshot model =
    { size = model.size
    , legend = model.legend
    , layers = model.layers
    , layerIndex = model.layerIndex
    }


{-| 押してから離すまでで形を決める道具か。 -}
isShapeTool : Tool -> Bool
isShapeTool tool =
    tool == Line || tool == Rect || tool == Ellipse


{-| 形の道具が確定・プレビューで塗るセルの一覧。ワイルドカードを使わず
全部並べるのは、Tool を増やしたときコンパイラに漏れを教えてもらうため。
形の道具以外は shape が立たないのでここへは来ないが、来ても直線で害がない。
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


{-| 2 点を結ぶ直線のセル。長い方の軸の歩数で線形補間する。 -}
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


{-| ラベルを選んだら消しゴムからはペンに戻す(選んだ色で塗れない、を防ぐ)。 -}
pickTool : Tool -> Tool
pickTool tool =
    if tool == Eraser then
        Pen

    else
        tool


{-| いま塗る文字。消しゴムは空き。 -}
brushChar : Model -> Char
brushChar model =
    if model.tool == Eraser then
        emptyChar

    else
        model.active


mapEditing : (Entry -> Entry) -> Model -> Model
mapEditing f model =
    case model.editing of
        Nothing ->
            model

        Just char ->
            { model
                | legend =
                    model.legend
                        |> List.map
                            (\entry ->
                                if entry.char == char then
                                    f entry

                                else
                                    entry
                            )
                , save = SaveIdle
            }


{-| 自由追加ラベルの初期色。金角(goldenHue と同じ考え方)で回して隣同士を離す。 -}
freshFill : Int -> String
freshFill index =
    "hsl(" ++ String.fromInt (modBy 360 (index * 137)) ++ " 70% 55%)"


{-| 一筆の途中か(親がグローバル mouseup を購読する間だけ True)。 -}
strokeActive : Model -> Bool
strokeActive model =
    model.stroke


{-| グリッドのつまみをドラッグ中か(親がグローバル mousemove/up を購読する間だけ True)。 -}
resizeActive : Model -> Bool
resizeActive model =
    model.resize /= Nothing


{-| 保存成功。飛ばした物をそのまま「保存済み」へ移し、そのバージョン番号を
sketches へ足す — 一覧を取り直す前に続けて 2 回保存しても、2 回目が
1 回目のバージョンを上書きしない(sketchPath が sketches の最大値から
次を決めるため)。

足す先の名前は flying.path から取り出す(model.name ではない) —
飛行中に名前を打ち替えられていたら、model.name はもう違う名前を指しているため。
-}
saved : Model -> Model
saved model =
    case model.save of
        SaveFlying flying ->
            { model
                | save = SaveDone flying
                , sketches = withVersion flying.path model.sketches
            }

        _ ->
            model


{-| 保存できた場所のバージョン番号を、そのラフの一覧へ足す(重複は足さない)。
名前・バージョンともパスから取り出す(呼び出し側の model.name には頼らない)。
-}
withVersion : String -> List Sketch -> List Sketch
withVersion path sketches =
    case ( nameFromPath path, versionFromPath path ) of
        ( Just name, Just v ) ->
            if List.any (\s -> s.name == name) sketches then
                sketches
                    |> List.map
                        (\s ->
                            if s.name == name && not (List.member v s.versions) then
                                { s | versions = v :: s.versions }

                            else
                                s
                        )

            else
                sketches ++ [ { name = name, versions = [ v ] } ]

        _ ->
            sketches


{-| 保存失敗。理由は保存ボタンの脇に出す。 -}
saveFailed : String -> Model -> Model
saveFailed reason model =
    { model | save = SaveFailed reason }


{-| GET /sketch/list の中身を流し込む(Atelier だけが呼ぶ — NewGame は
これから産まれるプロジェクトなので必ず v1 から)。
-}
gotList : List Sketch -> Model -> Model
gotList sketches model =
    { model | sketches = sketches }



-- 表示


{-| 窓が出ているか(呼び側が Esc の受け口を張るかの判断に使う)。 -}
isOpen : Model -> Bool
isOpen model =
    model.open


view : Model -> Html Msg
view model =
    div [ HA.class "sketch-pad mb-4 rounded-lg border border-edge bg-panel" ]
        (viewHeader model
            :: (if model.open then
                    [ viewWindow
                        { title = "🎨 画面のラフを描く"
                        , hint = "閉じても塗った絵は残ります"
                        , onClose = Just ToggleOpen
                        }
                        (inlineParts model ++ [ viewSaveRow model ])
                    ]

                else
                    []
               )
        )


viewHeader : Model -> Html Msg
viewHeader model =
    button
        [ HA.class "sketch-toggle flex w-full cursor-pointer items-center gap-2 p-3 text-left text-xs font-semibold text-ink"
        , HE.onClick ToggleOpen
        ]
        [ span [] [ text "🎨 画面のラフを描く" ]
        , span [ HA.class "text-[10px] font-normal text-ink-faint" ]
            [ text "任意 — 「大体ここに何がある」を塗って AI に伝える" ]
        ]


{-| 塗り場を浮かせる窓。中身の大きさで決まる幅(画面いっぱいには広げない)で、
はみ出す分だけ窓の中が動く。

オーバーレイを押しても閉じない — グリッドの外で指を離す操作が多い塗り場では、
1 回の押し間違いで塗った絵ごと引っ込む事故になるため。

onClose が Nothing の間は閉じられない(閉じると戻れなくなる仕事を
窓の中で走らせている、と呼び側が判断したとき)。

-}
viewWindow : { title : String, hint : String, onClose : Maybe msg } -> List (Html msg) -> Html msg
viewWindow window body =
    div [ HA.class "sketch-window-layer fixed inset-0 z-[60] flex items-center justify-center bg-black/70 p-4" ]
        [ div [ HA.class "sketch-window flex max-h-[92vh] max-w-[min(96vw,64rem)] flex-col rounded-lg border border-edge bg-panel shadow-2xl" ]
            [ div [ HA.class "flex items-center gap-2 border-b border-edge px-3 py-2" ]
                [ span [ HA.class "text-xs font-semibold text-ink" ] [ text window.title ]
                , span [ HA.class "text-[10px] text-ink-faint" ] [ text window.hint ]
                , span [ HA.class "flex-1" ] []
                , case window.onClose of
                    Just close ->
                        button
                            [ HA.class "sketch-window-close btn btn-mini"
                            , HA.title "閉じる(Esc)"
                            , HE.onClick close
                            ]
                            [ text "✕ 閉じる" ]

                    Nothing ->
                        button [ HA.class "sketch-window-close btn btn-mini", HA.disabled True ]
                            [ text "✕ 閉じる" ]
                ]
            , div [ HA.class "min-h-0 flex-1 overflow-auto p-3" ] body
            ]
        ]


{-| 保存行を持たない埋め込み用の姿。保存先(プロジェクト)がまだ無い
場所(新しいゲームのラフのカード)に置くための形 — 保存は親の仕事になる。
-}
viewInline : Model -> Html Msg
viewInline model =
    div [] (inlineParts model)


inlineParts : Model -> List (Html Msg)
inlineParts model =
    List.concat
        [ [ viewPresets model
          , viewCamera model
          , viewChips model
          ]
        , viewChipEditor model
        , [ viewTools model
          , div [ HA.class "flex flex-wrap items-start gap-2" ]
                [ viewGridBlock model
                , viewLayerPanel model
                ]
          , viewNote model
          ]
        ]


viewPresets : Model -> Html Msg
viewPresets model =
    div [ HA.class "mb-2 flex items-center gap-1.5" ]
        (span [ HA.class "text-[10px] text-ink-faint" ] [ text "はじめの品揃え:" ]
            :: List.map
                (\preset ->
                    button
                        [ HA.class
                            (if model.preset == preset.id then
                                "sketch-preset cursor-pointer rounded-full border border-accent bg-accent/10 px-2.5 py-0.5 text-[11px] text-accent"

                             else
                                "sketch-preset cursor-pointer rounded-full border border-edge px-2.5 py-0.5 text-[11px] text-ink-soft hover:border-ink-faint"
                            )
                        , HE.onClick (PresetPicked preset.id)
                        ]
                        [ text preset.label ]
                )
                presets
        )


{-| ラベルの帯。チップ 1 回目のクリック=選ぶ、2 回目=その場編集。 -}
viewChips : Model -> Html Msg
viewChips model =
    div [ HA.class "mb-2 flex flex-wrap items-center gap-1.5" ]
        ((model.legend |> List.map (viewChip model))
            ++ [ button
                    [ HA.class "sketch-chip-add cursor-pointer rounded-full border border-dashed border-edge px-2.5 py-0.5 text-[11px] text-ink-faint hover:border-ink-faint hover:text-ink-soft"
                    , HA.disabled (nextChar (List.map .char model.legend) == Nothing)
                    , HE.onClick ChipAdded
                    ]
                    [ text "＋" ]
               ]
        )


viewChip : Model -> Entry -> Html Msg
viewChip model entry =
    button
        [ HA.class
            (if model.active == entry.char then
                "sketch-chip flex cursor-pointer items-center gap-1.5 rounded-full border border-accent bg-accent/10 px-2.5 py-0.5 text-[11px] text-ink"

             else
                "sketch-chip flex cursor-pointer items-center gap-1.5 rounded-full border border-edge px-2.5 py-0.5 text-[11px] text-ink-soft hover:border-ink-faint"
            )
        , HA.title
            (if String.isEmpty entry.desc then
                "もう一度押すと名前と色を直せます"

             else
                entry.desc
            )
        , HE.onClick (ChipPicked entry.char)
        ]
        [ span
            [ HA.class "h-2.5 w-2.5 shrink-0 rounded-full border border-black/20"
            , HA.style "background-color" entry.fill
            ]
            []
        , text entry.name
        ]


{-| その場編集の列。開いているチップの名前・色・ひとことを直す。 -}
viewChipEditor : Model -> List (Html Msg)
viewChipEditor model =
    case model.editing |> Maybe.andThen (\c -> model.legend |> List.filter (\entry -> entry.char == c) |> List.head) of
        Nothing ->
            []

        Just entry ->
            [ div [ HA.class "sketch-chip-editor mb-2 flex flex-wrap items-center gap-1.5 rounded border border-edge bg-black/20 p-2" ]
                [ Html.input
                    [ HA.class "field h-7 w-24 text-xs"
                    , HA.value entry.name
                    , HA.placeholder "名前"
                    , HE.onInput ChipNameEdited
                    ]
                    []
                , Html.input
                    [ HA.type_ "color"
                    , HA.class "h-7 w-9 cursor-pointer rounded border border-edge bg-transparent"
                    , HA.value entry.fill
                    , HE.onInput ChipFillEdited
                    ]
                    []
                , Html.input
                    [ HA.class "field h-7 min-w-40 flex-1 text-xs"
                    , HA.value entry.desc
                    , HA.placeholder "ひとこと(AI への補足。例: 崩れかけた石壁)"
                    , HE.onInput ChipDescEdited
                    ]
                    []
                , button
                    [ HA.class "sketch-chip-delete btn h-7 px-2 text-[11px]"
                    , HA.title "このラベルと、塗ったセルをすべて消します"
                    , HE.onClick ChipDeleted
                    ]
                    [ text "削除" ]
                , button [ HA.class "btn h-7 px-2 text-[11px]", HE.onClick ChipEditClosed ] [ text "閉じる" ]
                ]
            ]


viewTools : Model -> Html Msg
viewTools model =
    let
        toolButton tool title =
            button
                [ HA.class
                    (if model.tool == tool then
                        "sketch-tool flex h-7 w-7 cursor-pointer items-center justify-center rounded border border-accent bg-accent/10 text-accent"

                     else
                        "sketch-tool flex h-7 w-7 cursor-pointer items-center justify-center rounded border border-edge text-ink-soft hover:border-ink-faint"
                    )
                , HA.title title
                , HA.attribute "aria-label" title
                , HE.onClick (ToolPicked tool)
                ]
                [ toolGlyph tool ]
    in
    div [ HA.class "mb-2 flex flex-wrap items-center gap-1.5" ]
        [ toolButton Pen "ペン — 選んだラベルで 1 セルずつ塗る"
        , toolButton Bucket "バケツ — 同じ色の続きをまとめて塗る"
        , toolButton Eraser "消しゴム — 空きに戻す"
        , toolButton Line "直線 — 押した所から離した所まで直線を引く"
        , toolButton Rect "矩形 — 2 点を対角とする四角の枠を描く"
        , toolButton Ellipse "楕円 — 2 点を対角とするだ円の枠を描く"
        , button
            [ HA.class "sketch-undo flex h-7 w-7 cursor-pointer items-center justify-center rounded border border-edge text-ink-soft hover:border-ink-faint"
            , HA.title "戻す — ひとつ前の状態に戻る"
            , HA.attribute "aria-label" "戻す — ひとつ前の状態に戻る"
            , HA.disabled (List.isEmpty model.undo)
            , HE.onClick UndoClicked
            ]
            [ undoGlyph ]
        ]


{-| 道具のアイコン。絵文字でなく SVG なのは、文字色(選択中の accent)に
そのまま追随させ、どの OS でも同じ見た目にするため。
-}
toolGlyph : Tool -> Html msg
toolGlyph tool =
    let
        stroked shapes =
            Svg.svg
                [ SA.viewBox "0 0 16 16"
                , SA.width "16"
                , SA.height "16"
                , SA.fill "none"
                , SA.stroke "currentColor"
                , SA.strokeWidth "1.5"
                , SA.strokeLinecap "round"
                , SA.strokeLinejoin "round"
                ]
                shapes

        -- バケツと消しゴムだけ 24 基準。定番の形(lucide)のパスをそのまま使うためで、
        -- 16 に手で縮めて描き直すと曲線の丸みが崩れる。線幅 2/24 ≒ 1.5/16 で太さは揃う
        stroked24 shapes =
            Svg.svg
                [ SA.viewBox "0 0 24 24"
                , SA.width "16"
                , SA.height "16"
                , SA.fill "none"
                , SA.stroke "currentColor"
                , SA.strokeWidth "2"
                , SA.strokeLinecap "round"
                , SA.strokeLinejoin "round"
                ]
                shapes
    in
    case tool of
        Pen ->
            stroked
                [ Svg.path [ SA.d "M4.5 11.5 L11.5 4.5 L13 6 L6 13 Z" ] []
                , Svg.path [ SA.d "M4.5 11.5 L3.5 14 L6 13 Z", SA.fill "currentColor" ] []
                ]

        Bucket ->
            stroked24
                [ Svg.path [ SA.d "m19 11-8-8-8.6 8.6a2 2 0 0 0 0 2.8l5.2 5.2c.8.8 2 .8 2.8 0L19 11Z" ] []
                , Svg.path [ SA.d "m5 2 5 5" ] []
                , Svg.path [ SA.d "M2 13h15" ] []
                , Svg.path [ SA.d "M22 20a2 2 0 1 1-4 0c0-1.6 1.7-2.4 2-4 .3 1.6 2 2.4 2 4Z", SA.fill "currentColor", SA.stroke "none" ] []
                ]

        Eraser ->
            stroked24
                [ Svg.path [ SA.d "m7 21-4.3-4.3c-1-1-1-2.5 0-3.4l9.6-9.6c1-1 2.5-1 3.4 0l5.6 5.6c1 1 1 2.5 0 3.4L13 21" ] []
                , Svg.path [ SA.d "M22 21H7" ] []
                , Svg.path [ SA.d "m5 11 9 9" ] []
                ]

        Line ->
            stroked
                [ Svg.line [ SA.x1 "3", SA.y1 "13", SA.x2 "13", SA.y2 "3" ] [] ]

        Rect ->
            stroked
                [ Svg.rect [ SA.x "3", SA.y "4", SA.width "10", SA.height "8" ] [] ]

        Ellipse ->
            stroked
                [ Svg.ellipse [ SA.cx "8", SA.cy "8", SA.rx "5.5", SA.ry "4" ] [] ]


undoGlyph : Html msg
undoGlyph =
    Svg.svg
        [ SA.viewBox "0 0 16 16"
        , SA.width "16"
        , SA.height "16"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "1.5"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        [ Svg.path [ SA.d "M4.5 6 H10 A3.5 3.5 0 0 1 10 13 H6" ] []
        , Svg.path [ SA.d "M7 3 L4 6 L7 9" ] []
        ]


{-| カメラ選び。普段はチップ 1 枚に畳む — 一覧を常に開くと
塗り場より先に絵柄ボタンが場所を取ってしまうため。
-}
viewCamera : Model -> Html Msg
viewCamera model =
    div [ HA.class "mb-2" ]
        (button
            [ HA.class "sketch-camera-toggle cursor-pointer rounded-full border border-edge px-2.5 py-0.5 text-[11px] text-ink-soft hover:border-ink-faint"
            , HE.onClick CameraToggled
            ]
            [ text "カメラ: "
            , span [ HA.class "text-accent" ] [ text (cameraLabel model.camera) ]
            , text
                (if model.cameraOpen then
                    " ▴"

                 else
                    " ▾"
                )
            ]
            :: (if model.cameraOpen then
                    [ div [ HA.class "sketch-camera-list mt-1.5 flex flex-wrap gap-1.5 rounded border border-edge bg-black/20 p-2" ]
                        ([ TopDown, SideView, Quarter, Behind, FirstPerson, NoAngle ]
                            |> List.map (viewCameraOption model.camera)
                        )
                    ]

                else
                    []
               )
        )


viewCameraOption : Camera -> Camera -> Html Msg
viewCameraOption chosen camera =
    button
        [ HA.class
            (if chosen == camera then
                "sketch-camera-option flex cursor-pointer flex-col items-center gap-0.5 rounded border border-accent bg-accent/10 px-2 py-1.5"

             else
                "sketch-camera-option flex cursor-pointer flex-col items-center gap-0.5 rounded border border-edge px-2 py-1.5 hover:border-ink-faint"
            )
        , HE.onClick (CameraPicked camera)
        ]
        [ cameraGlyph camera
        , span [ HA.class "text-[11px] text-ink" ] [ text (cameraLabel camera) ]
        , span [ HA.class "text-[9px] text-ink-faint" ] [ text (cameraHint camera) ]
        ]


cameraHint : Camera -> String
cameraHint camera =
    case camera of
        TopDown ->
            "真上から"

        SideView ->
            "真横から"

        Quarter ->
            "ななめ上から"

        Behind ->
            "主人公の後ろ"

        FirstPerson ->
            "主人公の目"

        NoAngle ->
            "1 枚絵・ADV"


{-| カメラの絵柄。言葉だけだと「クォーター」等が伝わらないので、
傾けたミニグリッド等の図で見せる(画像を持たないのは、色テーマに
CSS だけで追随させるため)。
-}
cameraGlyph : Camera -> Html msg
cameraGlyph camera =
    div [ HA.class "flex h-9 w-11 items-center justify-center" ]
        [ case camera of
            TopDown ->
                miniGrid []

            SideView ->
                div [ HA.class "relative h-6 w-8" ]
                    [ div [ HA.class "absolute bottom-0 left-0 right-0 h-2 bg-ok/60" ] []
                    , div [ HA.class "absolute bottom-2 left-1 h-2.5 w-1.5 bg-warn" ] []
                    ]

            Quarter ->
                miniGrid [ HA.style "transform" "rotateX(56deg) rotateZ(45deg)" ]

            Behind ->
                div [ HA.class "relative flex items-center justify-center" ]
                    [ miniGrid [ HA.style "transform" "rotateX(55deg)" ]
                    , div [ HA.class "absolute bottom-0 left-1/2 -ml-0.5 h-2 w-1.5 bg-warn" ] []
                    ]

            FirstPerson ->
                div [ HA.class "relative h-6 w-8 overflow-hidden rounded-[2px] border border-ink-soft/40" ]
                    [ div [ HA.class "absolute bottom-0 left-0 right-0 h-3 bg-ok/25" ] []
                    , div [ HA.class "absolute left-0 right-0 top-3 h-px bg-ink-soft/40" ] []
                    , div [ HA.class "absolute inset-0 flex items-center justify-center text-[10px] text-warn" ] [ text "+" ]
                    ]

            NoAngle ->
                div [ HA.class "relative h-6 w-8 rounded-[2px] border border-ink-soft/40" ]
                    [ div [ HA.class "absolute bottom-1 left-1 h-2 w-2 rounded-[1px] bg-warn/70" ] []
                    , div [ HA.class "absolute bottom-1 right-1 h-3 w-2.5 rounded-[1px] bg-ok/50" ] []
                    ]
        ]


miniGrid : List (Html.Attribute msg) -> Html msg
miniGrid attrs =
    div (HA.class "grid grid-cols-3 gap-px" :: attrs)
        (List.repeat 9 (div [ HA.class "h-1.5 w-1.5 rounded-[1px] bg-ink-soft/50" ] []))


{-| レイヤーパネル。一番上の行が一番手前 — イラストソフトと同じ並びにして、
どちらが前かを覚え直さずに済ませる(model.layers は奥から手前なので裏返す)。
-}
viewLayerPanel : Model -> Html Msg
viewLayerPanel model =
    let
        count =
            List.length model.layers
    in
    div [ HA.class "sketch-layers w-48 shrink-0 rounded border border-edge bg-black/20 p-1.5" ]
        [ div [ HA.class "mb-1 flex items-center justify-between text-[10px] text-ink-faint" ]
            [ span [] [ text "レイヤー" ]
            , span [] [ text (String.fromInt count ++ " / " ++ String.fromInt maxLayers) ]
            ]
        , div [ HA.class "flex flex-col gap-0.5" ]
            (model.layers
                |> List.indexedMap (viewLayerRow model)
                |> List.reverse
            )
        , div [ HA.class "mt-1.5 flex items-center gap-1" ]
            [ button
                [ HA.class "sketch-layer-add btn btn-mini"
                , HA.title "レイヤーを 1 枚足す(一番手前に置きます)"
                , HA.disabled (count >= maxLayers)
                , HE.onClick LayerAdded
                ]
                [ text "＋ 追加" ]
            , button
                [ HA.class "sketch-layer-delete btn btn-ghost btn-mini"
                , HA.title "選んでいるレイヤーを消す"
                , HA.disabled (count <= 1)
                , HE.onClick LayerDeleted
                ]
                [ text "削除" ]
            , button
                [ HA.class "sketch-layer-up btn btn-ghost btn-mini"
                , HA.title "1 つ手前へ"
                , HA.attribute "aria-label" "1 つ手前へ"
                , HA.disabled (model.layerIndex >= count - 1)
                , HE.onClick LayerRaised
                ]
                [ text "↑" ]
            , button
                [ HA.class "sketch-layer-down btn btn-ghost btn-mini"
                , HA.title "1 つ奥へ"
                , HA.attribute "aria-label" "1 つ奥へ"
                , HA.disabled (model.layerIndex <= 0)
                , HE.onClick LayerLowered
                ]
                [ text "↓" ]
            ]
        , div [ HA.class "mt-1 text-[9px] leading-snug text-ink-faint" ]
            [ text "塗るのは選んだ 1 枚だけ。ほかは薄く見えます" ]
        ]


viewLayerRow : Model -> Int -> LayerSheet -> Html Msg
viewLayerRow model index sheet =
    let
        chosen =
            index == model.layerIndex

        painted =
            paintedCells sheet
    in
    div
        [ HA.class
            ("sketch-layer flex cursor-pointer items-center gap-1 rounded border p-1 "
                ++ (if chosen then
                        "border-accent bg-accent/15"

                    else
                        "border-transparent hover:bg-white/5"
                   )
            )
        , HE.onClick (LayerPicked index)
        ]
        [ button
            [ HA.class
                ("sketch-layer-eye flex h-5 w-5 shrink-0 cursor-pointer items-center justify-center rounded "
                    ++ (if sheet.visible then
                            "text-ink-soft hover:text-ink"

                        else
                            "text-ink-faint"
                       )
                )
            , HA.title
                (if sheet.visible then
                    "隠す(畳んだ絵にも入らなくなります)"

                 else
                    "また見せる"
                )
            , HA.attribute "aria-label" "表示・非表示"

            -- 目だけ押したときに塗る先まで動かさない
            , HE.stopPropagationOn "click" (D.succeed ( LayerVisibilityToggled index, True ))
            ]
            [ eyeGlyph sheet.visible ]
        , Html.input
            [ HA.class "sketch-layer-name field h-5 min-w-0 flex-1 px-1 text-[11px]"
            , HA.value sheet.name
            , HA.placeholder "名前"
            , HE.onInput (LayerRenamed index)

            -- 名前を打つ間は 1 文字ごとに選び直さない(入力の取り合いになる)
            , HE.stopPropagationOn "click" (D.succeed ( LayerPicked index, True ))
            ]
            []
        , span [ HA.class "shrink-0 text-[9px] text-ink-faint" ]
            [ text
                (if painted == 0 then
                    "空"

                 else
                    String.fromInt painted
                )
            ]
        ]


{-| 目のしるし。閉じた目は斜線を足すだけにして、開いた目と形を揃える
(別の絵にすると、並んだ行のどれが隠れているのか見比べにくい)。
-}
eyeGlyph : Bool -> Html msg
eyeGlyph visible =
    Svg.svg
        [ SA.viewBox "0 0 16 16"
        , SA.width "13"
        , SA.height "13"
        , SA.fill "none"
        , SA.stroke "currentColor"
        , SA.strokeWidth "1.3"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        (List.concat
            [ [ Svg.path [ SA.d "M1.5 8 C3.5 4.5 5.8 3 8 3 C10.2 3 12.5 4.5 14.5 8 C12.5 11.5 10.2 13 8 13 C5.8 13 3.5 11.5 1.5 8 Z" ] []
              , Svg.circle [ SA.cx "8", SA.cy "8", SA.r "2" ] []
              ]
            , if visible then
                []

              else
                [ Svg.line [ SA.x1 "2.5", SA.y1 "13.5", SA.x2 "13.5", SA.y2 "2.5" ] [] ]
            ]
        )


{-| サイズバッジ + セルの大きさスライダー + グリッド + つまみ 6 つ。
つまみはグリッドの外周に重ねる(セルの中に置くと塗りの mousedown と
取り合いになる)。グリッドはスクロール箱に入れる — セルを大きくしたり
48x32 まで広げてもパネルがページを乗っ取らないように。
-}
viewGridBlock : Model -> Html Msg
viewGridBlock model =
    div [ HA.class "min-w-0 flex-1" ]
        [ div [ HA.class "mb-1 flex flex-wrap items-center gap-3" ]
            [ span
                [ HA.class
                    (if model.resize /= Nothing then
                        "sketch-size text-[10px] text-accent"

                     else
                        "sketch-size text-[10px] text-ink-faint"
                    )
                ]
                [ text (String.fromInt model.size.w ++ " × " ++ String.fromInt model.size.h ++ " — 端や角をつまんで広げる・縮める") ]
            , span [ HA.class "flex items-center gap-1.5 text-[10px] text-ink-faint" ]
                [ text ("セル: " ++ String.fromInt model.cellPx ++ "px")
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "10"
                    , HA.max "32"
                    , HA.value (String.fromInt model.cellPx)
                    , HA.class "sketch-cell-size w-24 cursor-pointer"
                    , HE.onInput CellSizeEdited
                    ]
                    []
                ]
            , span [ HA.class "flex items-center gap-1.5 text-[10px] text-ink-faint" ]
                [ text ("再現度: " ++ fidelityLabel model.fidelity)
                , Html.input
                    [ HA.type_ "range"
                    , HA.min "1"
                    , HA.max "4"
                    , HA.value (String.fromInt model.fidelity)
                    , HA.class "sketch-fidelity w-24 cursor-pointer"
                    , HE.onInput FidelityEdited
                    ]
                    []
                ]
            ]
        , div [ HA.class "max-h-96 max-w-full overflow-auto" ]
            [ div [ HA.class "relative inline-block p-2" ]
                [ viewGrid model
                , viewHandle model RightEdge
                , viewHandle model BottomEdge
                , viewHandle model CornerEdge
                , viewHandle model LeftEdge
                , viewHandle model TopEdge
                , viewHandle model TopLeftEdge
                ]
            ]
        ]


viewHandle : Model -> Edge -> Html Msg
viewHandle model edge =
    let
        dragging =
            model.resize |> Maybe.map (\going -> going.edge == edge) |> Maybe.withDefault False

        ( marker, place ) =
            case edge of
                RightEdge ->
                    ( "sketch-resize-right", "absolute top-2 right-0 bottom-2 w-2 cursor-ew-resize rounded" )

                BottomEdge ->
                    ( "sketch-resize-bottom", "absolute bottom-0 left-2 right-2 h-2 cursor-ns-resize rounded" )

                CornerEdge ->
                    ( "sketch-resize-corner", "absolute bottom-0 right-0 h-2 w-2 cursor-nwse-resize rounded" )

                LeftEdge ->
                    ( "sketch-resize-left", "absolute top-2 left-0 bottom-2 w-2 cursor-ew-resize rounded" )

                TopEdge ->
                    ( "sketch-resize-top", "absolute top-0 left-2 right-2 h-2 cursor-ns-resize rounded" )

                TopLeftEdge ->
                    ( "sketch-resize-topleft", "absolute top-0 left-0 h-2 w-2 cursor-nwse-resize rounded" )

        tone =
            if dragging then
                " bg-accent/40"

            else if edge == CornerEdge || edge == TopLeftEdge then
                " bg-white/10 hover:bg-accent/40"

            else
                " bg-white/5 hover:bg-accent/40"
    in
    div
        [ HA.class (marker ++ " " ++ place ++ tone)
        , HE.custom "mousedown"
            (D.map2
                (\x y -> { message = ResizeStarted edge { x = x, y = y }, stopPropagation = True, preventDefault = True })
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)
            )
        ]
        []


viewGrid : Model -> Html Msg
viewGrid model =
    let
        colorOf char =
            model.legend
                |> List.filter (\entry -> entry.char == char)
                |> List.head
                |> Maybe.map .fill

        previewSet =
            case model.shape of
                Just going ->
                    Set.fromList (shapeCells model.tool going.start going.current)

                Nothing ->
                    Set.empty

        previewFill =
            colorOf model.active

        -- 選んでいない表示中のレイヤーを重なり順に畳んだ下敷き。
        -- 塗る操作はここへ届かない
        ghostRows =
            model.layers
                |> List.indexedMap Tuple.pair
                |> List.filter (\( index, sheet ) -> index /= model.layerIndex && sheet.visible)
                |> List.map (\( _, sheet ) -> sheet.rows)
                |> List.foldl (mergeRows model.size) (blankRows model.size)
    in
    div [ HA.class "sketch-grid inline-block cursor-crosshair select-none border border-edge" ]
        (List.map2
            (\row ghostRow ->
                List.map2 Tuple.pair (String.toList row) (String.toList ghostRow)
            )
            (fitRows model.size (activeRows model))
            (fitRows model.size ghostRows)
            |> List.indexedMap
                (\y cells ->
                    div [ HA.class "flex" ]
                        (cells
                            |> List.indexedMap
                                (\x ( char, ghost ) ->
                                    viewCell model.cellPx colorOf previewFill previewSet x y char ghost
                                )
                        )
                )
        )


viewCell : Int -> (Char -> Maybe String) -> Maybe String -> Set ( Int, Int ) -> Int -> Int -> Char -> Char -> Html Msg
viewCell cellSize colorOf previewFill previewSet x y char ghost =
    let
        fillOf maybeFill =
            case maybeFill of
                Just fill ->
                    [ HA.style "background-color" fill ]

                Nothing ->
                    [ HA.class "bg-black/20" ]

        look =
            if Set.member ( x, y ) previewSet then
                -- 内側の白い縁で「まだ確定していない」ことを見せる
                fillOf previewFill
                    ++ [ HA.style "box-shadow" "inset 0 0 0 1px rgba(255,255,255,0.5)" ]

            else
                case colorOf char of
                    Just fill ->
                        [ HA.style "background-color" fill ]

                    Nothing ->
                        -- ほかのレイヤーの塗りは薄く敷くだけ。同じ濃さで出すと、
                        -- どのセルが今の一筆で動くのか分からなくなる
                        case colorOf ghost of
                            Just fill ->
                                [ HA.class "sketch-cell-ghost"
                                , HA.style "background-color" fill
                                , HA.style "opacity" "0.3"
                                ]

                            Nothing ->
                                [ HA.class "bg-black/20" ]
    in
    div
        ([ HA.class "sketch-cell shrink-0 border border-black/10"
         , HA.style "width" (String.fromInt cellSize ++ "px")
         , HA.style "height" (String.fromInt cellSize ++ "px")
         , HE.custom "mousedown"
            (D.succeed { message = CellDown ( x, y ), stopPropagation = True, preventDefault = True })
         , HE.onMouseEnter (CellEntered ( x, y ))
         ]
            ++ look
        )
        []


viewNote : Model -> Html Msg
viewNote model =
    div [ HA.class "mt-2" ]
        [ div [ HA.class "text-[10px] text-ink-faint" ] [ text "ラフに対する補足" ]
        , Html.textarea
            [ HA.class "field mt-1 h-auto min-h-[2.5rem] w-full resize-y py-1.5 text-xs leading-relaxed"
            , HA.rows 2
            , HA.value model.note
            , HE.onInput NoteEdited
            ]
            []
        ]


viewSaveRow : Model -> Html Msg
viewSaveRow model =
    div [ HA.class "mt-2 flex flex-wrap items-center gap-1.5" ]
        [ Html.input
            [ HA.class "field h-7 w-40 text-xs"
            , HA.placeholder "保存名(例: stage2)"
            , HA.value model.name
            , HE.onInput NameEdited
            ]
            []
        , button
            [ HA.class "btn h-7 px-2.5 text-[11px]"
            , HA.disabled
                (case model.save of
                    SaveFlying _ ->
                        True

                    _ ->
                        False
                )
            , HE.onClick SaveClicked
            ]
            [ text
                (case model.save of
                    SaveFlying _ ->
                        "保存中…"

                    SaveDone _ ->
                        "保存しました"

                    _ ->
                        "保存"
                )
            ]
        , span [ HA.class "text-[10px] text-ink-faint" ]
            [ text
                (case model.save of
                    SaveFailed reason ->
                        "保存できませんでした — " ++ reason

                    _ ->
                        saveLabel model
                )
            ]
        ]


{-| 保存ボタン脇の案内。次のバージョン番号が何番かが一目で伝わるよう
「vN として保存されます」の形に整える(パスも添えて、迷ったときに探せるようにする)。

sketchPath が SaveDone の場所をそのまま返している間(= 絵も名前も保存直後から
触っていない)は、まだの事でなくもう済んだ事として言う — ボタン側の
「保存しました」と食い違わせないため。
-}
saveLabel : Model -> String
saveLabel model =
    let
        path =
            sketchPath model

        settled =
            case model.save of
                SaveDone done ->
                    done.path == path

                _ ->
                    False
    in
    case versionFromPath path of
        Just v ->
            if settled then
                "v" ++ String.fromInt v ++ " に保存しました（" ++ path ++ "）"

            else
                "v" ++ String.fromInt v ++ " として保存されます（" ++ path ++ "）"

        Nothing ->
            path ++ " に保存されます"
