module SketchPad exposing
    ( Camera(..)
    , Edge(..)
    , Entry
    , Model
    , Msg(..)
    , Out(..)
    , Tool(..)
    , cameraLabel
    , decode
    , encode
    , init
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
    )

{-| 画面のラフ塗り。「大体ここ壁・ここに人」をマス目に雑に塗って、
AI への依頼文に文字グリッドとして自動で挟むための下書き部品。

正本はこの Model が持つ(ゲームの Doc ではないので親の編集直列に乗せない)。
塗りの道具(ペン・バケツ・消しゴム)は PixelEditor の純関数を借りる。
ラベル(何色が何を表すか)は人間が名前・色・ひとことで自由に増やせて、
1 文字コードは Studio が自動で割り振る(人間には見せない)。

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
-}
type Camera
    = TopDown
    | SideView
    | Quarter
    | Behind
    | FirstPerson


{-| リサイズのつまみ。左右の端=列、上下の端=行、角=両方。
左・上のつまみで広げたときはマスが左・上に増える(見えている絵は動かさない)。
-}
type Edge
    = RightEdge
    | BottomEdge
    | CornerEdge
    | LeftEdge
    | TopEdge
    | TopLeftEdge


{-| 保存の進み。失敗はサーバの理由をそのまま出す。 -}
type SaveState
    = SaveIdle
    | SaveFlying
    | SaveDone
    | SaveFailed String


type alias Model =
    { open : Bool
    , preset : String
    , size : { w : Int, h : Int }
    , legend : List Entry
    , rows : List String
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

    -- 1 マスの画面上の大きさ(px)。保存には含めない — 表示倍率は絵ではない
    , cellPx : Int

    -- ラフの再現度(1=雰囲気 〜 4=マス単位で忠実)。絵に対する意図なので
    -- カメラと同じく保存にも依頼文にも載せる
    , fidelity : Int

    -- 戻す用のスナップショット(一筆・1 リサイズで 1 本・直近 20 本まで)。
    -- rows だけ戻すとサイズと食い違うし、legend を外すとラベル削除を戻した
    -- とき凡例の無い文字がグリッドに残るし、hidden を外すと左シフトの undo 後に
    -- 広げたとき退避の塗りが列ずれで復活する。だから必ず 4 つ対で戻す
    , undo : List { size : { w : Int, h : Int }, rows : List String, legend : List Entry, hidden : List String }
    , save : SaveState
    , camera : Camera

    -- カメラの絵柄一覧を開いているか(普段はチップ 1 枚に畳む)
    , cameraOpen : Bool

    -- 縮めて見えなくなった塗りの退避(これまで塗った全域)。
    -- 保存には含めない — 保存物 = 見えている絵、という約束を守るため
    , hidden : List String

    -- グリッドのつまみをドラッグ中だけ Just。親はこの間だけグローバル mousemove/up を購読する。
    -- 開始時の状態を丸ごと持つのは、寸法を毎回ここから計算し直すため —
    -- 差分シフトの積み重ねだと、左・上へ縮めすぎた瞬間に列が落ちて
    -- 同じドラッグ内で戻しても失われる(右・下と手触りが非対称になる)
    , resize :
        Maybe
            { edge : Edge
            , start : { x : Float, y : Float }
            , startSize : { w : Int, h : Int }
            , startRows : List String
            , startHidden : List String
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
    | UndoClicked
    | NoteEdited String
    | NameEdited String
    | CellSizeEdited String
    | FidelityEdited String
    | SaveClicked


{-| 空きマスの文字。ラベルには使わせない予約。 -}
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


{-| 1 マスの画面上の大きさ(px)の初期値。 -}
defaultCellPx : Int
defaultCellPx =
    20


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
    , rows = List.repeat preset.size.h (String.repeat preset.size.w (String.fromChar emptyChar))
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
    , hidden = []
    , resize = Nothing
    }



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

        _ ->
            Nothing



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


{-| sketch.json の中身。desc が空のラベルは desc ごと省く。 -}
encode : Model -> String
encode model =
    E.encode 2
        (E.object
            [ ( "version", E.int 1 )
            , ( "kind", E.string "sketch" )
            , ( "preset", E.string model.preset )
            , ( "size", E.object [ ( "w", E.int model.size.w ), ( "h", E.int model.size.h ) ] )
            , ( "camera", E.string (cameraId model.camera) )
            , ( "fidelity", E.int model.fidelity )
            , ( "legend", E.list encodeEntry model.legend )
            , ( "rows", E.list E.string model.rows )
            , ( "note", E.string model.note )
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
    D.map7
        (\preset size cameraText fidelity legend rows note ->
            let
                base =
                    fromPreset preset
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
                , rows = rows
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


{-| 保存先の相対パス。名前が空なら "screen"。
区切り文字はサーバの門番より先に UI で捨てる(エラーで戸惑わせない)。
-}
sketchPath : Model -> String
sketchPath model =
    "draft/sketch/" ++ safeName model.name ++ ".sketch.json"


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
-}
promptSection : Model -> Maybe String
promptSection model =
    let
        usedChars =
            model.rows |> String.concat |> String.toList

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
        in
        Just
            (String.join "\n"
                (List.concat
                    [ [ "## 画面のラフ（カメラ: " ++ cameraLabel model.camera ++ "、" ++ String.fromInt model.size.w ++ "x" ++ String.fromInt model.size.h ++ "、再現度: " ++ fidelityLabel model.fidelity ++ "、1文字=1マス。塗りから自動生成）"
                      , "凡例: " ++ legendLine
                      ]
                    , model.rows
                    , noteLines
                    , [ "凡例のかっこ内は意図です。" ++ fidelityClause model.fidelity
                      , "原本: " ++ sketchPath model
                      ]
                    ]
                )
            )


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
            "マス単位でできるだけ忠実に再現してください。"

        _ ->
            "ラフなのでマス単位の忠実さは不要です。雰囲気が伝われば十分です。"


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
            -- マスの大きさ(見た目の好み)と再現度(人の意図)も残す
            ( { fresh
                | open = True
                , note = model.note
                , name = model.name
                , cellPx = model.cellPx
                , fidelity = model.fidelity
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
                        , rows = List.map erase model.rows
                        , hidden = List.map erase model.hidden
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
                                floodAt brush point model.rows

                            _ ->
                                paintAt brush point model.rows
                in
                ( { model
                    | rows = next
                    , stroke = model.tool /= Bucket
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
                        ( { model | rows = paintAt (brushChar model) point model.rows }, OutNone )

                    else
                        ( model, OutNone )

        StrokeEnded ->
            case model.shape of
                Just going ->
                    ( { model
                        | rows =
                            shapeCells model.tool going.start going.current
                                |> List.foldl (paintAt (brushChar model)) model.rows
                        , shape = Nothing
                        , stroke = False
                      }
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
                        , startRows = model.rows
                        , startHidden = model.hidden
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
                            { model
                                | size = going.startSize
                                , rows = going.startRows
                                , hidden = going.startHidden
                            }
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
                                , rows = going.startRows
                                , legend = model.legend
                                , hidden = going.startHidden
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

        UndoClicked ->
            case model.undo of
                previous :: rest ->
                    ( { model
                        | size = previous.size
                        , rows = previous.rows
                        , legend = previous.legend
                        , hidden = previous.hidden
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
            ( { model | save = SaveFlying }
            , OutSave { path = sketchPath model, content = encode model }
            )


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
    let
        fullW =
            max model.size.w
                (model.hidden |> List.map String.length |> List.maximum |> Maybe.withDefault 0)

        fullH =
            max model.size.h (List.length model.hidden)

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
                                pad fullW (rowAt y model.hidden |> Maybe.withDefault "")
                        in
                        case rowAt y model.rows of
                            Just visible ->
                                pad model.size.w visible
                                    |> String.left model.size.w
                                    |> (\front -> front ++ String.dropLeft model.size.w behind)

                            Nothing ->
                                behind
                    )

        dW =
            newSize.w - model.size.w

        dH =
            newSize.h - model.size.h

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
    { model | size = newSize, rows = cut, hidden = shifted }


{-| undo 1 本ぶんの控え。 -}
snapshot : Model -> { size : { w : Int, h : Int }, rows : List String, legend : List Entry, hidden : List String }
snapshot model =
    { size = model.size, rows = model.rows, legend = model.legend, hidden = model.hidden }


{-| 押してから離すまでで形を決める道具か。 -}
isShapeTool : Tool -> Bool
isShapeTool tool =
    tool == Line || tool == Rect || tool == Ellipse


{-| 形の道具が確定・プレビューで塗るマスの一覧。ワイルドカードを使わず
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


{-| 2 点を結ぶ直線のマス。長い方の軸の歩数で線形補間する。 -}
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


{-| 2 点を対角とする四角の枠線のマス。中は塗らない —
塗り潰しはバケツで一撫でできるので、塗り潰し版の道具を別に増やさない。
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


{-| 外接矩形に内接する枠線の楕円のマス。列走査と行走査の両方を重ねるのは、
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


{-| 保存成功。 -}
saved : Model -> Model
saved model =
    { model | save = SaveDone }


{-| 保存失敗。理由は保存ボタンの脇に出す。 -}
saveFailed : String -> Model -> Model
saveFailed reason model =
    { model | save = SaveFailed reason }



-- 表示


view : Model -> Html Msg
view model =
    div [ HA.class "sketch-pad mb-4 rounded-lg border border-edge bg-panel" ]
        (if model.open then
            [ viewHeader model, viewBody model ]

         else
            [ viewHeader model ]
        )


viewHeader : Model -> Html Msg
viewHeader model =
    button
        [ HA.class "sketch-toggle flex w-full cursor-pointer items-center gap-2 p-3 text-left text-xs font-semibold text-ink"
        , HE.onClick ToggleOpen
        ]
        [ span []
            [ text
                (if model.open then
                    "▾ 画面のラフを描く"

                 else
                    "▸ 画面のラフを描く"
                )
            ]
        , span [ HA.class "text-[10px] font-normal text-ink-faint" ]
            [ text "任意 — 「大体ここに何がある」を塗って AI に伝える" ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    div [ HA.class "px-3 pb-3" ]
        (inlineParts model ++ [ viewSaveRow model ])


{-| ヘッダも保存行も持たない埋め込み用の姿。保存先(プロジェクト)がまだ無い
場所(新しいゲームのラフ札)に置くための形 — 保存は親の仕事になる。
-}
viewInline : Model -> Html Msg
viewInline model =
    div [ HA.class "px-3 pb-3" ] (inlineParts model)


inlineParts : Model -> List (Html Msg)
inlineParts model =
    List.concat
        [ [ viewPresets model
          , viewCamera model
          , viewChips model
          ]
        , viewChipEditor model
        , [ viewTools model
          , viewGridBlock model
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
                    , HA.title "このラベルと、塗ったマスをすべて消します"
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
        [ toolButton Pen "ペン — 選んだラベルで 1 マスずつ塗る"
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
                        ([ TopDown, SideView, Quarter, Behind, FirstPerson ]
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
        ]


miniGrid : List (Html.Attribute msg) -> Html msg
miniGrid attrs =
    div (HA.class "grid grid-cols-3 gap-px" :: attrs)
        (List.repeat 9 (div [ HA.class "h-1.5 w-1.5 rounded-[1px] bg-ink-soft/50" ] []))


{-| サイズバッジ + マスの大きさスライダー + グリッド + つまみ 6 つ。
つまみはグリッドの外周に重ねる(マスの中に置くと塗りの mousedown と
取り合いになる)。グリッドはスクロール箱に入れる — マスを大きくしたり
48x32 まで広げてもパネルがページを乗っ取らないように。
-}
viewGridBlock : Model -> Html Msg
viewGridBlock model =
    div []
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
                [ text ("マス: " ++ String.fromInt model.cellPx ++ "px")
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
    in
    div [ HA.class "sketch-grid inline-block cursor-crosshair select-none border border-edge" ]
        (model.rows
            |> List.indexedMap
                (\y row ->
                    div [ HA.class "flex" ]
                        (row
                            |> String.toList
                            |> List.indexedMap (\x char -> viewCell model.cellPx colorOf previewFill previewSet x y char)
                        )
                )
        )


viewCell : Int -> (Char -> Maybe String) -> Maybe String -> Set ( Int, Int ) -> Int -> Int -> Char -> Html Msg
viewCell cellSize colorOf previewFill previewSet x y char =
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
                fillOf (colorOf char)
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
            , HA.disabled (model.save == SaveFlying)
            , HE.onClick SaveClicked
            ]
            [ text
                (case model.save of
                    SaveFlying ->
                        "保存中…"

                    SaveDone ->
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
                        sketchPath model ++ " に保存されます"
                )
            ]
        ]
