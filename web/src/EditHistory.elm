module EditHistory exposing
    ( Before(..)
    , Entry
    , History
    , batchPaths
    , canRedo
    , canUndo
    , cutOnExternalChange
    , depth
    , empty
    , inverse
    , push
    , redo
    , undo
    )

{-| 元に戻す / やり直すの骨組み(純ロジック)。

まだ画面には繋がっていない。ここで決めるのは「1 手とは何か」「その逆は何か」
の 2 つだけで、繋ぎ込みは次の手術に回す。

## 何を 1 手と数えるか

編集はすべて Edit.Payload の一本道(docEdit)を通る。だから履歴も payload を
そのまま積む — 画面の操作ごとに専用の記録を作らない。スライダーのドラッグの
ように 1 つの操作が payload を何十件も生む場合は、同じ group 名を付けて
積むことで 1 手に畳む(畳んだ手は「最初の逆操作」と「最後の順操作」を持つ)。

## 逆操作の導き方(新しい書き戻し経路を発明しない)

戻すのも普通の編集 1 件でなければならない。op の語彙は Set / Append / Remove /
BatchSet のままで、位置指定の挿入は持たない。そこで、配列の途中を消した手だけは
「その配列を丸ごと Set で書き戻す」に倒す(一覧の並べ替えと同じ流儀)。

| 積んだ手                   | 旧値(Before)      | 逆操作                          |
| -------------------------- | ------------------ | ------------------------------- |
| Set path v(元の値あり)   | Value (Just old)   | Set path old                    |
| Set path v(キーが無かった) | Value Nothing      | Remove path                     |
| Append path v              | Array old          | Remove path[len old]            |
| Remove path(キー)        | Value (Just old)   | Set path old                    |
| Remove path[i](配列要素) | Array old          | Set path old(配列を丸ごと)    |
| BatchSet(set の列)      | Batch [(p, old)]   | BatchSet(各 p を旧値へ)       |

逆が組めない組み合わせ(旧値が無いのに Remove を積む等)は履歴に積まず、
その場で履歴を切る — 間違った逆操作を持つくらいなら、戻せない方が安全。

## 外から文書が変わったら切る

履歴は「積んだ時の文書」を前提にしている。ファイルの切り替え・再読込・
テキスト欄の直接編集・保存の衝突解決のように、外から正本が入れ替わった時は
前提が崩れるので全部捨てる(cutOnExternalChange)。

## 画面へ繋ぐときの方針(次の手術)

  - ⌘Z / ⇧⌘Z は画面全体の 1 か所(Main の subscriptions)で受け、
    undo が返した payload を今の queueEdit へそのまま流す。
  - 盤面(MapEditor)は今も自前の undo を持っている。まずはこの履歴を
    「盤面の外の編集」だけに使い、盤面が前面の間は今までどおり盤面側に任せる。
    盤面の一筆も payload 1 本に畳めるようになった時点で、こちらへ寄せる。
  - 履歴は開いているファイルにひも付く(file が変われば捨てる)。

-}

import Edit exposing (Op(..), Payload, Seg(..))
import Json.Decode as D
import Json.Encode as E


{-| 逆操作を組むのに要る「編集する前の姿」。どれが要るかは op で決まる。 -}
type Before
    = -- その場所の旧値(Nothing = そこには何も無かった)
      Value (Maybe E.Value)
      -- 触る配列の旧内容(Append / 配列要素の Remove)
    | Array (List E.Value)
      -- BatchSet の中身 1 件ずつの (場所, 旧値)
    | Batch (List ( List Seg, Maybe E.Value ))


{-| 1 手。forward は積んだ編集、backward はそれを打ち消す編集。 -}
type alias Entry =
    { label : String
    , group : Maybe String
    , forward : Payload
    , backward : Payload
    }


{-| done の先頭が「直近の 1 手」。undone は やり直し 待ち(先頭が次に出る手)。 -}
type alias History =
    { file : Maybe String
    , done : List Entry
    , undone : List Entry
    }


empty : History
empty =
    { file = Nothing, done = [], undone = [] }


canUndo : History -> Bool
canUndo history =
    not (List.isEmpty history.done)


canRedo : History -> Bool
canRedo history =
    not (List.isEmpty history.undone)


{-| (戻せる手の数, やり直せる手の数)。画面のボタンの活き死にに使う。 -}
depth : History -> ( Int, Int )
depth history =
    ( List.length history.done, List.length history.undone )


{-| 積む。逆が組めない手は積まずに履歴を切る(戻せない方を選ぶ)。
別のファイルの手が来たら、それまでの履歴は捨てて新しいファイルの 1 手目にする。
同じ group の手が続いたら 1 手に畳む(ドラッグ中の洪水を 1 回の戻すで戻せるように)。
-}
push :
    { file : String, label : String, group : Maybe String, payload : Payload, before : Before }
    -> History
    -> History
push item history =
    case inverse item.payload item.before of
        Nothing ->
            { empty | file = Just item.file }

        Just backward ->
            let
                entry =
                    { label = item.label
                    , group = item.group
                    , forward = item.payload
                    , backward = backward
                    }

                sameFile =
                    history.file == Just item.file
            in
            if not sameFile then
                { file = Just item.file, done = [ entry ], undone = [] }

            else
                case ( item.group, history.done ) of
                    ( Just group, last :: rest ) ->
                        if last.group == Just group then
                            -- 畳む: 見た目は最後の姿・戻り先は最初の姿
                            { history
                                | done = { entry | backward = last.backward } :: rest
                                , undone = []
                            }

                        else
                            { history | done = entry :: history.done, undone = [] }

                    _ ->
                        -- 新しい手を積んだらやり直しの先は消える(枝分かれは持たない)
                        { history | done = entry :: history.done, undone = [] }


{-| 1 手戻す。返るのは「今すぐ流すべき編集」と、進めた履歴。 -}
undo : History -> Maybe ( Payload, History )
undo history =
    case history.done of
        entry :: rest ->
            Just ( entry.backward, { history | done = rest, undone = entry :: history.undone } )

        [] ->
            Nothing


redo : History -> Maybe ( Payload, History )
redo history =
    case history.undone of
        entry :: rest ->
            Just ( entry.forward, { history | done = entry :: history.done, undone = rest } )

        [] ->
            Nothing


{-| 外から正本が入れ替わった(開き直し・再読込・テキスト直接編集・衝突の上書き)。
履歴の前提が崩れるので全部捨てる — 古い逆操作は今の文書には当たらない。
-}
cutOnExternalChange : History -> History
cutOnExternalChange history =
    { file = history.file, done = [], undone = [] }


{-| 逆操作の導出。組めない組み合わせは Nothing(呼び側が履歴を切る)。 -}
inverse : Payload -> Before -> Maybe Payload
inverse payload before =
    case ( payload.op, before ) of
        ( SetOp, Value (Just old) ) ->
            Just { payload | value = old }

        ( SetOp, Value Nothing ) ->
            -- 無かったキーを書いた手 → 消せば元通り
            Just { op = RemoveOp, path = payload.path, value = E.null, isInt = False }

        ( AppendOp, Array old ) ->
            -- 足したのは必ず末尾の 1 件(足す前の長さがその添字)
            Just
                { op = RemoveOp
                , path = payload.path ++ [ IdxSeg (List.length old) ]
                , value = E.null
                , isInt = False
                }

        ( RemoveOp, Value (Just old) ) ->
            Just { op = SetOp, path = payload.path, value = old, isInt = payload.isInt }

        ( RemoveOp, Array old ) ->
            -- 配列の途中を消した手。位置指定の挿入は持たないので、
            -- その配列を丸ごと書き戻す(一覧の並べ替えと同じ流儀)
            arrayParent payload.path
                |> Maybe.map
                    (\parent ->
                        { op = SetOp, path = parent, value = E.list identity old, isInt = False }
                    )

        ( BatchSetOp, Batch olds ) ->
            Just
                { op = BatchSetOp
                , path = payload.path
                , value = E.list encodeBatchSet (List.map toOldSet olds)
                , isInt = False
                }

        _ ->
            Nothing


{-| 配列要素のパス(末尾が添字)から、その配列自身のパスへ。 -}
arrayParent : List Seg -> Maybe (List Seg)
arrayParent path =
    case List.reverse path of
        (IdxSeg _) :: rest ->
            Just (List.reverse rest)

        _ ->
            Nothing


{-| 旧値の無い(元は書かれていなかった)場所は null を書き戻す —
BatchSet はキー単位の削除を持たないので、消すのでなく null で見えるままにする。
-}
toOldSet : ( List Seg, Maybe E.Value ) -> ( List Seg, E.Value )
toOldSet ( path, old ) =
    ( path, Maybe.withDefault E.null old )


encodeBatchSet : ( List Seg, E.Value ) -> E.Value
encodeBatchSet ( path, value ) =
    E.object
        [ ( "op", E.string "set" )
        , ( "path", E.list Edit.encodeSeg path )
        , ( "value", value )
        , ( "intField", E.bool False )
        ]


{-| BatchSet の中身(encode 済みの set の列)から場所だけを読む。
履歴を積む側が「どこの旧値を控えるか」を知るための口。
-}
batchPaths : E.Value -> List (List Seg)
batchPaths value =
    D.decodeValue (D.list (D.field "path" (D.list segDecoder))) value
        |> Result.withDefault []


segDecoder : D.Decoder Seg
segDecoder =
    D.oneOf [ D.map KeySeg D.string, D.map IdxSeg D.int ]
