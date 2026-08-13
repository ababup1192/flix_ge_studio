module EntryOpsTest exposing (suite)

{-| CRUD(追加・複製・削除)が元データへ流す編集の導出を固定する。

Op の中身は E.Value を含む(== に掛けられない)ので、文字列に畳んでから比べる。

-}

import EntryOps
import Expect
import Json.Encode as E
import Refs exposing (Entry(..), PathSeg(..))
import Schema exposing (FieldType(..))
import Test exposing (Test, describe, test)


{-| Op → 比較用の 1 行(op 種 / パス / 値の JSON)。 -}
opText : EntryOps.Op -> String
opText op =
    case op of
        EntryOps.SetAt path value ->
            "set " ++ pathText path ++ " = " ++ E.encode 0 value

        EntryOps.AppendAt path value ->
            "append " ++ pathText path ++ " <- " ++ E.encode 0 value

        EntryOps.RemoveAt path ->
            "remove " ++ pathText path


pathText : List PathSeg -> String
pathText path =
    path
        |> List.map
            (\seg ->
                case seg of
                    Key key ->
                        key

                    Idx i ->
                        String.fromInt i
            )
        |> String.join "/"


emptyField : FieldType -> Schema.Field
emptyField type_ =
    { type_ = type_
    , label = Nothing
    , help = Nothing
    , unit = Nothing
    , hint = Nothing
    , order = Nothing
    , widget = Nothing
    , required = False
    , default = Nothing
    , min = Nothing
    , max = Nothing
    , step = Nothing
    , enabledWhen = Nothing
    }


withDefault : E.Value -> Schema.Field -> Schema.Field
withDefault value field =
    { field | default = Just value }


{-| routes(catalog): type は enum・speed は default 持ち。 -}
routesSection : Schema.Section
routesSection =
    { kind = Schema.Catalog
    , label = Nothing
    , help = Nothing
    , group = Nothing
    , widget = Nothing
    , oneOf = False
    , fields =
        [ ( "type", emptyField (TEnum [ "straight", "sine" ]) )
        , ( "speed", emptyField TFloat |> withDefault (E.float 60) )
        ]
    }


{-| spawns(list): 零値の種類を広く踏む。 -}
spawnsSection : Schema.Section
spawnsSection =
    { kind = Schema.ListKind
    , label = Nothing
    , help = Nothing
    , group = Nothing
    , widget = Nothing
    , oneOf = False
    , fields =
        [ ( "atX", emptyField TInt )
        , ( "name", emptyField TText )
        , ( "active", emptyField TBool )
        , ( "route", emptyField (TRef "routes") )
        , ( "tags", emptyField (TList TText) )
        , ( "spawnAt", emptyField TVec2 )
        ]
    }


doc : E.Value
doc =
    E.object
        [ ( "routes"
          , E.object
                [ ( "slow", E.object [ ( "type", E.string "straight" ), ( "speed", E.float 60 ) ] )
                , ( "slow_copy", E.object [ ( "type", E.string "sine" ), ( "speed", E.float 30 ) ] )
                ]
          )
        , ( "spawns"
          , E.list identity
                [ E.object [ ( "atX", E.int 200 ), ( "route", E.string "slow" ) ]
                , E.object [ ( "atX", E.int 400 ), ( "route", E.string "slow_copy" ) ]
                ]
          )
        ]


suite : Test
suite =
    describe "EntryOps — CRUD の編集導出"
        [ test "newEntry: default があれば default、無ければ type ごとの零値" <|
            \() ->
                EntryOps.newEntry routesSection
                    |> E.encode 0
                    |> Expect.equal """{"type":"straight","speed":60}"""
        , test "newEntry: int/text/bool/ref/list は零値、vec2 は原点({x:0,y:0})" <|
            \() ->
                EntryOps.newEntry spawnsSection
                    |> E.encode 0
                    |> Expect.equal """{"atX":0,"name":"","active":false,"route":"","tags":[],"spawnAt":{"x":0,"y":0}}"""
        , test "freshId: _copy が空いていればそのまま、埋まっていれば番号を足す" <|
            \() ->
                [ EntryOps.freshId [ "slow" ] "slow"
                , EntryOps.freshId [ "slow", "slow_copy" ] "slow"
                , EntryOps.freshId [ "slow", "slow_copy", "slow_copy2" ] "slow"
                ]
                    |> Expect.equal [ "slow_copy", "slow_copy2", "slow_copy3" ]
        , test "addProblem: 空 id と重複は理由付きで拒み、空いていれば通す" <|
            \() ->
                [ EntryOps.addProblem [ "slow" ] "  "
                , EntryOps.addProblem [ "slow" ] "slow"
                , EntryOps.addProblem [ "slow" ] "mid"
                ]
                    |> Expect.equal [ Just "id が空です", Just "\"slow\" は既にあります", Nothing ]
        , test "addCatalogOp: id のキーへ雛形を書き込む" <|
            \() ->
                EntryOps.addCatalogOp "routes" routesSection "mid"
                    |> opText
                    |> Expect.equal """set routes/mid = {"type":"straight","speed":60}"""
        , test "addListOp: 配列末尾へ雛形を挿す" <|
            \() ->
                EntryOps.addListOp "spawns" spawnsSection
                    |> opText
                    |> Expect.equal
                        """append spawns <- {"atX":0,"name":"","active":false,"route":"","tags":[],"spawnAt":{"x":0,"y":0}}"""
        , test "duplicateOp(catalog): 今の値のコピーを空き id へ挿し、その行を選ばせる" <|
            \() ->
                EntryOps.duplicateOp "routes" doc (AtKey "slow")
                    |> Maybe.map (\plan -> ( opText plan.op, plan.select ))
                    |> Expect.equal
                        (Just
                            ( """set routes/slow_copy2 = {"type":"straight","speed":60}"""
                            , AtKey "slow_copy2"
                            )
                        )
        , test "duplicateOp(list): 元の行の直後へコピーを挿し、その行を選ばせる" <|
            \() ->
                EntryOps.duplicateOp "spawns" doc (AtIndex 0)
                    |> Maybe.map (\plan -> ( opText plan.op, plan.select ))
                    |> Expect.equal
                        (Just
                            ( """set spawns = [{"atX":200,"route":"slow"},{"atX":200,"route":"slow"},{"atX":400,"route":"slow_copy"}]"""
                            , AtIndex 1
                            )
                        )
        , test "listRowOp(MoveRow): 配列を丸ごと 1 本の set で書き、動かした行を選ばせる" <|
            \() ->
                let
                    swapped =
                        """set spawns = [{"atX":400,"route":"slow_copy"},{"atX":200,"route":"slow"}]"""
                in
                [ EntryOps.listRowOp "spawns" doc (EntryOps.MoveRow 1 -1)
                , EntryOps.listRowOp "spawns" doc (EntryOps.MoveRow 0 1)
                ]
                    |> List.map (Maybe.map (\plan -> ( opText plan.op, plan.select )))
                    |> Expect.equal
                        [ Just ( swapped, AtIndex 0 )
                        , Just ( swapped, AtIndex 1 )
                        ]
        , test "listRowOp: 端の外へ動かす操作・無い行の複製は何も出さない" <|
            \() ->
                [ EntryOps.listRowOp "spawns" doc (EntryOps.MoveRow 0 -1)
                , EntryOps.listRowOp "spawns" doc (EntryOps.MoveRow 1 1)
                , EntryOps.listRowOp "spawns" doc (EntryOps.DuplicateRow 9)
                ]
                    |> List.map (Maybe.map (\plan -> opText plan.op))
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "duplicateOp: 文書に無いエントリ(消えた直後の選択)は何も出さない" <|
            \() ->
                [ EntryOps.duplicateOp "routes" doc (AtKey "ghost") |> Maybe.map (\p -> opText p.op)
                , EntryOps.duplicateOp "spawns" doc (AtIndex 9) |> Maybe.map (\p -> opText p.op)
                ]
                    |> Expect.equal [ Nothing, Nothing ]
        , test "deleteOp: catalog はキー、list は添字を消す" <|
            \() ->
                [ EntryOps.deleteOp "routes" (AtKey "slow")
                , EntryOps.deleteOp "spawns" (AtIndex 1)
                ]
                    |> List.map opText
                    |> Expect.equal [ "remove routes/slow", "remove spawns/1" ]
        ]
