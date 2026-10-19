module TalkRun exposing
    ( Choice
    , Entry(..)
    , Program
    , Reading
    , Status(..)
    , choose
    , emptyProgram
    , overridesFrom
    , programDecoder
    , start
    , valueOf
    )

{-| 選択肢を選びながら読み進める欄の決まり(公式の Try Yarn Spinner の「流す欄」に近い形)。

組んだ木(dialogue.json)を Studio の中で流す: 台詞を順に出し、選択肢で止まり、選ぶと続きへ進む。
`<<set>>`・`<<if>>`・行末の `<<if>>`(選べない選択肢は灰色で並ぶ)・`{式}`・`visited` / `visited_count` を
engine の YarnRunner と同じ決まりで解く(訪れた数は「ノードを出た回数」)。`<<jump>>` は残りを捨てて行き先へ。
ゲームを待つコマンド(`<<wait>>` など)は流さずに「コマンド」として欄に出すだけ。

絵は出さない(本物の画面は右のプレビュー)。欄の行を押すと、その行をその時の変数の値で撮る(TalkView)。

-}

import Dict exposing (Dict)
import Json.Decode as D
import TalkTree exposing (Value(..))


-- 木の形(engine の go/internal/yarn/tree.go と同じ欄)


type Expr
    = EValue Value
    | EVar String
    | EVisited String
    | EVisitedCount String
    | EUnary String Expr
    | EBinary String Expr Expr


type Part
    = PText String
    | PExpr Expr


type Stmt
    = SLine { line : Int, speaker : String, text : String, parts : List Part }
    | SOptions (List Option)
    | SJump String
    | SStop
    | SCommand { line : Int, name : String, args : List String }
    | SSet String Expr
    | SIf (List Clause)


type alias Option =
    { line : Int, speaker : String, text : String, parts : List Part, condition : Maybe Expr, body : List Stmt }


type alias Clause =
    { condition : Maybe Expr, body : List Stmt }


{-| ノードの名前 → 本文と、宣言した変数の初めの値。 -}
type alias Program =
    { nodes : Dict String (List Stmt), declared : Dict String Value }


emptyProgram : Program
emptyProgram =
    { nodes = Dict.empty, declared = Dict.empty }


-- 流れ


{-| 欄に出す 1 行: 台詞(行・話し手・差し込んだ後の字) / コマンド / 選んだ選択肢 / ノードに入った印。 -}
type Entry
    = Said { line : Int, speaker : String, text : String }
    | Commanded { line : Int, text : String }
    | Chose { line : Int, text : String }
    | Entered String


{-| 並べる選択肢 1 つ(選べないものも灰色で並ぶ)。 -}
type alias Choice =
    { line : Int, text : String, available : Bool }


{-| 流れの今。

  - Choosing … 選択肢で止まっている
  - Ended … 終わった(最後のノードを出た・<<stop>>)
  - Broken 理由 … 流せない(知らないノードへの jump・型の合わない式・止まらない輪)

-}
type Status
    = Choosing (List Choice)
    | Ended
    | Broken String


{-| 読み進めている 1 回ぶん。frames は今のノードの残り(上が先)。 -}
type alias Reading =
    { node : String
    , frames : List (List Stmt)
    , pending : List Option
    , vars : Dict String Value
    , visits : Dict String Int
    , log : List Entry
    , status : Status
    }


{-| node の頭から流す。vars は初めの値に重ねる値(変数の一覧で変えた物)。 -}
start : Program -> Dict String Value -> String -> Reading
start program overrides node =
    let
        fresh =
            { node = node
            , frames = []
            , pending = []
            , vars = Dict.union overrides program.declared
            , visits = Dict.empty
            , log = []
            , status = Ended
            }
    in
    enter program node fresh |> run program


{-| i 番目(並べた順・選べないものも数える)を選ぶ。選べない番号は何も変えない。 -}
choose : Program -> Int -> Reading -> Reading
choose program i reading =
    case ( reading.status, reading.pending |> List.drop i |> List.head ) of
        ( Choosing choices, Just option ) ->
            if choices |> List.drop i |> List.head |> Maybe.map .available |> Maybe.withDefault False then
                { reading
                    | frames = option.body :: reading.frames
                    , pending = []
                    , log = reading.log ++ [ Chose { line = option.line, text = render reading option.parts option.text } ]
                }
                    |> run program

            else
                reading

        _ ->
            reading


{-| 変数の一覧で変えた値の字を、宣言の型で読む(読めない字・宣言の無い名前は捨てる)。 -}
overridesFrom : Program -> Dict String String -> Dict String Value
overridesFrom program texts =
    texts
        |> Dict.toList
        |> List.filterMap
            (\( name, text ) ->
                Dict.get name program.declared
                    |> Maybe.andThen
                        (\declared ->
                            case declared of
                                Number _ ->
                                    String.toFloat text |> Maybe.map Number

                                Flag _ ->
                                    case text of
                                        "true" ->
                                            Just (Flag True)

                                        "false" ->
                                            Just (Flag False)

                                        _ ->
                                            Nothing

                                Text _ ->
                                    Just (Text text)
                        )
                    |> Maybe.map (Tuple.pair name)
            )
        |> Dict.fromList


{-| 変数の今の値(宣言していなければ Nothing)。 -}
valueOf : String -> Reading -> Maybe Value
valueOf name reading =
    Dict.get name reading.vars


enter : Program -> String -> Reading -> Reading
enter program node reading =
    case Dict.get node program.nodes of
        Just body ->
            { reading | node = node, frames = [ body ], log = reading.log ++ [ Entered node ] }

        Nothing ->
            { reading | frames = [], status = Broken ("ノード " ++ node ++ " がありません") }


{-| 止まる所(選択肢・終わり)まで流す。止まらない輪は 2000 文で打ち切る。 -}
run : Program -> Reading -> Reading
run program reading =
    runFor 2000 program reading


runFor : Int -> Program -> Reading -> Reading
runFor fuel program reading =
    if fuel <= 0 then
        { reading | status = Broken "2000 文流しても選択肢にも終わりにも着きません(止まらない輪?)" }

    else
        case reading.status of
            Broken _ ->
                reading

            _ ->
                case reading.frames of
                    [] ->
                        { reading | visits = addVisit reading.node reading.visits, status = Ended }

                    [] :: rest ->
                        runFor (fuel - 1) program { reading | frames = rest }

                    (stmt :: more) :: rest ->
                        let
                            next =
                                { reading | frames = more :: rest }
                        in
                        case stmt of
                            SLine l ->
                                runFor (fuel - 1) program { next | log = next.log ++ [ Said { line = l.line, speaker = l.speaker, text = render next l.parts l.text } ] }

                            SCommand c ->
                                runFor (fuel - 1) program { next | log = next.log ++ [ Commanded { line = c.line, text = "<<" ++ String.join " " (c.name :: c.args) ++ ">>" } ] }

                            SSet name expr ->
                                case eval next expr of
                                    Ok value ->
                                        runFor (fuel - 1) program { next | vars = Dict.insert name value next.vars }

                                    Err why ->
                                        { next | status = Broken why }

                            SIf clauses ->
                                case pickClause next clauses of
                                    Ok (Just body) ->
                                        runFor (fuel - 1) program { next | frames = body :: next.frames }

                                    Ok Nothing ->
                                        runFor (fuel - 1) program next

                                    Err why ->
                                        { next | status = Broken why }

                            SJump target ->
                                runFor (fuel - 1) program (enter program target { next | visits = addVisit next.node next.visits })

                            SStop ->
                                { next | frames = [], visits = addVisit next.node next.visits, status = Ended }

                            SOptions options ->
                                let
                                    choices =
                                        options
                                            |> List.map
                                                (\o ->
                                                    { line = o.line
                                                    , text = render next o.parts o.text
                                                    , available =
                                                        case o.condition of
                                                            Nothing ->
                                                                True

                                                            Just c ->
                                                                eval next c == Ok (Flag True)
                                                    }
                                                )
                                in
                                if List.any .available choices then
                                    { next | pending = options, status = Choosing choices }

                                else
                                    -- 公式と同じく、全部が選べない選択肢のまとまりは飛ばす
                                    runFor (fuel - 1) program next


addVisit : String -> Dict String Int -> Dict String Int
addVisit node visits =
    Dict.update node (\n -> Just (Maybe.withDefault 0 n + 1)) visits


pickClause : Reading -> List Clause -> Result String (Maybe (List Stmt))
pickClause reading clauses =
    case clauses of
        [] ->
            Ok Nothing

        c :: rest ->
            case c.condition of
                Nothing ->
                    Ok (Just c.body)

                Just expr ->
                    case eval reading expr of
                        Ok (Flag True) ->
                            Ok (Just c.body)

                        Ok (Flag False) ->
                            pickClause reading rest

                        Ok _ ->
                            Err "<<if>> の式が真偽になりません"

                        Err why ->
                            Err why


{-| {式} を差し込んだ字(parts が無ければ書いたとおり)。 -}
render : Reading -> List Part -> String -> String
render reading parts text =
    if List.isEmpty parts then
        text

    else
        parts
            |> List.map
                (\p ->
                    case p of
                        PText t ->
                            t

                        PExpr e ->
                            eval reading e |> Result.map TalkTree.valueText |> Result.withDefault "{?}"
                )
            |> String.concat


eval : Reading -> Expr -> Result String Value
eval reading expr =
    case expr of
        EValue v ->
            Ok v

        EVar name ->
            Dict.get name reading.vars |> Result.fromMaybe ("変数 " ++ name ++ " を宣言していません")

        EVisited node ->
            Ok (Flag (Dict.get node reading.visits |> Maybe.withDefault 0 |> (\n -> n > 0)))

        EVisitedCount node ->
            Ok (Number (Dict.get node reading.visits |> Maybe.withDefault 0 |> toFloat))

        EUnary op a ->
            case ( op, eval reading a ) of
                ( "not", Ok (Flag b) ) ->
                    Ok (Flag (not b))

                ( "neg", Ok (Number n) ) ->
                    Ok (Number -n)

                ( _, Err why ) ->
                    Err why

                _ ->
                    Err (op ++ " の型が合いません")

        EBinary op a b ->
            Result.map2 Tuple.pair (eval reading a) (eval reading b)
                |> Result.andThen (binary op)


binary : String -> ( Value, Value ) -> Result String Value
binary op pair =
    case ( op, pair ) of
        ( "+", ( Number x, Number y ) ) ->
            Ok (Number (x + y))

        ( "+", ( Text x, Text y ) ) ->
            Ok (Text (x ++ y))

        ( "-", ( Number x, Number y ) ) ->
            Ok (Number (x - y))

        ( "*", ( Number x, Number y ) ) ->
            Ok (Number (x * y))

        ( "/", ( Number x, Number y ) ) ->
            if y == 0 then
                Err "0 で割りました"

            else
                Ok (Number (x / y))

        ( "%", ( Number x, Number y ) ) ->
            if y == 0 then
                Err "0 で割りました"

            else
                Ok (Number (x - y * toFloat (floor (x / y))))

        ( "==", ( x, y ) ) ->
            Ok (Flag (x == y))

        ( "!=", ( x, y ) ) ->
            Ok (Flag (x /= y))

        ( "<", ( Number x, Number y ) ) ->
            Ok (Flag (x < y))

        ( "<=", ( Number x, Number y ) ) ->
            Ok (Flag (x <= y))

        ( ">", ( Number x, Number y ) ) ->
            Ok (Flag (x > y))

        ( ">=", ( Number x, Number y ) ) ->
            Ok (Flag (x >= y))

        ( "and", ( Flag x, Flag y ) ) ->
            Ok (Flag (x && y))

        ( "or", ( Flag x, Flag y ) ) ->
            Ok (Flag (x || y))

        _ ->
            Err (op ++ " の型が合いません")



-- デコーダ


programDecoder : D.Decoder Program
programDecoder =
    D.map2 Program
        (D.oneOf
            [ D.field "nodes" (D.list (D.map2 Tuple.pair (D.field "title" D.string) (D.oneOf [ D.field "body" (D.list stmtDecoder), D.succeed [] ])))
                |> D.map Dict.fromList
            , D.succeed Dict.empty
            ]
        )
        (D.oneOf
            [ D.field "variables" (D.list (D.map2 Tuple.pair (D.field "name" D.string) (D.field "value" valueDecoder)))
                |> D.map Dict.fromList
            , D.succeed Dict.empty
            ]
        )


valueDecoder : D.Decoder Value
valueDecoder =
    D.oneOf [ D.map Flag D.bool, D.map Number D.float, D.map Text D.string ]


stmtDecoder : D.Decoder Stmt
stmtDecoder =
    let
        str key =
            D.oneOf [ D.field key D.string, D.succeed "" ]

        int key =
            D.oneOf [ D.field key D.int, D.succeed 0 ]

        body =
            D.oneOf [ D.field "body" (D.list (D.lazy (\_ -> stmtDecoder))), D.succeed [] ]
    in
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "line" ->
                        D.map4 (\line speaker text parts -> SLine { line = line, speaker = speaker, text = text, parts = parts })
                            (int "line")
                            (str "speaker")
                            (str "text")
                            partsDecoder

                    "options" ->
                        D.field "options"
                            (D.list
                                (D.map6 Option
                                    (int "line")
                                    (str "speaker")
                                    (str "text")
                                    partsDecoder
                                    (D.maybe (D.field "condition" exprDecoder))
                                    body
                                )
                            )
                            |> D.map SOptions

                    "jump" ->
                        D.map SJump (D.field "node" D.string)

                    "stop" ->
                        D.succeed SStop

                    "command" ->
                        D.map3 (\line name args -> SCommand { line = line, name = name, args = args })
                            (int "line")
                            (str "name")
                            (D.oneOf [ D.field "args" (D.list D.string), D.succeed [] ])

                    "set" ->
                        D.map2 SSet (D.field "name" D.string) (D.field "expr" exprDecoder)

                    "if" ->
                        D.field "clauses" (D.list (D.map2 Clause (D.maybe (D.field "condition" exprDecoder)) body))
                            |> D.map SIf

                    _ ->
                        D.succeed (SCommand { line = 0, name = kind, args = [] })
            )


partsDecoder : D.Decoder (List Part)
partsDecoder =
    D.oneOf
        [ D.field "parts"
            (D.list
                (D.oneOf
                    [ D.field "expr" exprDecoder |> D.map PExpr
                    , D.field "text" D.string |> D.map PText
                    ]
                )
            )
        , D.succeed []
        ]


exprDecoder : D.Decoder Expr
exprDecoder =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                let
                    args =
                        D.field "args" (D.list (D.lazy (\_ -> exprDecoder)))
                in
                case kind of
                    "value" ->
                        D.map EValue (D.field "value" valueDecoder)

                    "variable" ->
                        D.map EVar (D.field "name" D.string)

                    "visited" ->
                        D.map EVisited (D.field "name" D.string)

                    "visited_count" ->
                        D.map EVisitedCount (D.field "name" D.string)

                    _ ->
                        args
                            |> D.andThen
                                (\list ->
                                    case list of
                                        [ a ] ->
                                            D.succeed (EUnary kind a)

                                        [ a, b ] ->
                                            D.succeed (EBinary kind a b)

                                        _ ->
                                            D.fail ("式 " ++ kind ++ " の引数の数")
                                )
            )
