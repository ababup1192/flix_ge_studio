module TalkTree exposing
    ( Link
    , LinkKind(..)
    , Tree
    , TreeNode
    , Value(..)
    , Variable
    , decoder
    , empty
    , treePathOf
    , valueText
    )

{-| 組んだ会話の木(.yarn と同じフォルダの dialogue.json。engine の bin/fge yarn build が書く)を読む。

会話の画面の「変数の一覧」(`<<declare>>` の名前と初めの値)と「ノードの図」(ノードの箱と、
`<<jump>>`・選択肢の行き先の矢印)の元。.yarn の文法は持たない(読むのは道具が書いた木だけ)。

-}

import Json.Decode as D


{-| 変数の値(公式と同じ 3 つの型)。 -}
type Value
    = Number Float
    | Flag Bool
    | Text String


{-| `<<declare $名前 = 初めの値>>` の 1 つ。 -}
type alias Variable =
    { name : String, value : Value, file : String, line : Int }


{-| 矢印の種類: 本文の `<<jump>>` / 選択肢の中の `<<jump>>`(選んだ時の行き先)。 -}
type LinkKind
    = JumpLink
    | OptionLink


{-| 1 本の矢印(行き先のノード・書いた行)。 -}
type alias Link =
    { target : String, kind : LinkKind, line : Int }


{-| ノード 1 つ(箱)。position はヘッダの `position: x,y`(書いていなければ Nothing)。 -}
type alias TreeNode =
    { title : String
    , file : String
    , line : Int
    , position : Maybe ( Float, Float )
    , links : List Link
    }


type alias Tree =
    { variables : List Variable, nodes : List TreeNode }


empty : Tree
empty =
    { variables = [], nodes = [] }


{-| シナリオと同じフォルダの木。 -}
treePathOf : String -> String
treePathOf path =
    case String.indexes "/" path |> List.reverse |> List.head of
        Just i ->
            String.left i path ++ "/dialogue.json"

        Nothing ->
            "dialogue.json"


{-| 値の字(始まりの引数 `$名前=値` と同じ書き方: 数は 1.5 の形・真偽は true / false・文字列はそのまま)。 -}
valueText : Value -> String
valueText value =
    case value of
        Number n ->
            String.fromFloat n

        Flag b ->
            if b then
                "true"

            else
                "false"

        Text s ->
            s


decoder : D.Decoder Tree
decoder =
    D.map2 Tree
        (D.oneOf [ D.field "variables" (D.list variableDecoder), D.succeed [] ])
        (D.oneOf [ D.field "nodes" (D.list nodeDecoder), D.succeed [] ])


variableDecoder : D.Decoder Variable
variableDecoder =
    D.map4 Variable
        (D.field "name" D.string)
        (D.field "value"
            (D.oneOf
                [ D.map Flag D.bool
                , D.map Number D.float
                , D.map Text D.string
                ]
            )
        )
        (D.oneOf [ D.field "file" D.string, D.succeed "" ])
        (D.oneOf [ D.field "line" D.int, D.succeed 0 ])


nodeDecoder : D.Decoder TreeNode
nodeDecoder =
    D.map5 TreeNode
        (D.field "title" D.string)
        (D.oneOf [ D.field "file" D.string, D.succeed "" ])
        (D.oneOf [ D.field "line" D.int, D.succeed 0 ])
        (D.oneOf [ D.field "headers" (D.list headerDecoder) |> D.map positionOf, D.succeed Nothing ])
        (D.oneOf [ D.field "body" (D.list (stmtLinks JumpLink)) |> D.map List.concat, D.succeed [] ])


headerDecoder : D.Decoder ( String, String )
headerDecoder =
    D.map2 Tuple.pair (D.field "key" D.string) (D.oneOf [ D.field "value" D.string, D.succeed "" ])


positionOf : List ( String, String ) -> Maybe ( Float, Float )
positionOf headers =
    headers
        |> List.filter (\( k, _ ) -> k == "position")
        |> List.head
        |> Maybe.andThen
            (\( _, v ) ->
                case String.split "," v |> List.map (String.trim >> String.toFloat) of
                    [ Just x, Just y ] ->
                        Just ( x, y )

                    _ ->
                        Nothing
            )


{-| 1 文の中の矢印(選択肢・if の中も深く見る)。kind は選択肢の中なら OptionLink。 -}
stmtLinks : LinkKind -> D.Decoder (List Link)
stmtLinks kind =
    D.field "kind" D.string
        |> D.andThen
            (\k ->
                case k of
                    "jump" ->
                        D.map2 (\target line -> [ { target = target, kind = kind, line = line } ])
                            (D.field "node" D.string)
                            (D.oneOf [ D.field "line" D.int, D.succeed 0 ])

                    "options" ->
                        D.oneOf
                            [ D.field "options" (D.list (bodyLinks OptionLink)) |> D.map List.concat
                            , D.succeed []
                            ]

                    "if" ->
                        D.oneOf
                            [ D.field "clauses" (D.list (bodyLinks kind)) |> D.map List.concat
                            , D.succeed []
                            ]

                    _ ->
                        D.succeed []
            )


bodyLinks : LinkKind -> D.Decoder (List Link)
bodyLinks kind =
    D.oneOf
        [ D.field "body" (D.list (D.lazy (\_ -> stmtLinks kind))) |> D.map List.concat
        , D.succeed []
        ]
