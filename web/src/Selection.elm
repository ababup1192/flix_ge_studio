module Selection exposing
    ( EntrySel(..)
    , catalogName
    , fromRefsEntry
    , listEntry
    , mapTarget
    , toRefsEntry
    )

{-| 「今どのエントリを選んでいるか」の解決(純ロジック)。

選択は表示と独立に生きる — 絞り込みで行が隠れても、並べ替えで順序が変わっても
指し先は変わらない。逆に、テキスト側の編集で消えたエントリを指したままの選択は
「無効」に倒す。その判定を view に散らさず、ここ 1 か所で行う。

-}

import Doc
import Edit exposing (Seg(..))
import Json.Decode as D
import Refs
import Schema


{-| catalog は名前・list は添字でエントリを指す。 -}
type EntrySel
    = ByKey String
    | ByIndex Int


{-| 選択中の catalog エントリ名。テキスト側の編集で消えた後の選択は無効。 -}
catalogName : Maybe EntrySel -> String -> D.Value -> Maybe String
catalogName sel key doc =
    case sel of
        Just (ByKey name) ->
            if List.member name (Doc.catalogKeys key doc) then
                Just name

            else
                Nothing

        _ ->
            Nothing


listEntry : Maybe EntrySel -> String -> D.Value -> Maybe ( Int, D.Value )
listEntry sel key doc =
    case sel of
        Just (ByIndex i) ->
            Doc.list key doc |> List.drop i |> List.head |> Maybe.map (\entry -> ( i, entry ))

        _ ->
            Nothing


toRefsEntry : EntrySel -> Refs.Entry
toRefsEntry sel =
    case sel of
        ByKey name ->
            Refs.AtKey name

        ByIndex i ->
            Refs.AtIndex i


fromRefsEntry : Refs.Entry -> EntrySel
fromRefsEntry entry =
    case entry of
        Refs.AtKey name ->
            ByKey name

        Refs.AtIndex i ->
            ByIndex i


{-| 盤面(マップ)の選択(キーと添字)から、フォームに要る 4 つ組を解く。
配列は該当添字の要素、単体(soul 型)はそのオブジェクト。書き戻し先の path も
ここで決める。セクションの引き当ては呼び側(スキーマを持つ側)の仕事。
-}
mapTarget :
    Maybe Schema.Section
    -> D.Value
    -> String
    -> Maybe Int
    -> Maybe { title : String, section : Schema.Section, entry : D.Value, path : List Seg }
mapTarget section doc key index =
    case ( section, index ) of
        ( Just sec, Just i ) ->
            Doc.list key doc
                |> List.drop i
                |> List.head
                |> Maybe.map
                    (\entry ->
                        { title = key ++ " #" ++ String.fromInt i
                        , section = sec
                        , entry = entry
                        , path = [ KeySeg key, IdxSeg i ]
                        }
                    )

        ( Just sec, Nothing ) ->
            Just
                { title = key
                , section = sec
                , entry = Doc.record key doc
                , path = [ KeySeg key ]
                }

        _ ->
            Nothing
