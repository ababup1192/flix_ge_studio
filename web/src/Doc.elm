module Doc exposing (catalog, catalogKeys, list, record)

{-| 開いている文書(パース済み JSON)からセクションの中身を引く純関数。

セクションが無い/形が違うときは空(空 Dict・空リスト・null)に倒す —
書きかけの文書でフォームを即死させないため。unknown key の検出はここではやらない。

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E


catalog : String -> D.Value -> Dict String D.Value
catalog key doc =
    Dict.fromList (entryPairs key doc)


{-| catalog のエントリ名を文書に書いてある順で。Dict.keys だと辞書順に化けるので別口。 -}
catalogKeys : String -> D.Value -> List String
catalogKeys key doc =
    List.map Tuple.first (entryPairs key doc)


entryPairs : String -> D.Value -> List ( String, D.Value )
entryPairs key doc =
    D.decodeValue (D.field key (D.keyValuePairs D.value)) doc
        |> Result.withDefault []
        -- 文書側も "//" キーは注釈(スキーマと同じ流儀)
        |> List.filter (\( name, _ ) -> not (String.startsWith "//" name))


list : String -> D.Value -> List D.Value
list key doc =
    D.decodeValue (D.field key (D.list D.value)) doc
        |> Result.withDefault []


record : String -> D.Value -> D.Value
record key doc =
    D.decodeValue (D.field key D.value) doc
        |> Result.withDefault E.null
