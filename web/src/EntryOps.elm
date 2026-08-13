module EntryOps exposing
    ( Op(..)
    , RowEdit(..)
    , addCatalogOp
    , addListOp
    , addProblem
    , deleteOp
    , duplicateOp
    , fieldValue
    , freshId
    , listRowOp
    , newEntry
    )

{-| エントリの追加・複製・削除(テーブル上部の CRUD)が元データへ流す編集の導出。

ここは純ロジック(Html も Effect も作らない)。「どの操作がどの編集になるか」を
view に散らさず 1 箇所に集め、elm-test で固定する。パスの語彙は Refs.PathSeg を
そのまま使う — 編集のパス型を 3 つ目に増やさないため。

-}

import Dict
import Doc
import Json.Decode as D
import Json.Encode as E
import Refs exposing (Entry(..), PathSeg(..))
import Schema exposing (Field, FieldType(..), Section)


{-| 元データ(jsonc テキスト)への編集 1 件。
Set=キーへの書き込み(catalog の追加・複製)・Append=配列末尾へ挿入(list)・
Remove=キー/要素の削除。
-}
type Op
    = SetAt (List PathSeg) E.Value
    | AppendAt (List PathSeg) E.Value
    | RemoveAt (List PathSeg)


{-| catalog へ id 付きで雛形を挿す。挿入位置は jsonc 編集がオブジェクト末尾に
落とすので、ここではパスと値だけ決める。
-}
addCatalogOp : String -> Section -> String -> Op
addCatalogOp sectionKey section id =
    SetAt [ Key sectionKey, Key id ] (newEntry section)


addListOp : String -> Section -> Op
addListOp sectionKey section =
    AppendAt [ Key sectionKey ] (newEntry section)


{-| 選択エントリのコピー。catalog は空いている id を作って挿し、list は元の行の直後へ。
select は複製後に選ばせたい行(呼び側が entrySel へ写す)。
エントリが文書に無い(テキスト側で消えた直後等)なら Nothing。
-}
duplicateOp : String -> D.Value -> Entry -> Maybe { op : Op, select : Entry }
duplicateOp sectionKey doc entry =
    case entry of
        AtKey name ->
            Doc.catalog sectionKey doc
                |> Dict.get name
                |> Maybe.map
                    (\value ->
                        let
                            newId =
                                freshId (Doc.catalogKeys sectionKey doc) name
                        in
                        { op = SetAt [ Key sectionKey, Key newId ] value
                        , select = AtKey newId
                        }
                    )

        AtIndex i ->
            listRowOp sectionKey doc (DuplicateRow i)


{-| 一覧(list)セクションの行そのものへの操作。行の入れ替え(dir = -1 で上へ・
+1 で下へ)と、選んだ行の直後への複製。
-}
type RowEdit
    = MoveRow Int Int
    | DuplicateRow Int


{-| 行操作の書き戻しは「その配列を丸ごと 1 本の Set で書く」に畳む —
挿入や入れ替えを添字ごとの編集に散らすと、途中状態の文書が画面に出る。
select は操作のあと選ばせたい行(動かした行・複製された行)。
範囲の外を指す操作は Nothing(表示と文書がずれた瞬間のクリックで壊さない)。
-}
listRowOp : String -> D.Value -> RowEdit -> Maybe { op : Op, select : Entry }
listRowOp sectionKey doc edit =
    let
        items =
            Doc.list sectionKey doc

        write newItems =
            SetAt [ Key sectionKey ] (E.list identity newItems)
    in
    case edit of
        MoveRow index dir ->
            let
                other =
                    index + dir
            in
            if index < 0 || other < 0 || index >= List.length items || other >= List.length items then
                Nothing

            else
                Just
                    { op = write (swapped index other items)
                    , select = AtIndex other
                    }

        DuplicateRow index ->
            items
                |> List.drop index
                |> List.head
                |> Maybe.map
                    (\value ->
                        { op = write (insertedAfter index value items)
                        , select = AtIndex (index + 1)
                        }
                    )


swapped : Int -> Int -> List E.Value -> List E.Value
swapped a b items =
    let
        at i =
            items |> List.drop i |> List.head
    in
    case ( at a, at b ) of
        ( Just va, Just vb ) ->
            items
                |> List.indexedMap
                    (\i v ->
                        if i == a then
                            vb

                        else if i == b then
                            va

                        else
                            v
                    )

        _ ->
            items


insertedAfter : Int -> E.Value -> List E.Value -> List E.Value
insertedAfter index value items =
    items
        |> List.indexedMap
            (\i v ->
                if i == index then
                    [ v, value ]

                else
                    [ v ]
            )
        |> List.concat


deleteOp : String -> Entry -> Op
deleteOp sectionKey entry =
    case entry of
        AtKey name ->
            RemoveAt [ Key sectionKey, Key name ]

        AtIndex i ->
            RemoveAt [ Key sectionKey, Idx i ]


{-| 追加を拒む理由(Nothing = 通してよい)。Refs.renameProblem と同じ流儀で、
黙って捨てずに理由を返す。
-}
addProblem : List String -> String -> Maybe String
addProblem existing rawId =
    let
        id =
            String.trim rawId
    in
    if id == "" then
        Just "id が空です"

    else if List.member id existing then
        Just ("\"" ++ id ++ "\" は既にあります")

    else
        Nothing


{-| 複製に使う空き id。"name_copy" から始めて埋まっていれば番号を足す。 -}
freshId : List String -> String -> String
freshId existing base =
    let
        candidate =
            base ++ "_copy"
    in
    if List.member candidate existing then
        freshIdNumbered existing base 2

    else
        candidate


freshIdNumbered : List String -> String -> Int -> String
freshIdNumbered existing base n =
    let
        candidate =
            base ++ "_copy" ++ String.fromInt n
    in
    if List.member candidate existing then
        freshIdNumbered existing base (n + 1)

    else
        candidate


{-| セクションの雛形 1 エントリ。default があればそれ、無ければ type ごとの零値。 -}
newEntry : Section -> E.Value
newEntry section =
    E.object (section.fields |> List.map (\( name, field ) -> ( name, fieldValue field )))


{-| 欄 1 つの既定値。default があればそれ、無ければ type ごとの零値。 -}
fieldValue : Field -> E.Value
fieldValue field =
    case field.default of
        Just value ->
            value

        Nothing ->
            zeroValue field.type_


{-| enum は先頭の選択肢(存在しない値を作らない)。ref は空文字 — 参照先を勝手に
選ばず、未設定として問題パネル/フォームに見えるままにする。
custom は零の形をこちらで決めない(null で見えるまま。weights 等は schema の
default に任せる — null の下へは jsonc の最小編集が刺さらない)。
-}
zeroValue : FieldType -> E.Value
zeroValue type_ =
    case type_ of
        TText ->
            E.string ""

        TInt ->
            E.int 0

        TFloat ->
            E.float 0

        TBool ->
            E.bool False

        TEnum (first :: _) ->
            E.string first

        TEnum [] ->
            E.string ""

        TRef _ ->
            E.string ""

        TVec2 ->
            E.object [ ( "x", E.int 0 ), ( "y", E.int 0 ) ]

        TColor ->
            E.string "#ffffff"

        TTexture ->
            E.string ""

        TList _ ->
            E.list identity []

        TRecord fields ->
            E.object (fields |> List.map (\( name, field ) -> ( name, fieldValue field )))

        _ ->
            E.null
