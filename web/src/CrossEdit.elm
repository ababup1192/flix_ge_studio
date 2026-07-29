module CrossEdit exposing
    ( FileEdits
    , Phase(..)
    , Run
    , doneText
    , editing
    , getting
    , putting
    , start
    , takeNext
    , tookFile
    )

{-| 開いていないファイルへの書き戻し(1 ファイルずつの直列)。

1 ファイルぶんは必ず「最新の本文を取る → jsonc の最小編集を当てる → 保存する」の
3 拍。開いてから時間が経っていても他所の編集を潰さないための取り直しで、
編集そのものは開いている文書と同じ applyDocEdits(既存の書き戻し経路)を使う。

ここは進行の台帳だけを持つ(封筒を投げるのは呼び側)。往復 1 回ごとに、
どの id の応答を待っているかを step に控えて突き合わせる — 追い越してきた
古い応答で先へ進まないため。

-}

import Edit


{-| 1 ファイルぶんの仕事。 -}
type alias FileEdits =
    { file : String
    , edits : List Edit.Payload
    }


{-| 待っている往復 1 つ。数字は封筒の id、後ろは報せに使う材料。 -}
type Phase
    = Getting Int String (List Edit.Payload)
    | Editing Int String Int
    | Putting Int String Int


type alias Run =
    { label : String
    , pending : List FileEdits
    , step : Maybe Phase
    , doneFiles : Int
    , doneEdits : Int
    }


start : String -> List FileEdits -> Run
start label files =
    { label = label, pending = files, step = Nothing, doneFiles = 0, doneEdits = 0 }


{-| 次のファイルを 1 件取り出す(空なら Nothing = 全部済んだ)。 -}
takeNext : Run -> Maybe ( FileEdits, Run )
takeNext run =
    case run.pending of
        next :: rest ->
            Just ( next, { run | pending = rest } )

        [] ->
            Nothing


getting : Int -> FileEdits -> Run -> Run
getting id file run =
    { run | step = Just (Getting id file.file file.edits) }


editing : Int -> String -> Int -> Run -> Run
editing id file count run =
    { run | step = Just (Editing id file count) }


putting : Int -> String -> Int -> Run -> Run
putting id file count run =
    { run | step = Just (Putting id file count) }


{-| 1 ファイル書き終えた。 -}
tookFile : Int -> Run -> Run
tookFile count run =
    { run | step = Nothing, doneFiles = run.doneFiles + 1, doneEdits = run.doneEdits + count }


{-| 全部済んだ時の報せ。 -}
doneText : Run -> String
doneText run =
    run.label
        ++ ": "
        ++ String.fromInt run.doneFiles
        ++ " ファイル "
        ++ String.fromInt run.doneEdits
        ++ " 箇所"
