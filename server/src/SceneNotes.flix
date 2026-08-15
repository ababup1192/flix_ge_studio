// SceneNotes — 場面の説明 Doc (assets/*.scenes.json) を読むだけの層。
// 「どの場面を描くか」を決めるのはゲームの SceneRender.shotNames() で、この Doc は
// 描き上がった絵に題と一言のラベルを添えるだけ。ラベルが無くてもギャラリーは名前で出るので、
// 読めない・無い・形が違うはすべて空の Map へ落とす (fail-open)。

mod SceneNotes {
    use Util.Json.Json
    use Util.Json.Json.JArray

    /// 場面 1 つぶんのラベル。既定値はここが持つ (Doc 側には書かない)。
    pub type alias Note = { title = String, desc = String, tags = List[String] }

    /// 説明がまだ無い場面のラベル。
    pub def defaultNote(): Note = { title = "", desc = "", tags = Nil }

    /// Doc の置き場。1 ゲームに 1 枚を想定するが、複数あれば名前順で先勝ち。
    pub def docPattern(): String = "assets/*.scenes.json"

    // ── 純粋な読み取り ───────────────────────────────────

    /// Doc の中身 → 場面名 (拡張子なし) → ラベル。JSON として読めない・rows が
    /// 無い・rows が配列でないは空の Map。
    pub def fromText(text: String): Map[String, Note] =
        match DocJson.parse(text) {
            case None       => Map.empty()
            case Some(json) => rowsOf(json) |> List.filterMap(noteOf) |> List.toMap
        }

    /// rows のオブジェクトだけを取り出す。形が違う要素は落とす。
    def rowsOf(json: Json): List[Map[String, Json]] =
        let rows = JsonCodec.expectObject(json) |> Option.flatMap(obj -> Map.get("rows", obj));
        match rows {
            case Some(JArray(items)) =>
                Vector.toList(items) |> List.filterMap(JsonCodec.expectObject)
            case _ => Nil
        }

    /// rows の 1 要素 → (場面名, ラベル)。name が無い行はラベルとして使えないので落とす。
    def noteOf(row: Map[String, Json]): Option[(String, Note)] =
        let name = JsonCodec.stringOr("name", "", row);
        if (name == "")
            None
        else
            Some((name, {
                title = JsonCodec.stringOr("title", defaultNote()#title, row),
                desc  = JsonCodec.stringOr("desc", defaultNote()#desc, row),
                tags  = tagsOf(row)
            }))

    /// tags は文字列の配列。配列でない・要素が文字列でないは黙って落とす。
    def tagsOf(row: Map[String, Json]): List[String] =
        match Map.get("tags", row) {
            case Some(JArray(items)) =>
                Vector.toList(items) |> List.filterMap(JsonCodec.expectString)
            case _ => Nil
        }

    // ── ディスクから ─────────────────────────────────────

    /// プロジェクトの assets/*.scenes.json を読む。無ければ空の Map。
    /// ファイル探しは宣言の照合と同じ規則 (ResourceDecl.matchFiles) に乗せる。
    pub def load(dir: String): Map[String, Note] \ IO =
        ResourceDecl.matchFiles(docPattern(), FileIndex.listAllJson(dir))
            |> List.sort
            |> List.map(path -> match FileIndex.readInside(dir, path) {
                case Ok(text) => fromText(text)
                case Err(_)   => Map.empty()
            })
            |> List.foldLeft((acc, notes) -> Map.union(acc, notes), Map.empty())
}
