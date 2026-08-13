// FileVerbs — ファイルそのものへの動詞 (新規 / 複製 / 名前を変える / 消す)。
// 中身の編集は PUT /file の担当で、こちらは「ファイルの出し入れ」だけを扱う。
//
// パスはすべて FileIndex.resolveInside の門番を通す。ブラウザから来た文字列で
// プロジェクトの外を触らせない — 動詞が増えるほど、門番を通し忘れた 1 本が
// 命取りになるので、開く・書く・消すのどれもここ 1 か所に集める。
mod FileVerbs {
    import java.io.{File => JFile}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool}

    /// POST /file/new — 中身付きで新しいファイルを作る。既にあれば作らない
    /// (上書きは「新規」ではない — 既存の作り込みを黙って消さない)。
    pub def create(dir: String, body: String): (Int32, Json) \ IO =
        match twoFields("path", "content", body) {
            case None => (400, errorJson("body must be a JSON object {path, content}"))
            case Some((relPath, content)) =>
                if (exists(dir, relPath))
                    (409, errorJson("already exists: ${relPath}"))
                else
                    written(dir, relPath, content)
        }

    /// POST /file/duplicate — {path, toPath}。中身を読んで別名で作る。
    pub def duplicate(dir: String, body: String): (Int32, Json) \ IO =
        match twoFields("path", "toPath", body) {
            case None => (400, errorJson("body must be a JSON object {path, toPath}"))
            case Some((fromRel, toRel)) =>
                if (exists(dir, toRel))
                    (409, errorJson("already exists: ${toRel}"))
                else match FileIndex.readInside(dir, fromRel) {
                    case Err(message) => (404, errorJson(message))
                    case Ok(content)  => written(dir, toRel, content)
                }
        }

    /// POST /file/rename — {path, toPath}。名前を変えるだけで、中身も参照も触らない
    /// (この名前を指しているレジストリの書き換えは別の仕事)。
    pub def rename(dir: String, body: String): (Int32, Json) \ IO =
        match twoFields("path", "toPath", body) {
            case None => (400, errorJson("body must be a JSON object {path, toPath}"))
            case Some((fromRel, toRel)) =>
                if (exists(dir, toRel))
                    (409, errorJson("already exists: ${toRel}"))
                else match (FileIndex.resolveInside(dir, fromRel), FileIndex.resolveInside(dir, toRel)) {
                    case (Some(fromFull), Some(toFull)) =>
                        let src = new JFile(fromFull);
                        if (not src.isFile())
                            (404, errorJson("not found: ${fromRel}"))
                        else if (src.renameTo(new JFile(toFull)))
                            (200, JObject(Map#{"ok" => JBool(true), "path" => JString(toRel)}))
                        else
                            (400, errorJson("rename failed: ${fromRel}"))

                    case _ => (400, errorJson("path is outside the project"))
                }
        }

    /// POST /file/delete — {path}。消えた事実だけを返す (元に戻すは持たない)。
    pub def delete(dir: String, body: String): (Int32, Json) \ IO =
        match (oneField("path", body)) {
            case None => (400, errorJson("body must be a JSON object {path}"))
            case Some(relPath) => match FileIndex.resolveInside(dir, relPath) {
                case None => (400, errorJson("path is outside the project: ${relPath}"))
                case Some(fullPath) =>
                    let file = new JFile(fullPath);
                    if (not file.isFile())
                        (404, errorJson("not found: ${relPath}"))
                    else if (file.delete())
                        (200, JObject(Map#{"ok" => JBool(true), "path" => JString(relPath)}))
                    else
                        (400, errorJson("delete failed: ${relPath}"))
            }
        }

    /// 書き込みの結果を応答へ。書けない理由 (プロジェクト外・親が無い) は 400。
    def written(dir: String, relPath: String, content: String): (Int32, Json) \ IO =
        match FileIndex.writeInside(str = content, dir, relPath) {
            case Err(message) => (400, errorJson(message))
            case Ok(_)        => (200, JObject(Map#{"ok" => JBool(true), "path" => JString(relPath)}))
        }

    /// 門番を通したうえでの在り無し。門番に弾かれたパスは「ある」と答える —
    /// 触れない場所へ作りに行かせない (作成・改名の手前で必ず止まる)。
    def exists(dir: String, relPath: String): Bool \ IO =
        match FileIndex.resolveInside(dir, relPath) {
            case None           => true
            case Some(fullPath) => new JFile(fullPath).exists()
        }

    def oneField(name: String, body: String): Option[String] =
        DocJson.parse(body)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> Map.get(name, obj))
            |> Option.flatMap(JsonCodec.expectString)

    def twoFields(first: String, second: String, body: String): Option[(String, String)] =
        DocJson.parse(body)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> forM (
                a <- Map.get(first, obj) |> Option.flatMap(JsonCodec.expectString);
                b <- Map.get(second, obj) |> Option.flatMap(JsonCodec.expectString)
            ) yield (a, b))

    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
