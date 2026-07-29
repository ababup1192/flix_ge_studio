// Golden — 焼き直した絵と音が「前と同じか」を見張る窓口。
//
// 有効かどうかはファイルの決まりだけで判じる: プロジェクトに golden/ があれば
// この機能が生きる。ゲームの言葉(場面名・題材)は一切見ない。
//
// 見比べるのは 2 組:
//   golden/*.png      ↔ gallery/*.png     (焼いた絵)
//   golden/sfx/*.wav  ↔ assets/sfx/*.wav  (焼いた音)
//
// 「割れて何日目か」を言えるように、割れを最初に見つけた時刻だけを
// .studio/golden-diff.json に控える。控えが無い時の初回は「今」。
// 記録が消えても困らない(日数が 0 日からになるだけ)。
mod Golden {
    import java.io.{File => JFile}
    import java.lang.Exception
    import java.lang.System
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JArray, JObject, JString, JBool, JNumber}

    /// 見張りの記録の置き場(プロジェクトの中の小さな控え)。
    def recordPath(): String = ".studio/golden-diff.json"

    /// GET /golden/status — 一覧と、割れの件数。
    pub def status(dir: String): (Int32, Json) \ IO =
        if (not new JFile("${dir}/golden").isDirectory())
            (200, JObject(Map#{"enabled" => JBool(false), "items" => JArray(Vector#{})}))
        else {
            let items = imageItems(dir) ::: soundItems(dir);
            let broken = items |> List.filter(item -> not item#same) |> List.map(item -> item#name);
            let seen = rememberBroken(dir, broken);
            (200, JObject(Map#{
                "enabled" => JBool(true),
                // 「割れて何日目か」は (now - since) で画面が出す。時計はサーバの物 1 つに揃える
                "now"     => JNumber(JsonCodec.intToBd(seconds(System.currentTimeMillis()))),
                "total"   => JNumber(JsonCodec.intToBd(List.length(items))),
                "broken"  => JNumber(JsonCodec.intToBd(List.length(broken))),
                "items"   => JArray(items |> List.map(item -> itemJson(item, seen)) |> List.toVector)
            }))
        }

    /// POST /golden/bless {name} — 今の焼き上がりを新しい正にする。
    pub def bless(dir: String, body: String): (Int32, Json) \ IO =
        match (JsonCompat.parse(body)
                |> Option.flatMap(JsonCodec.expectObject)
                |> Option.flatMap(obj -> Map.get("name", obj))
                |> Option.flatMap(JsonCodec.expectString)) {
            case None => (400, errorJson("body must be a JSON object {name}"))
            case Some(name) =>
                if (not Gallery.isSafeName(name))
                    (400, errorJson("bad name: ${name}"))
                else {
                    let (baked, golden) =
                        if (String.endsWith(suffix = ".wav", name))
                            ("assets/sfx/${name}", "golden/sfx/${name}")
                        else
                            ("gallery/${name}", "golden/${name}");
                    match copyInside(dir, baked, golden) {
                        case Err(message) => (400, errorJson(message))
                        case Ok(_)        => {
                            forget(dir, name);
                            (200, JObject(Map#{"ok" => JBool(true), "name" => JString(name)}))
                        }
                    }
                }
        }

    // ── 見比べ ────────────────────────────────────────────

    pub type alias Item = {name = String, kind = String, same = Bool, goldenMtime = Int64}

    def imageItems(dir: String): List[Item] \ IO =
        namesIn(dir, "golden", ".png")
            |> List.map(name -> ({
                name = name,
                kind = "image",
                same = sameBytes(dir, "golden/${name}", "gallery/${name}"),
                goldenMtime = mtimeOf(dir, "golden/${name}")
            }: Item))

    def soundItems(dir: String): List[Item] \ IO =
        namesIn(dir, "golden/sfx", ".wav")
            |> List.map(name -> ({
                name = name,
                kind = "sound",
                same = sameBytes(dir, "golden/sfx/${name}", "assets/sfx/${name}"),
                goldenMtime = mtimeOf(dir, "golden/sfx/${name}")
            }: Item))

    /// その置き場の、拡張子が合うファイル名(名前順)。
    def namesIn(dir: String, subdir: String, suffix: String): List[String] \ IO =
        let files = new JFile("${dir}/${subdir}").listFiles();
        if (Object.isNull(files)) Nil
        else
            Array.toList(files)
                |> List.filter(file -> file.isFile())
                |> List.map(file -> file.getName())
                |> List.filter(name -> String.endsWith(suffix = suffix, name))
                |> List.sort

    /// 中身がバイトまで同じか。片方が無ければ「割れている」。
    def sameBytes(dir: String, left: String, right: String): Bool \ IO =
        match (readBytes(dir, left), readBytes(dir, right)) {
            case (Some(a), Some(b)) =>
                Array.length(a) == Array.length(b)
                    and (List.range(0, Array.length(a))
                            |> List.forAll(i -> Array.get(i, a) == Array.get(i, b)))
            case _ => false
        }

    def readBytes(dir: String, relPath: String): Option[Array[Int8, Static]] \ IO =
        try {
            let file = new JFile("${dir}/${relPath}");
            if (file.isFile()) Some(Files.readAllBytes(file.toPath())) else None
        } catch {
            case _: Exception => None
        }

    def mtimeOf(dir: String, relPath: String): Int64 \ IO =
        let file = new JFile("${dir}/${relPath}");
        if (file.isFile()) file.lastModified() else 0i64

    /// 焼き上がりを正へ写す(どちらもプロジェクトの中に限る)。
    def copyInside(dir: String, bakedRel: String, goldenRel: String): Result[String, Unit] \ IO =
        match FileIndex.resolveInside(dir, goldenRel) {
            case None => Err("path is outside the project")
            case Some(dst) =>
                match readBytes(dir, bakedRel) {
                    case None => Err("not baked yet: ${bakedRel}")
                    case Some(bytes) =>
                        try {
                            discard new JFile(dst).getParentFile().mkdirs();
                            discard Files.write(new JFile(dst).toPath(), bytes);
                            Ok(())
                        } catch {
                            case _: Exception => Err("copy failed: ${bakedRel}")
                        }
                }
        }

    // ── 「割れて何日目か」の控え ──────────────────────────

    /// 割れている物の「最初に見つけた時刻」。初めて見た物は今の時刻で控える。
    /// 直った物は控えから落とす(次に割れたらまた 0 日目から)。
    def rememberBroken(dir: String, broken: List[String]): Map[String, Int32] \ IO =
        let now = seconds(System.currentTimeMillis());
        let before = readRecord(dir);
        let kept = broken
            |> List.map(name -> (name, Map.getWithDefault(name, now, before)))
            |> List.toMap;
        if (kept != before) writeRecord(dir, kept) else ();
        kept

    def forget(dir: String, name: String): Unit \ IO =
        let before = readRecord(dir);
        if (Map.memberOf(name, before)) writeRecord(dir, Map.remove(name, before)) else ()

    def readRecord(dir: String): Map[String, Int32] \ IO =
        match FileIndex.readInside(dir, recordPath()) {
            case Err(_)   => Map#{}
            case Ok(text) => match (JsonCompat.parse(text) |> Option.flatMap(JsonCodec.expectObject)) {
                case None      => Map#{}
                case Some(obj) => obj |> Map.mapWithKey((_, value) ->
                        JsonCodec.expectInt(value) |> Option.getWithDefault(0))
            }
        }

    def writeRecord(dir: String, marks: Map[String, Int32]): Unit \ IO =
        let json = JObject(marks |> Map.mapWithKey((_, at) -> JNumber(JsonCodec.intToBd(at))));
        discard new JFile("${dir}/.studio").mkdirs();
        discard FileIndex.writeInside(str = Util.Json.toPrettyString(0, json), dir, recordPath())

    // ── 応答 ──────────────────────────────────────────────

    def itemJson(item: Item, seen: Map[String, Int32]): Json =
        let base = Map#{
            "name"        => JString(item#name),
            "kind"        => JString(item#kind),
            "match"       => JBool(item#same),
            "goldenMtime" => JNumber(JsonCodec.intToBd(seconds(item#goldenMtime)))
        };
        match Map.get(item#name, seen) {
            case Some(at) => Map.insert("since", JNumber(JsonCodec.intToBd(at)), base) |> JObject
            case None     => JObject(base)
        }

    /// ミリ秒 → 秒(JSON は秒で持つ。画面が要るのは「何日目か」だけ)。
    def seconds(ms: Int64): Int32 =
        Int64.toFloat64(ms / 1000i64) |> Float64.tryToInt32 |> Option.getWithDefault(0)

    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
