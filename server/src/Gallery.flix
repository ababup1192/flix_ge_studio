// Gallery — 焼き上がった絵 (gallery/ debug/) とお手本 (golden/) を読むだけの窓口。
// 一覧・画像・音・差分 (golden とのバイト比較) をエディタ画面へ配る。
// 書き込みは一切しない。ディレクトリが無くても止めず、空の一覧で返す (fail-open)。

mod Gallery {
    import java.io.{File => JFile}
    import java.nio.file.Files
    import java.math.{BigDecimal => JBigDecimal}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber}

    /// 画像を配ってよいディレクトリの白名前簿。ここに無い dir 指定は断る。
    pub def imageDirs(): Set[String] = Set#{"gallery", "golden", "debug"}

    /// ブラウザから来るファイル名の門番。区切りや `..` を含む名前はプロジェクトの
    /// 別の場所を指せてしまうので、名前 1 語だけを通す。
    pub def isSafeName(name: String): Bool =
        name != ""
            and not String.contains(substr = "/", name)
            and not String.contains(substr = "\\", name)
            and not String.contains(substr = "..", name)

    // ── 一覧 ──────────────────────────────────────────────

    /// GET /gallery/list の本文。gallery/ golden/ debug/ の PNG と
    /// debug/sounds/ の WAV を {name, mtime} の列で返す。
    pub def listJson(dir: String): Json \ IO =
        JObject(Map#{
            "gallery" => entriesJson(listNamed(suffix = ".png", dir, "gallery")),
            "golden"  => entriesJson(listNamed(suffix = ".png", dir, "golden")),
            "debug"   => entriesJson(listNamed(suffix = ".png", dir, "debug")),
            "sounds"  => entriesJson(listNamed(suffix = ".wav", dir, "debug/sounds"))
        })

    /// dir/subdir 直下から拡張子一致のファイルを {name, mtime} で集める (名前順)。
    /// ディレクトリが無い・読めないは空の一覧。
    def listNamed(suffix: {suffix = String}, dir: String, subdir: String): List[{name = String, mtime = Int64}] \ IO =
        let entries = new JFile("${dir}/${subdir}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(file -> file.isFile() and String.endsWith(suffix = suffix#suffix, file.getName()))
            |> List.map(file -> {name = file.getName(), mtime = file.lastModified()})
            |> List.sortBy(entry -> entry#name)

    /// {name, mtime} の列を JSON 配列へ。
    def entriesJson(entries: List[{name = String, mtime = Int64}]): Json =
        JArray(entries
            |> List.map(entry -> JObject(Map#{
                "name"  => JString(entry#name),
                "mtime" => JNumber(JBigDecimal.valueOf(entry#mtime))
            }))
            |> List.toVector)

    // ── 画像・音のバイト列 ────────────────────────────────

    /// GET /gallery/image — dir 指定 (白名前簿) と安全な名前で PNG のバイト列を引く。
    /// 指定が不正・ファイルが無いは None (呼び手が 404 にする)。
    pub def imageBytes(dir: String, subdir: String, name: String): Option[Array[Int8, Static]] \ IO =
        if (not Set.memberOf(subdir, imageDirs()) or not isSafeName(name)
                or not String.endsWith(suffix = ".png", name))
            None
        else
            readBytes("${dir}/${subdir}/${name}")

    /// GET /gallery/sound — debug/sounds/ の WAV のバイト列。
    pub def soundBytes(dir: String, name: String): Option[Array[Int8, Static]] \ IO =
        if (not isSafeName(name) or not String.endsWith(suffix = ".wav", name))
            None
        else
            readBytes("${dir}/debug/sounds/${name}")

    /// ファイルをバイト列で読む。無い・読めないは None。
    def readBytes(path: String): Option[Array[Int8, Static]] \ IO =
        let file = new JFile(path);
        if (file.isFile()) Some(Files.readAllBytes(file.toPath())) else None

    // ── 差分 (gallery vs golden) ──────────────────────────

    /// 1 枚ぶんの見比べ結果。make bench の cmp と同じで、1 バイトでも違えば diff。
    pub def diffStatus(inGallery: Bool, inGolden: Bool, same: Bool): String =
        match (inGallery, inGolden) {
            case (true, true)   => if (same) "ok" else "diff"
            case (true, false)  => "missingGolden"
            case (false, true)  => "missingGallery"
            case (false, false) => "missingGallery"
        }

    /// gallery/ と golden/ の名前の和集合を取り、名前ごとの status を並べる (名前順)。
    /// `same` はバイト一致の判定 (テストでは差し替えられる)。
    pub def diffItems(galleryNames: List[String], goldenNames: List[String],
                      same: String -> Bool \ ef): List[(String, String)] \ ef =
        List.append(galleryNames, goldenNames)
            |> List.distinct
            |> List.sort
            |> List.map(name -> {
                let inGallery = List.memberOf(name, galleryNames);
                let inGolden = List.memberOf(name, goldenNames);
                let sameBytes = if (inGallery and inGolden) same(name) else false;
                (name, diffStatus(inGallery, inGolden, sameBytes))
            })

    /// GET /gallery/diff の本文。
    pub def diffJson(dir: String): Json \ IO =
        let items = diffItemsOf(dir) |> List.map(item -> {
            let (name, status) = item;
            JObject(Map#{"name" => JString(name), "status" => JString(status)})
        });
        JObject(Map#{"items" => JArray(List.toVector(items))})

    /// ディスク上の gallery/ golden/ を見比べる。diff の枚数は Journey も使う。
    pub def diffItemsOf(dir: String): List[(String, String)] \ IO =
        let galleryNames = listNamed(suffix = ".png", dir, "gallery") |> List.map(entry -> entry#name);
        let goldenNames = listNamed(suffix = ".png", dir, "golden") |> List.map(entry -> entry#name);
        diffItems(galleryNames, goldenNames, name -> sameBytes(dir, name))

    /// gallery/name と golden/name のバイト一致。どちらか読めなければ不一致扱い。
    def sameBytes(dir: String, name: String): Bool \ IO =
        match (readBytes("${dir}/gallery/${name}"), readBytes("${dir}/golden/${name}")) {
            case (Some(a), Some(b)) => Array.sameElements(a, b)
            case _                  => false
        }

    /// gallery/ の PNG の mtime 一覧 (Journey の鮮度判定に使う)。
    pub def galleryMtimes(dir: String): List[Int64] \ IO =
        listNamed(suffix = ".png", dir, "gallery") |> List.map(entry -> entry#mtime)
}
