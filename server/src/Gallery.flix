// Gallery — 描き出した絵 (gallery/ debug/) とリファレンス画像 (reference/) を読むだけの窓口。
// 一覧・画像・音・差分 (リファレンス画像とのバイト比較) をエディタ画面へ配る。
// 書き込みは一切しない。ディレクトリが無くても止めず、空の一覧で返す (fail-open)。

mod Gallery {
    import java.io.{File => JFile}
    import java.nio.file.Files
    import java.math.{BigDecimal => JBigDecimal}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber}

    /// 画像を配ってよいディレクトリの許可リスト。ここに無い dir 指定は断る。
    pub def imageDirs(): Set[String] = Set#{"gallery", "reference", "reference/archive", "debug"}

    /// ブラウザから来るファイル名の門番。区切りや `..` を含む名前はプロジェクトの
    /// 別の場所を指せてしまうので、名前 1 語だけを通す。
    pub def isSafeName(name: String): Bool =
        name != ""
            and not String.contains(substr = "/", name)
            and not String.contains(substr = "\\", name)
            and not String.contains(substr = "..", name)

    // ── 一覧 ──────────────────────────────────────────────

    /// GET /gallery/list の本文。gallery/ reference/ debug/ の PNG と
    /// debug/sounds/ の WAV を {name, mtime} の列で返す。
    pub def listJson(dir: String): Json \ IO =
        JObject(Map#{
            "gallery" => entriesJson(listNamed(suffix = ".png", dir, "gallery")),
            "reference" => entriesJson(listNamed(suffix = ".png", dir, "reference")),
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

    /// GET /gallery/image — dir 指定 (許可リスト) と安全な名前で PNG のバイト列を引く。
    /// 指定が不正・ファイルが無いは None (呼び手が 404 にする)。
    pub def imageBytes(dir: String, subdir: String, name: String): Option[Array[Int8, Static]] \ IO =
        mediaBytes(dir, subdir, name) |> Option.map(found -> snd(found))

    /// 出せる置き場か。定番の 4 つに加えて、焼き上がりの場面が住む
    /// debug/cutscene の下 (grid の定規つき画像・フレーム別 PNG) を許す。
    /// ".." を含む指定は通さない (プロジェクトの外を指せてしまうため)。
    def allowedDir(subdir: String): Bool =
        not String.contains(substr = "..", subdir)
            and (Set.memberOf(subdir, imageDirs())
                    or subdir == "debug/cutscene"
                    or String.startsWith(prefix = "debug/cutscene/", subdir))

    /// 絵 1 枚と、その種類 (content-type)。PNG と GIF (焼いた場面) を出す。
    pub def mediaBytes(dir: String, subdir: String, name: String): Option[(String, Array[Int8, Static])] \ IO =
        if (not allowedDir(subdir) or not isSafeName(name))
            None
        else if (String.endsWith(suffix = ".png", name))
            readBytes("${dir}/${subdir}/${name}") |> Option.map(bytes -> ("image/png", bytes))
        else if (String.endsWith(suffix = ".gif", name))
            readBytes("${dir}/${subdir}/${name}") |> Option.map(bytes -> ("image/gif", bytes))
        else
            None

    /// GET /gallery/count — 許可リストの置き場にある PNG の数。
    /// 「前回の焼き」を復元した時にフレーム数(pngFrames)を取り戻すのに使う
    /// (GIF の存在確認だけではフレーム送りの目盛りが引けない)。
    pub def pngCount(dir: String, subdir: String): Int32 \ IO =
        if (not allowedDir(subdir))
            0
        else {
            let entries = new JFile("${dir}/${subdir}").listFiles();
            if (Object.isNull(entries)) 0
            else Array.toList(entries)
                |> List.filter(file -> file.isFile()
                        and String.endsWith(suffix = ".png", file.getName()))
                |> List.length
        }

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

    // ── 差分 (gallery vs reference) ───────────────────────

    /// 1 枚ぶんの見比べ結果。make bench の cmp と同じで、1 バイトでも違えば diff。
    pub def diffStatus(inGallery: Bool, inReference: Bool, same: Bool): String =
        match (inGallery, inReference) {
            case (true, true)   => if (same) "ok" else "diff"
            case (true, false)  => "missingReference"
            case (false, true)  => "missingGallery"
            case (false, false) => "missingGallery"
        }

    /// gallery/ と reference/ の名前の和集合を取り、名前ごとの status を並べる (名前順)。
    /// `same` はバイト一致の判定 (テストでは差し替えられる)。
    pub def diffItems(galleryNames: List[String], referenceNames: List[String],
                      same: String -> Bool \ ef): List[(String, String)] \ ef =
        List.append(galleryNames, referenceNames)
            |> List.distinct
            |> List.sort
            |> List.map(name -> {
                let inGallery = List.memberOf(name, galleryNames);
                let inReference = List.memberOf(name, referenceNames);
                let sameBytes = if (inGallery and inReference) same(name) else false;
                (name, diffStatus(inGallery, inReference, sameBytes))
            })

    /// ディスク上の gallery/ reference/ を見比べる。Changes の自動追随と NextStep が使う。
    pub def diffItemsOf(dir: String): List[(String, String)] \ IO =
        let galleryNames = listNamed(suffix = ".png", dir, "gallery") |> List.map(entry -> entry#name);
        let referenceNames = listNamed(suffix = ".png", dir, "reference") |> List.map(entry -> entry#name);
        diffItems(galleryNames, referenceNames, name -> sameBytes(dir, name))

    /// gallery/name と reference/name のバイト一致。どちらか読めなければ不一致扱い。
    def sameBytes(dir: String, name: String): Bool \ IO =
        match (readBytes("${dir}/gallery/${name}"), readBytes("${dir}/reference/${name}")) {
            case (Some(a), Some(b)) => Array.sameElements(a, b)
            case _                  => false
        }

    /// gallery/ の PNG の mtime 一覧 (NextStep の鮮度判定に使う)。
    pub def galleryMtimes(dir: String): List[Int64] \ IO =
        listNamed(suffix = ".png", dir, "gallery") |> List.map(entry -> entry#mtime)
}
