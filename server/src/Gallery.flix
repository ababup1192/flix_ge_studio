// Gallery — 描き出した絵 (gallery/ debug/) とリファレンス画像 (reference/) を読むだけのウィンドウ口。
// 一覧・画像・音・差分 (リファレンス画像とのバイト比較) をエディタ画面へ配る。
// 場面の題と一言は SceneNotes (assets/*.scenes.json) から借りて添えるだけで、
// ここが場面の意味を決めることはない。
// 書き込みは一切しない。ディレクトリが無くても止めず、空の一覧で返す (fail-open)。

mod Gallery {
    import java.io.{File => JFile}
    import java.nio.file.Files
    import java.math.{BigDecimal => JBigDecimal}
    import java.lang.{String => JavaString}
    import java.security.MessageDigest
    import java.util.HexFormat

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool}

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

    /// GET /gallery/scenes の本文。ギャラリー画面 1 枚ぶん — gallery/ の PNG に、
    /// 場面の説明 Doc のラベルとリファレンスとの差分を添えて名前順に並べる。
    /// ラベルの無い場面は title / desc が空で出る (画面が名前で代用する)。
    pub def scenesJson(dir: String): Json \ IO =
        let notes = SceneNotes.load(dir);
        let status = diffItemsOf(dir) |> List.toMap;
        let items = listNamed(suffix = ".png", dir, "gallery")
            |> List.map(entry -> sceneJson(notes, status, entry));
        JObject(Map#{
            "ok"    => JBool(true),
            "items" => JArray(List.toVector(items))
        })

    /// 場面 1 つぶんの応答。
    def sceneJson(notes: Map[String, SceneNotes.Note], status: Map[String, String],
                  entry: {name = String, mtime = Int64}): Json =
        let note = Map.getWithDefault(stemOf(entry#name), SceneNotes.defaultNote(), notes);
        JObject(Map#{
            "name"      => JString(entry#name),
            "mtime"     => JNumber(JBigDecimal.valueOf(entry#mtime)),
            "title"     => JString(note#title),
            "desc"      => JString(note#desc),
            "tags"      => JArray(note#tags |> List.map(JString) |> List.toVector),
            // approved は画面には ok と同じ (承認済みの絵。手元の基準は pump がすぐ写す)
            "reference" => JString(match Map.getWithDefault(entry#name, "missingReference", status) {
                case "approved" => "ok"
                case other      => other
            })
        })

    /// "s3_clear.png" → "s3_clear"。Doc の name は拡張子を持たない。
    pub def stemOf(fileName: String): String =
        if (String.endsWith(suffix = ".png", fileName))
            String.dropRight(String.length(".png"), fileName)
        else
            fileName

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
    /// `same` はバイト一致の判定、`approved` は reference/SHA256SUMS.txt の行との一致
    /// (どちらもテストでは差し替えられる)。
    ///
    /// approved: 手元の reference/*.png とは違う (または無い) が、絵の SHA-256 が
    /// SHA256SUMS.txt の行と同じ。ゲームのリポは PNG 本体を git に入れず一覧だけを追跡するので、
    /// 別の機械で基準を更新して push された絵は、手元では古い PNG と食い違って見える。
    /// それは承認済みの変化で、知らせる物ではない。
    pub def diffItems(galleryNames: List[String], referenceNames: List[String],
                      same: String -> Bool \ ef, approved: String -> Bool \ ef): List[(String, String)] \ ef =
        List.append(galleryNames, referenceNames)
            |> List.distinct
            |> List.sort
            |> List.map(name -> {
                let inGallery = List.memberOf(name, galleryNames);
                let inReference = List.memberOf(name, referenceNames);
                let sameBytes = if (inGallery and inReference) same(name) else false;
                let status = diffStatus(inGallery, inReference, sameBytes);
                let unmatched = status == "diff" or status == "missingReference";
                if (unmatched and approved(name)) (name, "approved") else (name, status)
            })

    /// ディスク上の gallery/ reference/ を見比べる。Changes の自動追随と NextStep が使う。
    pub def diffItemsOf(dir: String): List[(String, String)] \ IO =
        let galleryNames = listNamed(suffix = ".png", dir, "gallery") |> List.map(entry -> entry#name);
        let referenceNames = listNamed(suffix = ".png", dir, "reference") |> List.map(entry -> entry#name);
        let sums = loadReferenceSums(dir);
        diffItems(galleryNames, referenceNames, name -> sameBytes(dir, name),
            name -> isApproved(sums, dir, name))

    /// gallery/name と reference/name のバイト一致。どちらか読めなければ不一致扱い。
    def sameBytes(dir: String, name: String): Bool \ IO =
        match (readBytes("${dir}/gallery/${name}"), readBytes("${dir}/reference/${name}")) {
            case (Some(a), Some(b)) => Array.sameElements(a, b)
            case _                  => false
        }

    /// gallery/name の SHA-256 が SHA256SUMS.txt のその名前の行と同じか。行が無ければ false。
    def isApproved(sums: Map[String, String], dir: String, name: String): Bool \ IO =
        match (Map.get(name, sums), readBytes("${dir}/gallery/${name}")) {
            case (Some(expected), Some(bytes)) => sha256Hex(bytes) == expected
            case _                             => false
        }

    // ── reference/SHA256SUMS.txt (エンジンの make reference-update が書く一覧) ──

    /// reference/SHA256SUMS.txt を 名前 → SHA-256 で読む。無い・読めないは空 (= 承認済みは無し、今までどおり)。
    def loadReferenceSums(dir: String): Map[String, String] \ IO =
        match readBytes("${dir}/reference/SHA256SUMS.txt") {
            case Some(bytes) => parseReferenceSums(new JavaString(bytes, "UTF-8"))
            case None        => Map#{}
        }

    /// 一覧の本文 → 名前 → SHA-256。行は `shasum -a 256` の出力 "<ハッシュ>  <ファイル名>"
    /// (engine の bin/reference-update.sh が書く)。読み方は engine の
    /// go/internal/status の checkReference と同じ: 最初の空白の並びで 2 つに割り、
    /// 2 つにならない行は飛ばす。名前の前後の空白は落とす。
    pub def parseReferenceSums(text: String): Map[String, String] =
        String.lines(text)
            |> List.filterMap(line -> {
                let trimmed = String.trimLeft(line);
                let hash = String.takeWhile(c -> not Char.isWhitespace(c), trimmed);
                let name = String.trim(String.dropLeft(String.length(hash), trimmed));
                if (hash == "" or name == "") None else Some((name, hash))
            })
            |> List.toMap

    /// バイト列の SHA-256 (16 進の小文字)。shasum の出力と同じ形。
    pub def sha256Hex(bytes: Array[Int8, Static]): String \ IO =
        HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))

    /// gallery/ の PNG の mtime 一覧 (NextStep の鮮度判定に使う)。
    pub def galleryMtimes(dir: String): List[Int64] \ IO =
        listNamed(suffix = ".png", dir, "gallery") |> List.map(entry -> entry#mtime)

    /// gallery/ の PNG のうち、指定の時刻以降に書かれた物の名前 (Changes の自動追随が使う)。
    pub def galleryNamesSince(dir: String, since: Int64): List[String] \ IO =
        listNamed(suffix = ".png", dir, "gallery")
            |> List.filter(entry -> entry#mtime >= since)
            |> List.map(entry -> entry#name)
}
