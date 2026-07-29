// Search — GET /search?q=…。宣言済み Doc を横断して、文字列値の単純一致を探す。
//
// 対象は project.json の "editor" 宣言に一致するファイルだけ。生成物や依存を
// 巻き込まないための線引きで、宣言に無い物は「編集の対象ではない」= 探す対象でもない。
//
// 返す物は 2 組: ファイル名の一致 (files) と、中身の一致 (results)。
// 「map で探す」人はまず *.map.json の一群を思い浮かべるので、名前の照合は
// 大きさに関わらず常に行い、結果の先頭に置く (名前の照合は安い)。
//
// 中身の走査だけは巨大ファイル (512KB 超) を飛ばす。焼き上がりの sfx.json や
// ドット絵は 1 本で数百 KB あり、打鍵のたびに全走査すると画面が固まる。
//
// 照合は大小を区別する単純一致。置換 (web 側) が「見つけた文字をそのまま置き換える」
// 形なので、探し方と置き換え方を同じ規則に揃える。
mod Search {
    import java.io.{File => JFile}

    use Util.Json.Json
    use Util.Json.Json.{JArray, JObject, JString, JNumber, JBool}

    /// 本文を読む上限 (これを超えるファイルは名前だけ見る)。
    pub def bodyLimitBytes(): Int64 = 512i64 * 1024i64

    /// 返す件数の上限。超えた分は総数 (total) だけ伝える。
    pub def resultLimit(): Int32 = 200

    /// 1 件の当たり: どのファイルの・どの JSON パスの・どんな値か。
    pub type alias Hit = {file = String, path = List[Json], value = String, excerpt = String}

    pub def respond(dir: String, params: Map[String, String]): (Int32, Json) \ IO =
        match Map.get("q", params) {
            case None => (400, errorJson("q is required"))
            case Some(needle) =>
                if (String.isEmpty(needle))
                    (200, resultsJson(Nil, Nil, 0))
                else {
                    let paths = declaredPaths(dir);
                    let files = paths |> List.filter(path -> String.contains(substr = needle, path));
                    let hits = paths |> List.flatMap(path -> searchFile(dir, needle, path));
                    (200, resultsJson(files, List.take(resultLimit(), hits), List.length(hits)))
                }
        }

    /// 宣言に一致する実ファイル (重複は 1 つに、宣言順のまま)。
    pub def declaredPaths(dir: String): List[String] \ IO =
        match FileIndex.readInside(dir, "project.json") {
            case Err(_)   => Nil
            case Ok(text) => match JsonCompat.parse(text) {
                case None       => Nil
                case Some(json) => match ResourceDecl.fromProjectJson(json) {
                    case Err(_)    => Nil
                    case Ok(decls) => {
                        let all = FileIndex.listAllJson(dir);
                        decls#resources
                            |> List.flatMap(decl -> ResourceDecl.matchFiles(decl#pattern, all))
                            |> List.distinct
                    }
                }
            }
        }

    /// 1 ファイルの中身の当たり。大きすぎる物は走査しない (名前の照合は files 側が済ませている)。
    def searchFile(dir: String, needle: String, path: String): List[Hit] \ IO =
        if (tooLarge(dir, path))
            Nil
        else match FileIndex.readInside(dir, path) {
            case Err(_)   => Nil
            case Ok(text) => match JsonCompat.parse(text) {
                case None       => Nil
                case Some(json) => walk(needle, path, Nil, json)
            }
        }

    def tooLarge(dir: String, relPath: String): Bool \ IO =
        match FileIndex.resolveInside(dir, relPath) {
            case None           => true
            case Some(fullPath) => new JFile(fullPath).length() > bodyLimitBytes()
        }

    /// 文書を歩いて文字列値だけを照合する。キー名と数値は対象外 —
    /// 置換できない物を検索結果に出すと、押しても何も起きない行が混じるため。
    def walk(needle: String, file: String, rev: List[Json], json: Json): List[Hit] =
        match json {
            case JString(s) =>
                if (String.contains(substr = needle, s))
                    ({file = file, path = List.reverse(rev), value = s, excerpt = excerptOf(needle, s)}: Hit) :: Nil
                else Nil

            case JObject(fields) =>
                Map.toList(fields)
                    |> List.flatMap(match (key, value) -> walk(needle, file, JString(key) :: rev, value))

            case JArray(items) =>
                Vector.toList(items)
                    |> List.mapWithIndex((index, item) -> walk(needle, file, JNumber(Int32.toBigDecimal(index)) :: rev, item))
                    |> List.flatten

            case _ => Nil
        }

    /// 前後 30 文字ぶんの抜粋 (長い台詞の中の 1 語でも、どこで当たったか見える)。
    pub def excerptOf(needle: String, value: String): String =
        let window = 30;
        match String.indexOf(substr = needle, value) {
            case None => value
            case Some(at) =>
                let left = Int32.max(0, at - window);
                let right = Int32.min(String.length(value), at + String.length(needle) + window);
                let core = String.slice(start = left, end = right, value);
                let head = if (left > 0) "…" else "";
                let tail = if (right < String.length(value)) "…" else "";
                "${head}${core}${tail}"
        }

    def resultsJson(files: List[String], hits: List[Hit], total: Int32): Json =
        let shownFiles = List.take(resultLimit(), files);
        JObject(Map#{
            "files"      => JArray(shownFiles |> List.map(JString) |> List.toVector),
            "filesTotal" => JNumber(Int32.toBigDecimal(List.length(files))),
            "total"      => JNumber(Int32.toBigDecimal(total)),
            "truncated"  => JBool(total > List.length(hits) or List.length(files) > List.length(shownFiles)),
            "results"    => JArray(hits |> List.map(hitJson) |> List.toVector)
        })

    def hitJson(hit: Hit): Json =
        JObject(Map#{
            "file"    => JString(hit#file),
            "path"    => JArray(List.toVector(hit#path)),
            "value"   => JString(hit#value),
            "excerpt" => JString(hit#excerpt)
        })

    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
