// Sketches — ラフ塗り (draft/sketch/) のバージョン一覧を Studio に見せる窓口。
// 中身 (塗り・凡例) は解釈しない — 読むのは置き場の名前だけで、次に書く番号を
// 決める材料を返す。ラフの絵の意味は web 側 (SketchPad) の持ち物という線引き。

mod Sketches {
    import java.io.{File => JFile}
    import java.math.{BigDecimal => JBigDecimal}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool}

    /// ラフの置き場 (プロジェクト相対)。1 つのラフにつき 1 つのディレクトリ。
    pub def sketchRoot(): String = "draft/sketch"

    // ── 純粋なルール ─────────────────────────────────────

    /// ファイル名からバージョン番号 ("v3.json" → Some(3))。別の名前・0 以下は None。
    pub def versionOf(fileName: String): Option[Int32] =
        if (String.startsWith(prefix = "v", fileName) and String.endsWith(suffix = ".json", fileName)) {
            let digits = fileName
                |> String.dropLeft(1)
                |> String.dropRight(5);
            if (digits != "" and String.forAll(Char.isDigit, digits))
                Int32.fromString(digits) |> Option.filter(number -> number > 0)
            else
                None
        } else
            None

    /// 次に書くバージョン番号。まだ 1 つも無ければ 1。
    /// 抜け番があっても最大値の次を返す — 空いた番号を埋めると、
    /// 生成された物に書き残した「どのバージョンから」が別の絵を指してしまう。
    pub def nextVersion(versions: List[Int32]): Int32 =
        versions |> List.maximum |> Option.getWithDefault(0) |> num -> num + 1

    /// ラフの名前としてそのまま道に混ぜてよいか。区切りや上への移動を拒む。
    pub def isSafeName(name: String): Bool =
        name != "" and name != "." and name != ".."
            and not String.contains(substr = "/", name)
            and not String.contains(substr = "\\", name)

    // ── HTTP の口 ─────────────────────────────────────────

    /// GET /sketch/list — ラフの一覧と、それぞれのバージョン (新しい順)。
    /// 名前順に並べる (時刻を持たないので、人が探すなら名前の方が当てになる)。
    pub def listJson(dir: String): Json \ IO =
        let sketches = sketchDirNames(dir)
            |> List.sort
            |> List.map(name -> sketchJson(dir, name));
        JObject(Map#{"ok" => JBool(true), "sketches" => JArray(List.toVector(sketches))})

    /// ラフ 1 つぶんの応答。バージョンは新しい順 (先頭が最新)。
    def sketchJson(dir: String, name: String): Json \ IO =
        let versions = versionsOf(dir, name)
            |> List.sort
            |> List.reverse
            |> List.map(number -> JNumber(JBigDecimal.valueOf(Int32.toInt64(number))));
        JObject(Map#{
            "name"     => JString(name),
            "versions" => JArray(List.toVector(versions))
        })

    // ── 中身の読み取り ────────────────────────────────────

    /// draft/sketch 直下のディレクトリ名たち。置き場が無ければ空。
    def sketchDirNames(dir: String): List[String] \ IO =
        let entries = new JFile("${dir}/${sketchRoot()}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(file -> file.isDirectory())
            |> List.map(file -> file.getName())
            |> List.filter(isSafeName)

    /// 1 つのラフが持つバージョン番号たち。vN.json 以外のファイルは数えない。
    def versionsOf(dir: String, name: String): List[Int32] \ IO =
        let entries = new JFile("${dir}/${sketchRoot()}/${name}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(file -> file.isFile())
            |> List.filterMap(file -> versionOf(file.getName()))
}
