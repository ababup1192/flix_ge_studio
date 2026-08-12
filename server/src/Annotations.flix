// Annotations — ゲーム内で切った「違和感チケット」(debug/annotations/) を Studio に見せる窓口。
// チケットの中身 (world.json 等) は解釈しない — 読むのは README のタイトル行とコメント欄だけで、
// あとはパスを運ぶ。ゲームの言葉を Studio に持ち込まないための線引き。

mod Annotations {
    import java.io.{File => JFile}
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JBool}

    /// チケットの置き場 (プロジェクト相対)。engine の F8 注釈がここへ書き出す。
    def ticketsRoot(): String = "debug/annotations"

    /// 済んだチケットの移動先 (ticketsRoot の下の archive/)。
    def archiveDirName(): String = "archive"

    /// engine が README のコメント欄に置く空欄の印。これしか無ければ「未記入」と読む。
    def commentPlaceholder(): String = "（ここに修正指示を書いてください）"

    // ── 純粋なルール (README の節の読み書き) ──────────────

    /// README の 1 行目をチケットの見出しにする ("# " の飾りは剥がす)。
    pub def titleOf(readme: String): String =
        String.lines(readme)
            |> List.head
            |> Option.map(line -> String.trim(line))
            |> Option.map(line -> if (String.startsWith(prefix = "# ", line)) String.dropLeft(2, line) else line)
            |> Option.getWithDefault("")

    /// README の「## コメント」節の本文 (人が書いた一言)。節が無い・空欄の印のままなら ""。
    pub def commentOf(readme: String): String =
        let body = sectionBody(String.lines(readme)) |> String.intercalate("\n") |> String.trim;
        if (body == commentPlaceholder()) "" else body

    /// README の「## コメント」節の本文を comment に差し替えた全文。節が無ければ末尾に足す。
    /// コメント以外 (自動タイトル・囲った場所・コメント節の後に続く節) には触らない —
    /// 一言は必ず見出しの直後に置く。後ろに足すと commentOf が読めず、保存のたびに増殖する。
    pub def withComment(comment: {comment = String}, readme: String): String =
        let allLines = String.lines(readme);
        let body = "" :: comment#comment :: Nil;
        let kept = match splitAtSection(allLines) {
            case None                 => allLines ::: commentHeader() :: body
            case Some((before, rest)) => before ::: commentHeader() :: body ::: "" :: nextSectionOf(rest)
        };
        kept |> String.intercalate("\n")

    /// チケット id としてそのまま道に混ぜてよい名前か。区切りや上への移動を拒み、
    /// archive (移動先の入れ物) をチケットと取り違えない。
    pub def isSafeId(id: String): Bool =
        id != "" and id != "." and id != ".." and id != archiveDirName()
            and not String.contains(substr = "/", id)
            and not String.contains(substr = "\\", id)

    /// コメント節の見出し行。
    def commentHeader(): String = "## コメント"

    /// 行の列を「コメント節の手前まで」と「見出しを除いた残り」に分ける。節が無ければ None。
    def splitAtSection(allLines: List[String]): Option[(List[String], List[String])] =
        let isHeader = line -> String.trim(line) == commentHeader();
        if (List.exists(isHeader, allLines)) {
            let before = List.takeWhile(line -> not isHeader(line), allLines);
            let rest = List.dropWhile(line -> not isHeader(line), allLines) |> List.drop(1);
            Some((before, rest))
        } else
            None

    /// コメント節の本文: 見出しの後から、次の "## " 見出しの手前まで。
    def sectionBody(allLines: List[String]): List[String] =
        match splitAtSection(allLines) {
            case None            => Nil
            case Some((_, rest)) => List.takeWhile(line -> not isSection(line), rest)
        }

    /// コメント節の後に別の節が続くなら、その見出し以降 (コメント本文の差し替えで巻き込まない)。
    def nextSectionOf(rest: List[String]): List[String] =
        List.dropWhile(line -> not isSection(line), rest)

    def isSection(line: String): Bool =
        String.startsWith(prefix = "## ", String.trim(line))

    // ── HTTP の口 ─────────────────────────────────────────

    /// GET /annotations/list — チケットの一覧 (新しい順)。ディレクトリ名の頭が日時なので
    /// 名前の降順 = 新しい順。archive/ に移した物は出さない。
    pub def listJson(dir: String): Json \ IO =
        let tickets = ticketDirNames(dir)
            |> List.sort
            |> List.reverse
            |> List.map(id -> ticketJson(dir, id));
        JObject(Map#{"ok" => JBool(true), "tickets" => JArray(List.toVector(tickets))})

    /// チケット 1 件ぶんの応答。README が無い・読めないチケットも一覧には出す
    /// (タイトル空のまま。中で人が消しかけた物を隠すより見える方が困らない)。
    def ticketJson(dir: String, id: String): Json \ IO =
        let readme = FileIndex.readInside(dir, "${ticketsRoot()}/${id}/README.md")
            |> Result.toOption
            |> Option.getWithDefault("");
        JObject(Map#{
            "id"      => JString(id),
            "title"   => JString(titleOf(readme)),
            "comment" => JString(commentOf(readme)),
            "hasShot" => JBool(Option.nonEmpty(shotFile(dir, id)))
        })

    /// GET /annotations/shot?id=… — チケットのスクショ (赤枠つき優先)。無ければ None = 404。
    /// ブラウザが付ける URL エンコードは URI.getQuery が既に戻している (二重に戻さない —
    /// 戻し直すと + が空白に化けてファイルを見失う)。
    pub def shotBytes(dir: String, params: Map[String, String]): Option[Array[Int8, Static]] \ IO =
        Map.get("id", params)
            |> Option.filter(isSafeId)
            |> Option.flatMap(id -> shotFile(dir, id))
            |> Option.map(file -> Files.readAllBytes(file.toPath()))

    /// 見せる絵: highlighted.png (囲った場所が赤枠で分かる) を先に、無ければ screenshot.png。
    def shotFile(dir: String, id: String): Option[JFile] \ IO =
        let candidate = name -> {
            let file = new JFile("${dir}/${ticketsRoot()}/${id}/${name}");
            if (file.isFile()) Some(file) else None
        };
        match candidate("highlighted.png") {
            case Some(file) => Some(file)
            case None       => candidate("screenshot.png")
        }

    /// POST /annotations/comment {id, comment} — README のコメント欄へ一言を書き残す。
    pub def setComment(dir: String, bodyText: String): (Int32, Json) \ IO =
        match parseFields(bodyText) {
            case None => (400, failJson("body は {id, comment} の JSON で指定してください"))
            case Some((id, comment)) =>
                if (not isSafeId(id))
                    (400, failJson("チケット名が正しくありません: ${id}"))
                else {
                    let readmePath = "${ticketsRoot()}/${id}/README.md";
                    match FileIndex.readInside(dir, readmePath) {
                        case Err(_) => (404, failJson("チケットが見つかりません: ${id}"))
                        case Ok(readme) =>
                            match FileIndex.writeInside(str = withComment(comment = comment, readme), dir, readmePath) {
                                case Err(message) => (500, failJson("コメントを保存できませんでした: ${message}"))
                                case Ok(_)        => (200, JObject(Map#{"ok" => JBool(true)}))
                            }
                    }
                }
        }

    /// POST /annotations/archive {id} — 済んだチケットを archive/ へ移して一覧から下げる (消さない)。
    pub def archive(dir: String, bodyText: String): (Int32, Json) \ IO =
        match parseId(bodyText) {
            case None => (400, failJson("body は {id} の JSON で指定してください"))
            case Some(id) =>
                if (not isSafeId(id))
                    (400, failJson("チケット名が正しくありません: ${id}"))
                else {
                    let source = new JFile("${dir}/${ticketsRoot()}/${id}");
                    let destRoot = new JFile("${dir}/${ticketsRoot()}/${archiveDirName()}");
                    let dest = new JFile("${dir}/${ticketsRoot()}/${archiveDirName()}/${id}");
                    if (not source.isDirectory())
                        (404, failJson("チケットが見つかりません: ${id}"))
                    else {
                        discard destRoot.mkdirs();
                        if (source.renameTo(dest))
                            (200, JObject(Map#{"ok" => JBool(true)}))
                        else
                            (500, failJson("アーカイブへ移せませんでした: ${id}"))
                    }
                }
        }

    // ── 中身の読み取り ────────────────────────────────────

    /// debug/annotations 直下のチケットディレクトリ名たち (archive は除く)。置き場が無ければ空。
    def ticketDirNames(dir: String): List[String] \ IO =
        let entries = new JFile("${dir}/${ticketsRoot()}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries)
            |> List.filter(file -> file.isDirectory())
            |> List.map(file -> file.getName())
            |> List.filter(isSafeId)

    /// body {id, comment} の取り出し。形が違えば None。
    def parseFields(bodyText: String): Option[(String, String)] =
        DocJson.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> forM (
                id      <- Map.get("id", obj) |> Option.flatMap(JsonCodec.expectString);
                comment <- Map.get("comment", obj) |> Option.flatMap(JsonCodec.expectString)
            ) yield (id, comment))

    /// body {id} の取り出し。形が違えば None。
    def parseId(bodyText: String): Option[String] =
        DocJson.parse(bodyText)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj -> Map.get("id", obj))
            |> Option.flatMap(JsonCodec.expectString)

    def failJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
