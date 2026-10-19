// Talk — 会話の画面の口。左で .yarn を書き、右にカーソルの行の本物のゲームの画面を出す。
//
//   GET  /talk/files            会話のシナリオ（*.yarn）の一覧と、窓なしのゲームの入口があるか
//   POST /talk/build {path, text?}  text があれば保存してから、そのフォルダを bin/fge yarn build --json で組む
//                                （誤りは行番号と近い名前の案つき・ノードの一覧。text が無ければ check で読むだけ）
//   POST /talk/shot  {node, line}   窓なしのゲームで `start talk node=… line=…` を組んで 1 枚撮る（PNG を base64 で）
//   POST /talk/goto  {node, line}   起動中のゲームをその行へ飛ばす（POST /start?name=talk&…）
//
// .yarn を読むのはゲームの bin/fge（Go）だけ。Studio は文法を持たない。絵もゲームが描く（Studio は描かない）。
// 打っている途中の壊れた台本は PreviewUi と同じく HTTP 200 の ok:false で返し、エラーにしない
// （壊れている間は dialogue.json が書かれず、前の 1 枚を「止まっている理由」つきで出したままにする）。
mod Talk {
    use fge::{DocJson, JsonCodec}

    import java.io.{File => JFile}
    import java.io.BufferedReader
    import java.io.InputStreamReader
    import java.lang.ProcessBuilder
    import java.lang.Throwable
    import java.nio.file.Files
    import java.util.ArrayList
    import java.util.{Base64 => JBase64}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JBool, JNumber}

    // ── 純粋なルール ──────────────────────────────────────

    /// 頼めるシナリオのパスか（プロジェクトの中の .yarn だけ。.. と絶対パスは断る）。
    pub def isYarnPath(path: String): Bool =
        String.endsWith(suffix = ".yarn", path)
            and not String.startsWith(prefix = "/", path)
            and not String.contains(substr = "..", path)
            and not String.contains(substr = "\\", path)

    /// .yarn のあるフォルダ（build に渡す）。
    pub def folderOf(path: String): String =
        match String.indexOfRight(substr = "/", path) {
            case Some(i) => String.takeLeft(i, path)
            case None    => "."
        }

    /// 始まりの行（窓なしのゲームの flow・起動中のゲームの /start と同じ）。
    pub def startLine(node: String, line: Int32): String = "start talk node=${node} line=${line}"

    /// 起動中のゲームへの /start の 1 行。
    pub def gotoRequest(node: String, line: Int32): String = "POST /start?name=talk&node=${node}&line=${line}"

    /// ノード名に使ってよい字か（クエリと flow の行にそのまま載せるため。Yarn の title: も同じ字）。
    pub def isNodeName(node: String): Bool =
        node != "" and String.forAll(c -> Char.isLetterOrDigit(c) or c == '_' or c == '-' or c == '.', node)

    /// 撮った答えから PNG のパス（ok png=…）か、撮れなかった理由。
    pub def shotOutcome(body: String): Result[String, String] =
        let lines = String.lines(body) |> List.map(String.trim);
        match lines |> List.find(line -> String.startsWith(prefix = "ok png=", line)) {
            case Some(line) => Ok(String.dropLeft(String.length("ok png="), line))
            case None => Err(lines |> List.find(line -> String.startsWith(prefix = "error", line) or String.startsWith(prefix = "FAIL", line))
                                   |> Option.getWithDefault(lines |> List.head |> Option.getWithDefault("窓なしのゲームが何も答えませんでした")))
        }

    // ── GET /talk/files ───────────────────────────────────

    /// シナリオの一覧（lib・build・debug などの外）。
    pub def filesJson(dir: String): Json \ IO =
        let files = yarnFilesUnder(dir, "") |> List.sort;
        JObject(Map#{
            "ok"       => JBool(true),
            "files"    => JArray(files |> List.map(JString) |> List.toVector),
            "headless" => JBool(Headless.hasEntry(dir))
        })

    def yarnFilesUnder(dir: String, rel: String): List[String] \ IO =
        let entries = new JFile(if (rel == "") dir else "${dir}/${rel}").listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries) |> List.flatMap(f -> {
            let path = if (rel == "") f.getName() else "${rel}/${f.getName()}";
            if (f.isDirectory())
                (if (Set.memberOf(f.getName(), FileIndex.excludedDirs()) or String.startsWith(prefix = ".", f.getName())
                        or f.getName() == "test" or f.getName() == "src") Nil
                 else yarnFilesUnder(dir, path))
            else if (String.endsWith(suffix = ".yarn", f.getName())) path :: Nil
            else Nil
        })

    // ── POST /talk/build ──────────────────────────────────

    /// 保存（text があれば）して組む。誤りは ok:false と problems（その行に出す）。
    pub def build(dir: String, body: String): (Int32, Json) \ IO =
        let obj = DocJson.parse(body) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        let path = JsonCodec.stringOr("path", "", obj);
        if (not isYarnPath(path)) (400, failJson("path は .yarn（プロジェクトの中）"))
        else {
            let saved = match Map.get("text", obj) |> Option.flatMap(JsonCodec.expectString) {
                case Some(text) => FileIndex.writeInside(str = text, dir, path)
                case None       => Ok(())
            };
            match saved {
                case Err(why) => (200, failJson("保存できません: ${why}"))
                case Ok(_) =>
                    let verb = if (Map.memberOf("text", obj)) "build" else "check";
                    let (_, lines) = runIn(dir, yarnTool(dir) :: "yarn" :: verb :: folderOf(path) :: "--json" :: Nil);
                    match lines |> List.filterMap(line -> DocJson.parse(line) |> Option.flatMap(JsonCodec.expectObject)) |> List.last {
                        case Some(report) => (200, JObject(report |> Map.insert("saved", JBool(verb == "build"))))
                        case None =>
                            let said = lines |> List.head |> Option.getWithDefault("(出力なし)");
                            (200, failJson("このゲームの bin/fge では会話のシナリオを組めません（engine 0.49.0 の yarn --json が要ります）: ${said}"))
                    }
            }
        }

    /// .yarn を読む道具: ゲームの bin/fge-go（ゲームの engine の版の木を書く）。無ければ Studio の engine の物。
    def yarnTool(dir: String): String \ IO =
        let exe = if (EngineTasks.isWindows()) "fge-go.exe" else "fge-go";
        let own = new JFile("${dir}/bin/${exe}");
        if (own.canExecute()) own.getAbsolutePath()
        else EngineUpdate.toolPath(EngineHome.dir(), EngineTasks.isWindows())

    // ── POST /talk/shot ───────────────────────────────────

    /// 窓なしのゲームで 1 枚撮る。起こしている最中は ok:false・starting:true（画面は少し後に頼み直す）。
    pub def shot(headless: Headless.State, game: Game.Runtime, dir: String, body: String): (Int32, Json) \ IO =
        let (node, line) = targetOf(body);
        if (not isNodeName(node) or line < 1) (400, failJson("node（ノードの名前）と line（1 以上）が要ります"))
        else match Headless.ensure(headless, game, dir) {
            case Headless.Readiness.Unsupported(why) => (200, failJson(why))
            case Headless.Readiness.Starting(why) =>
                (200, JObject(Map#{ "ok" => JBool(false), "starting" => JBool(true),
                                    "error" => JObject(Map#{ "message" => JString(why) }) }))
            case Headless.Readiness.Ready(port) =>
                match Runner.requestGame(port, "POST /step?view=shot", startLine(node, line)) {
                    case Runner.GameReply.Answered(raw) => match shotOutcome(Runner.bodyOfHttpText(raw)) {
                        case Ok(png) => match readBase64("${dir}/${png}") {
                            case Some(data) => (200, JObject(Map#{
                                "ok" => JBool(true), "node" => JString(node),
                                "line" => JNumber(JsonCodec.intToBd(line)), "png" => JString(data) }))
                            case None => (200, failJson("撮った絵を読めません: ${png}"))
                        }
                        case Err(why) => (200, failJson(why))
                    }
                    case _ =>
                        let tail = Headless.logTail(dir, 3) |> String.intercalate(" / ");
                        (200, failJson("窓なしのゲームに繋がりません（debug/headless.log: ${tail}）"))
                }
        }

    def targetOf(body: String): (String, Int32) =
        let obj = DocJson.parse(body) |> Option.flatMap(JsonCodec.expectObject) |> Option.getWithDefault(Map.empty());
        (JsonCodec.stringOr("node", "", obj), JsonCodec.intOr("line", 0, obj))

    def readBase64(path: String): Option[String] \ IO =
        try { Some(JBase64.getEncoder().encodeToString(Files.readAllBytes(new JFile(path).toPath()))) } catch {
            case _: Throwable => None
        }

    // ── POST /talk/goto ───────────────────────────────────

    /// 起動中のゲームをその行へ飛ばす（遊んでいる World を差し替える。止めていれば止めたまま）。
    pub def goto(port: Option[Int32], body: String): (Int32, Json) \ IO =
        let (node, line) = targetOf(body);
        if (not isNodeName(node) or line < 1) (400, failJson("node（ノードの名前）と line（1 以上）が要ります"))
        else match port {
            case None => (200, failJson("ゲームが起動していません（▶ プレイで起動するか、DEBUG=true で起動したゲームなら games.json に書き置かれます）"))
            case Some(p) => match Runner.requestGame(p, gotoRequest(node, line), "") {
                case Runner.GameReply.Answered(raw) =>
                    let answer = Runner.bodyOfHttpText(raw);
                    if (String.startsWith(prefix = "ok", answer)) (200, JObject(Map#{ "ok" => JBool(true) }))
                    else (200, failJson(String.lines(answer) |> List.head |> Option.getWithDefault("ゲームが断りました")))
                case _ => (200, failJson("ゲームの命令口に繋がりません（起動の途中か、閉じた後です）"))
            }
        }

    // ── 小物 ──────────────────────────────────────────────

    def failJson(message: String): Json =
        JObject(Map#{ "ok" => JBool(false), "error" => JObject(Map#{ "message" => JString(message) }) })

    def runIn(dir: String, args: List[String]): (Int32, List[String]) \ IO =
        try {
            let list = new ArrayList();
            foreach (arg <- args) discard list.add(arg);
            let pb = new ProcessBuilder(list);
            discard pb.directory(new JFile(dir));
            discard pb.redirectErrorStream(true);
            let proc = pb.start();
            let reader = new BufferedReader(new InputStreamReader(proc.getInputStream(), "UTF-8"));
            let lines = readAll(reader, Nil);
            (proc.waitFor(), lines)
        } catch {
            case e: Throwable => (-1, e.toString() :: Nil)
        }

    def readAll(reader: BufferedReader, acc: List[String]): List[String] \ IO =
        let line = reader.readLine();
        if (Object.isNull(line)) List.reverse(acc) else readAll(reader, line :: acc)
}
