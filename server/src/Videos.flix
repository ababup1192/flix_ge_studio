// Videos — ギャラリーの「動画」のタブの口。ゲームが撮った動画（debug/video/ の mp4）を一覧にし、
// 再生用にファイルを配る（Range つき）。
//
// 撮れたか・基準と違うかの判定はここに置かない。engine の道具 `bin/fge videos status --json` が持っていて、
// ここはそれを流して結果をそのまま画面へ渡す（make status と Studio が同じ判定を読む）。
// 道具に videos が無い古い engine では、一覧の代わりに理由を返す（画面が案内に落とす）。
//
// debug/ は git の外なので、動画は撮った Mac にしか無い。置き場が無ければ here = false で返し、
// 画面が「この Mac では撮っていません」と言う。書き込みは一切しない。

mod Videos {
    use fge::{DocJson, JsonCodec}

    import java.lang.ProcessBuilder
    import java.lang.System
    import java.lang.Throwable
    import java.io.{File => JFile}
    import java.io.BufferedReader
    import java.io.InputStreamReader
    import java.util.ArrayList

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JBool}

    // ── 純粋なルール ──────────────────────────────────────

    /// 動画の置き場（ゲームの根から）。道具の videos.Dir と同じ。
    pub def videoDir(): String = "debug/video"

    /// 版の名前を数の並びとして比べる（"0.49.0" > "0.45.10" > "0.45.9"。読めない欄は 0）。
    pub def versionKey(name: String): List[Int32] =
        String.splitOn(substr = ".", name) |> List.map(part -> Int32.fromString(part) |> Option.getWithDefault(0))

    /// 道具を試す順: Studio の engine → 置き場の engine を新しい版から。
    // WhyNot: Studio の engine の道具だけに頼らない — Studio が同梱する engine はゲームの master の版に
    // そろえるので（README の「リリースの手順」）、ゲームより先に videos を持つ新しい engine が置き場に
    // あっても使えないままになる。判定は道具の版によらず同じ JSON の形。
    pub def toolCandidates(current: String, installed: List[String]): List[String] =
        let keyOf = dir -> versionKey(String.splitOn(substr = "/", dir) |> List.last |> Option.getWithDefault(dir));
        current :: (installed |> List.sortWith((a, b) -> keyOf(b) <=> keyOf(a)) |> List.filter(dir -> dir != current))

    /// 道具を呼ぶ引数。
    pub def statusArgs(tool: String, dir: String): List[String] =
        tool :: "videos" :: "status" :: "--root" :: dir :: "--json" :: Nil

    /// 配ってよいパスか。置き場の下の mp4・PNG（最後の画面）・txt（sfx.txt）だけを通す。
    pub def isServable(path: String): Bool =
        String.startsWith(prefix = videoDir() + "/", path)
            and not String.contains(substr = "..", path)
            and not String.contains(substr = "\\", path)
            and List.exists(ext -> String.endsWith(suffix = ext, path), ".mp4" :: ".png" :: ".txt" :: Nil)

    /// 配る時の Content-Type。
    pub def contentTypeOf(path: String): String =
        if (String.endsWith(suffix = ".mp4", path)) "video/mp4"
        else if (String.endsWith(suffix = ".png", path)) "image/png"
        else "text/plain; charset=utf-8"

    /// Range のヘッダ（bytes=開始-終わり）の読み。
    ///   Whole         … ヘッダが無い・読めない（全部を 200 で返す）
    ///   Part(a, b)    … a から b まで（両端を含む。206 で返す）
    ///   Unsatisfiable … ファイルの外を指した（416）
    // WhyNot: 複数の範囲（bytes=0-1,5-9）は読まない — video 要素は 1 つしか頼まない。
    pub enum Range with Eq, ToString {
        case Whole
        case Part(Int64, Int64)
        case Unsatisfiable
    }

    pub def rangeOf(header: Option[String], size: Int64): Range =
        match header |> Option.map(String.trim) {
            case Some(text) if String.startsWith(prefix = "bytes=", text) and not String.contains(substr = ",", text) =>
                match String.splitOn(substr = "-", String.dropLeft(6, text)) {
                    // bytes=-N は末尾の N バイト
                    case "" :: last :: Nil => match Int64.fromString(last) {
                        case Some(n) if n > 0i64 and size > 0i64 => Range.Part(Int64.max(0i64, size - n), size - 1i64)
                        case _ => Range.Unsatisfiable
                    }
                    case first :: last :: Nil => match Int64.fromString(first) {
                        case None => Range.Whole
                        case Some(offset) if offset >= size => Range.Unsatisfiable
                        case Some(offset) =>
                            let upTo = if (last == "") size - 1i64
                                       else Int64.fromString(last) |> Option.map(n -> Int64.min(n, size - 1i64)) |> Option.getWithDefault(size - 1i64);
                            if (upTo < offset) Range.Unsatisfiable else Range.Part(offset, upTo)
                    }
                    case _ => Range.Whole
                }
            case _ => Range.Whole
        }

    /// 道具が使えない時の応答（画面は reason を案内に出す）。
    pub def unavailableJson(reason: String, here: Bool): Json =
        JObject(Map#{
            "ok"        => JBool(true),
            "available" => JBool(false),
            "reason"    => JString(reason),
            "here"      => JBool(here),
            "videos"    => JArray(Vector.empty())
        })

    /// 道具の出力の最後の JSON の行に、使えた印と置き場の有無を足す。読めなければ None。
    pub def answerOf(lines: List[String], here: Bool): Option[Json] =
        lines
            |> List.filterMap(line -> DocJson.parse(line) |> Option.flatMap(JsonCodec.expectObject))
            |> List.last
            |> Option.map(obj -> JObject(obj
                |> Map.insert("ok", JBool(true))
                |> Map.insert("available", JBool(true))
                |> Map.insert("here", JBool(here))))

    /// 流し直しを頼む命令口への 1 行（flow をゲームの新しい World から流し、結果へ差し替える）。
    pub def rerunRequest(flow: String): String = "POST /flow?path=${flow}&apply=1"

    /// 命令口の答えの 1 行目が ok で始まれば流れた。それ以外（FAIL 行番号つき・error:）は理由。
    pub def rerunOutcome(body: String): Result[String, Unit] =
        match String.lines(body) |> List.head {
            case Some(line) if String.startsWith(prefix = "ok", line) => Ok(())
            case Some(line) => Err(line)
            case None       => Err("ゲームが何も答えませんでした")
        }

    /// 一覧の JSON から name の動画の flow を引く（無い・flow で撮っていなければ空）。
    pub def flowOf(name: String, listing: Json): String =
        match listing {
            case JObject(obj) => Map.get("videos", obj)
                |> Option.flatMap(JsonCodec.decodeList(JsonCodec.expectObject))
                |> Option.flatMap(List.find(v -> JsonCodec.stringOr("name", "", v) == name))
                |> Option.map(v -> JsonCodec.stringOr("flow", "", v))
                |> Option.getWithDefault("")
            case _ => ""
        }

    // ── POST /videos/rerun ────────────────────────────────

    /// POST /videos/rerun {name} — その動画を撮った flow を、起動中のゲームへ流し直す（ビルドしない）。
    /// 起動していない・flow で撮っていない動画は 409 / 400 と理由（画面はカードに出す）。
    pub def rerun(dir: String, port: Option[Int32], body: String): (Int32, Json) \ IO =
        let name = DocJson.parse(body) |> Option.flatMap(JsonCodec.expectObject)
            |> Option.map(obj -> JsonCodec.stringOr("name", "", obj)) |> Option.getWithDefault("");
        let flow = flowOf(name, listJson(dir));
        if (flow == "") (400, failJson("flow で撮っていない動画は流し直せません"))
        else match port {
            case None => (409, failJson("ゲームが起動していません（Studio の ▶ で起動してから押してください）"))
            case Some(p) => match Runner.requestGame(p, rerunRequest(flow), "") {
                case Runner.GameReply.Answered(raw) => match rerunOutcome(Runner.bodyOfHttpText(raw)) {
                    case Ok(_)    => (200, JObject(Map#{"ok" => JBool(true), "flow" => JString(flow)}))
                    case Err(why) => (200, failJson(why))
                }
                case _ => (409, failJson("ゲームの命令口に繋がりません（起動の途中か、閉じた後です）"))
            }
        }

    def failJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JObject(Map#{"message" => JString(message)})})

    // ── GET /videos ───────────────────────────────────────

    /// GET /videos の本文。
    pub def listJson(dir: String): Json \ IO =
        let here = new JFile("${dir}/${videoDir()}").isDirectory();
        let home = System.getProperty("user.home");
        let installed = EngineStore.versionsDir(home) |> listDirs |> List.filter(EngineStore.isVersionName)
            |> List.map(name -> EngineStore.versionDir(home, name));
        let tools = toolCandidates(EngineHome.dir(), installed)
            |> List.map(engine -> EngineUpdate.toolPath(engine, EngineTasks.isWindows()))
            |> List.filter(tool -> new JFile(tool).canExecute());
        firstAnswer(dir, here, tools, "")

    /// 道具を順に試し、最初に JSON で答えた物を返す。どれも答えなければ理由。
    def firstAnswer(dir: String, here: Bool, tools: List[String], said: String): Json \ IO =
        match tools {
            case Nil =>
                unavailableJson("動画の一覧を読める engine の道具がありません（engine 0.49.0 の bin/fge videos が要ります）${if (said == "") "" else ": " + said}", here)
            case tool :: rest =>
                let (code, lines) = runIn(dir, statusArgs(tool, dir));
                match answerOf(lines, here) {
                    case Some(json) if code == 0 => json
                    case _ => firstAnswer(dir, here, rest, lines |> List.head |> Option.getWithDefault(said))
                }
        }

    def listDirs(path: String): List[String] \ IO =
        let entries = new JFile(path).listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries) |> List.filter(f -> f.isDirectory()) |> List.map(f -> f.getName())

    /// 道具を dir で流し、終わりの番号と出力の行を返す。
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
