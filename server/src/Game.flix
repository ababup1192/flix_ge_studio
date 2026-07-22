// Game — 対象プロジェクトのゲーム本体 (`make debug`) を Studio から起こす係。
// 焼き係 (Runner の bake) とは別の走行権を持つので、焼きながらゲームも走らせられる。
// 走っているかの判定は「GL 窓つき java プロセスの cwd が対象プロジェクトか」で見る
// (GET /running-games と同じ見方)。ログは Runner と同じ器・同じ形で GET /game/log に配る。

mod Game {
    import java.lang.ProcessBuilder
    import java.lang.Exception
    import java.lang.{String => JavaString}
    import java.io.{File => JFile}
    import java.util.ArrayList

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool}

    /// ゲーム用の走行権とログの器 (bake とは別の 1 個を Editor.serve が作る)。
    pub def make(): Runner.State \ IO = Runner.make()

    // ── 純粋なルール ──────────────────────────────────────

    /// ps の 1 行がゲーム (GL 窓つき・headless でない java) か。
    /// -XstartOnFirstThread は macOS で GL 窓を開く java の目印。
    pub def isGameLine(line: String): Bool =
        String.contains(substr = "-XstartOnFirstThread", line)
            and not String.contains(substr = "-Djava.awt.headless=true", line)

    // ── 状態 (GET /game/status) ───────────────────────────

    /// GET /game/status の本文。
    pub def statusJson(dir: String): Json \ IO =
        JObject(Map#{"running" => JBool(isRunning(dir))})

    /// dir を cwd に持つゲームプロセスが居るか。ps で候補を拾い、lsof で cwd を確かめる。
    /// コマンドが失敗したら「走っていない」に倒す (起動は 409 でなく走らせてみる側へ)。
    pub def isRunning(dir: String): Bool \ IO =
        try {
            let canonical = new JFile(dir).getCanonicalPath();
            runCommand("ps" :: "-Ao" :: "pid=,command=" :: Nil)
                |> String.lines
                |> List.filter(isGameLine)
                |> List.filterMap(pidOf)
                |> List.exists(pid -> cwdOf(pid) == canonical)
        } catch {
            case _: Exception => false
        }

    /// ps の 1 行 "  1234 /usr/bin/java …" の先頭から pid を取る。
    def pidOf(line: String): Option[Int32] =
        let trimmed = String.trim(line);
        String.indexOfLeft(substr = " ", trimmed)
            |> Option.flatMap(pos -> Int32.fromString(String.slice(start = 0, end = pos, trimmed)))

    /// pid の作業ディレクトリ。lsof の -Fn 出力は "n<path>" 行なので接頭辞を剥がす。
    def cwdOf(pid: Int32): String \ IO =
        runCommand("lsof" :: "-a" :: "-d" :: "cwd" :: "-p" :: "${pid}" :: "-Fn" :: Nil)
            |> String.lines
            |> List.filterMap(line ->
                if (String.startsWith(prefix = "n", line)) Some(String.dropLeft(1, line)) else None)
            |> List.head
            |> Option.getWithDefault("")

    /// 外部コマンドを 1 回走らせ、標準出力を UTF-8 文字列で返す (Editor.runCommand と同じ作法)。
    def runCommand(args: List[String]): String \ IO =
        let list = new ArrayList();
        foreach (arg <- args) discard list.add(arg);
        let pb = new ProcessBuilder(list);
        let env = pb.environment();
        discard env.remove("DEVELOPER_DIR");
        discard env.remove("SDKROOT");
        let proc = pb.start();
        let bytes = proc.getInputStream().readAllBytes();
        discard proc.waitFor();
        new JavaString(bytes, "UTF-8")

    // ── 起動 (POST /game/start) ───────────────────────────

    /// 応答 (status, body)。すでに走っていれば 409。起動は `make debug` を投げっぱなしで
    /// 走らせる (GL 窓はゲーム側が開く)。ログは GET /game/log (Runner.logJson と同じ形)。
    pub def start(state: Runner.State, dir: String): (Int32, Json) \ IO =
        if (isRunning(dir))
            (409, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JObject(Map#{"message" => JString("ゲームはすでに走っています")})
            }))
        else
            Runner.launchTarget(state, dir, "debug", busy = "ゲームを起動している最中です")
}
