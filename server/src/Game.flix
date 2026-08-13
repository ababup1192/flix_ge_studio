// Game — 対象プロジェクトのゲーム本体を Studio から起こす係。
// 焼き係 (Runner の bake) とは別の走行権を持つので、焼きながらゲームも走らせられる。
// 起動した Process を自分の手元に覚えて生死を見る (ps や lsof は使わない —
// Windows には無く、macOS 専用の目印にも頼れないため)。
// server が入れ替わっても迷子にしないよう、pid と命令口を ~/.flix_ge_studio/games.json に
// 書き残し、次の server が「まだ生きているもの」だけを引き継ぐ。

mod Game {
    import java.lang.Process
    import java.lang.ProcessHandle
    import java.lang.Exception
    import java.lang.System
    import java.lang.Thread
    import java.io.{File => JFile}
    import java.net.ServerSocket
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.util.concurrent.TimeUnit

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool}

    /// ゲーム用の走行権とログの器 (bake とは別の 1 個を Editor.serve が作る)。
    pub def make(): Runner.State \ IO = Runner.make()

    /// 起動したゲームの命令口の覚え書き (キー: プロジェクト dir、値: DEBUG_HTTP_PORT)。
    /// Runner と同じ理由で Java の Map を借りる (リクエストをまたいで生きる可変の器)。
    /// 再起動は同じキーへの上書きで新しくなり、停止はリモート焼きの失敗時に忘れる —
    /// 古い対応が残ったままゲーム亡き後のポートへ頼み続けることはない。
    pub type alias Ports = JMap[String, String]

    pub def makePorts(): Ports \ IO = new JMap()

    /// この server が起こしたゲームの手元記録 (キー: dir、値: Process)。
    pub type alias Procs = JMap[String, Process]

    /// 前の server から引き継いだゲーム (キー: dir、値: pid の文字列)。
    /// 手元記録 (Procs) と違い停止の道具が pid しか無いので、別のコンテナで区別しておく。
    pub type alias Adopted = JMap[String, String]

    /// ゲームまわりの器一式。走行権・手元記録・引き継ぎ・命令口をいつも一緒に持ち歩く。
    pub type alias Runtime = {state = Runner.State, procs = Procs, adopted = Adopted, ports = Ports}

    /// 器一式を作り、前の server の書き残しから生きているゲームを引き継ぐ。
    pub def makeRuntime(): Runtime \ IO =
        let runtime = {state = Runner.make(), procs = new JMap(), adopted = new JMap(), ports = makePorts()};
        revive(runtime);
        runtime

    /// dir のゲームの命令口を覚える (再起動は上書き)。
    pub def rememberPort(ports: Ports, dir: String, port: Int32): Unit \ IO =
        discard ports.put(dir, "${port}")

    /// dir のゲームの命令口 (覚えていなければ None)。
    pub def portOf(ports: Ports, dir: String): Option[Int32] \ IO =
        let value = ports.get(dir);
        portFromRecord(if (Object.isNull(value)) None else Some(value))

    /// dir の覚え書きを消す (ゲームの停止・命令口の不調を検知したとき)。
    pub def forgetPort(ports: Ports, dir: String): Unit \ IO =
        discard ports.remove(dir)

    // ── 純粋なルール ──────────────────────────────────────

    /// 覚え書きの値からポート番号を読む。無記録・数値でない記録は None =
    /// 「命令口は知らない」に倒す (リモート焼きへは進まず従来の make bake になる)。
    pub def portFromRecord(value: Option[String]): Option[Int32] =
        value |> Option.flatMap(Int32.fromString)

    /// 書き残し 1 件ぶんの JSON。
    pub def entryJson(entry: {dir = String, pid = String, port = String}): Json =
        JObject(Map#{
            "dir"  => JString(entry#dir),
            "pid"  => JString(entry#pid),
            "port" => JString(entry#port)
        })

    /// 書き残し 1 件を JSON から読む。欠けや型違いは None (その 1 件だけ捨てる)。
    pub def entryFromJson(json: Json): Option[{dir = String, pid = String, port = String}] =
        forM (
            obj  <- JsonCodec.expectObject(json);
            dir  <- Map.get("dir", obj)  |> Option.flatMap(JsonCodec.expectString);
            pid  <- Map.get("pid", obj)  |> Option.flatMap(JsonCodec.expectString);
            port <- Map.get("port", obj) |> Option.flatMap(JsonCodec.expectString)
        ) yield {dir = dir, pid = pid, port = port}

    // ── 状態 (GET /game/status) ───────────────────────────

    /// GET /game/status の本文。
    pub def statusJson(runtime: Runtime, dir: String): Json \ IO =
        JObject(Map#{"running" => JBool(isRunning(runtime, dir))})

    /// dir のゲームが走っているか。自分で起こした Process が最優先、無ければ
    /// 引き継いだ pid の生死を見る。どちらも無ければ走行権 (make debug が走行中か) を見る —
    /// make に倒れた道 (macOS で flix.jar が無い開発) では Process が手に入らないため。
    /// 判定に失敗したら「走っていない」に倒す (起動は 409 でなく走らせてみる側へ)。
    pub def isRunning(runtime: Runtime, dir: String): Bool \ IO =
        try {
            let proc: Process = (runtime#procs).get(dir);
            if (not Object.isNull(proc)) proc.isAlive()
            else {
                let pidText: String = (runtime#adopted).get(dir);
                if (not Object.isNull(pidText))
                    Int64.fromString(pidText) |> Option.exists(pidAlive)
                else
                    Runner.runningTarget(runtime#state) == Some("debug")
            }
        } catch {
            case _: Exception => false
        }

    /// pid のプロセスがまだ生きているか。別のプロセスに番号が使い回された場合に備え、
    /// コマンド名が分かるときは java であることも確かめる (分からなければ生死だけ見る)。
    def pidAlive(pid: Int64): Bool \ IO =
        try {
            let opt = ProcessHandle.of(pid);
            if (not opt.isPresent()) false
            else {
                let processHandle = opt.get();
                if (not processHandle.isAlive()) false
                else {
                    let command = processHandle.info().command();
                    if (command.isPresent()) String.contains(substr = "java", command.get()) else true
                }
            }
        } catch {
            case _: Exception => false
        }

    // ── 起動 (POST /game/start) ───────────────────────────

    /// 応答 (status, body)。すでに走っていれば 409。起動は debug タスクを投げっぱなしで
    /// 走らせる (GL ウィンドウはゲーム側が開く)。ログは GET /game/log (Runner.logJson と同じ形)。
    /// 空きポートを選んで DEBUG_HTTP_PORT で渡し、起動できたときだけ覚える —
    /// 以後の自動検査の焼きは、この命令口 (POST /bake) に頼めるようになる。
    /// 起動した Process は手元記録に収め、pid を書き残す (次の server が引き継げるように)。
    pub def start(runtime: Runtime, dir: String): (Int32, Json) \ IO =
        if (isRunning(runtime, dir))
            (409, JObject(Map#{
                "ok"    => JBool(false),
                "error" => JObject(Map#{"message" => JString("ゲームはすでに走っています")})
            }))
        else {
            let port = freePort();
            let (status, json) = Runner.launchTargetEnvHook(runtime#state, dir, "debug",
                ("DEBUG_HTTP_PORT", "${port}") :: Nil,
                proc -> {
                    discard (runtime#procs).put(dir, proc);
                    discard (runtime#adopted).remove(dir);
                    register(entry = {dir = dir, pid = "${proc.pid()}", port = "${port}"})
                },
                busy = "ゲームを起動している最中です");
            if (status == 202) rememberPort(runtime#ports, dir, port) else ();
            (status, json)
        }

    // ── 停止 (POST /game/stop) ────────────────────────────

    /// dir のゲームを止める。まず穏やかに頼み (destroy)、2 秒待っても生きていたら強制する。
    /// 引き継いだゲームは pid でしか触れないので ProcessHandle 経由で同じことをする。
    /// 走っていなければ 200 で「もう止まっている」— 押し直しを失敗にしない。
    pub def stop(runtime: Runtime, dir: String): (Int32, Json) \ IO =
        let stopped = try {
            let proc: Process = (runtime#procs).get(dir);
            if (not Object.isNull(proc) and proc.isAlive()) {
                proc.destroy();
                discard proc.waitFor(2i64, TimeUnit.SECONDS);
                if (proc.isAlive()) { discard proc.destroyForcibly(); () } else ();
                true
            } else {
                let pidText: String = (runtime#adopted).get(dir);
                if (Object.isNull(pidText)) false
                else Int64.fromString(pidText) |> Option.exists(stopPid)
            }
        } catch {
            case _: Exception => false
        };
        discard (runtime#procs).remove(dir);
        discard (runtime#adopted).remove(dir);
        forgetPort(runtime#ports, dir);
        unregister(dir);
        (200, JObject(Map#{
            "ok"      => JBool(true),
            "stopped" => JBool(stopped)
        }))

    /// pid のプロセスを止める。まず穏やかに頼み、0.5 秒待っても生きていたら強制する。
    def stopPid(pid: Int64): Bool \ IO =
        try {
            let opt = ProcessHandle.of(pid);
            if (not opt.isPresent()) false
            else {
                let processHandle = opt.get();
                discard processHandle.destroy();
                Thread.sleep(500i64);
                if (processHandle.isAlive()) discard processHandle.destroyForcibly() else ();
                true
            }
        } catch {
            case _: Exception => false
        }

    // ── 一覧 (GET /running-games) ─────────────────────────

    /// いま走っているゲームの一覧 {pid, cwd}。書き残しのうち生きているものを数える
    /// (自分で起こしたぶんも起動時に書き残しているので、ここに全部そろう)。
    /// 読めなければ空の一覧 (バッジ表示という補助機能なので静かに倒す)。
    pub def runningGamesJson(): Json \ IO =
        let games = loadRegistry()
            |> List.filter(entry -> Int64.fromString(entry#pid) |> Option.exists(pidAlive))
            |> List.map(entry -> JObject(Map#{
                "pid" => Int64.fromString(entry#pid)
                    |> Option.flatMap(Int64.tryToInt32)
                    |> Option.getWithDefault(0)
                    |> JsonCodec.intToBd |> JNumber,
                "cwd" => JString(entry#dir)
            }));
        JObject(Map#{"games" => JArray(List.toVector(games))})

    // ── 書き残し (~/.flix_ge_studio/games.json) ───────────

    /// 書き残しの置き場。
    def registryFile(): String \ IO =
        "${System.getProperty("user.home")}${JFile.separator}.flix_ge_studio${JFile.separator}games.json"

    /// 書き残しを読む。無い・壊れているは空扱い (引き継げないだけで本体は困らない)。
    def loadRegistry(): List[{dir = String, pid = String, port = String}] \ IO =
        let readResult = run { Fs.ReadFile.read(registryFile()) }
            with Fs.ReadFile.runWithFileRead
            with Fs.FileRead.runWithIO;
        readResult
            |> Result.toOption
            |> Option.flatMap(DocJson.parse)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(Map.get("games"))
            |> Option.flatMap(json -> JsonCodec.decodeList(entryFromJson, json))
            |> Option.getWithDefault(Nil)

    /// 書き残しを丸ごと書き直す。隣に書いてから改名で差し替える — 書いている途中で
    /// 落ちても、壊れた JSON を残さない (壊れた記録は次の server が「空」と読んでしまう)。
    /// 書けなくても落とさない (引き継ぎは保険であって本体ではない)。
    def saveRegistry(entries: List[{dir = String, pid = String, port = String}]): Unit \ IO =
        try {
            discard new JFile("${System.getProperty("user.home")}${JFile.separator}.flix_ge_studio").mkdirs();
            let json = JObject(Map#{"games" => JArray(entries |> List.map(entryJson) |> List.toVector)});
            let temp = registryFile() + ".tmp";
            let _ = run { Fs.WriteFile.write(str = Util.Json.toPrettyString(2, json) + "\n", temp) }
                with Fs.WriteFile.runWithFileWrite
                with Fs.FileWrite.runWithIO;
            // Windows の renameTo は上書きできないので、先に古い方を消す (temp は完成済み
            // なので、この間に落ちても「無い」に倒れるだけで壊れた JSON にはならない)。
            discard new JFile(registryFile()).delete();
            discard new JFile(temp).renameTo(new JFile(registryFile()));
            ()
        } catch {
            case _: Exception => ()
        }

    /// dir の書き残しを差し替える (同じ dir の古い記録は消す)。
    def register(entry: {entry = {dir = String, pid = String, port = String}}): Unit \ IO =
        loadRegistry()
            |> List.filter(other -> other#dir != (entry#entry)#dir)
            |> List.append((entry#entry) :: Nil)
            |> saveRegistry

    /// dir の書き残しを消す。
    def unregister(dir: String): Unit \ IO =
        loadRegistry() |> List.filter(other -> other#dir != dir) |> saveRegistry

    /// 前の server の書き残しから、まだ生きているゲームだけを引き継ぐ。
    /// 死んでいた記録はその場で書き残しからも消す (pid の使い回しに賭け続けない)。
    def revive(runtime: Runtime): Unit \ IO =
        let (alive, dead) = loadRegistry()
            |> List.partition(entry -> Int64.fromString(entry#pid) |> Option.exists(pidAlive));
        foreach (entry <- alive) {
            discard (runtime#adopted).put(entry#dir, entry#pid);
            foreach (port <- Int32.fromString(entry#port)) rememberPort(runtime#ports, entry#dir, port)
        };
        if (not List.isEmpty(dead)) saveRegistry(alive) else ()

    /// OS に空きポートを 1 つ選んでもらう (0 番で束ねてすぐ手放す)。
    def freePort(): Int32 \ IO =
        let socket = new ServerSocket(0);
        let port = socket.getLocalPort();
        socket.close();
        port
}
