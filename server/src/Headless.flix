// Headless — 会話のプレビューに使う「窓なしのゲーム」の世話係（プロジェクトごとに 1 本）。
//
// 窓なしのゲームはゲーム自身のコード（engine の App.Headless・ゲームの E2e.serveHeadless）で、
// engine の bin/headless-run が jar を作って流す。Studio は自分で描かず、ここへ「この場面を 1 枚」と頼む
// （本物の立ち絵・表情・窓・後ろの場面が出る）。起動中のゲームは動かさない（動かすのは「この行へ飛ばす」だけ）。
//
// 決まり:
//   - 起こすのは頼まれた時だけ。立ち上げ中は待たせず「起こしています」を返す（初回は jar を作るので 1〜2 分）
//   - ゲーム（窓あり）が起動の途中なら起こさない（コンパイルと JVM のメモリの山を重ねない。Game.isStarting）
//   - メモリの上限は -Xmx1g（HEADLESS_JAVA_OPTS）・放っておかれたら自分で終わる（HEADLESS_IDLE_SECS）。
//     消えていたら次の頼みで黙って立て直す
//   - 窓なしのゲームは実機でない扱い（セーブ・表示中の Doc の名乗り・遊んだ記録を書かない。engine 側の決まり）
// ログは <ゲーム>/debug/headless.log。
mod Headless {
    import java.io.{File => JFile}
    import java.lang.Exception
    import java.lang.{String => JavaString}
    import java.net.{ServerSocket, Socket, InetSocketAddress}
    import java.nio.file.Files
    import java.util.ArrayList
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.util.concurrent.Executors
    import java.util.concurrent.Callable
    import java.lang.ProcessBuilder

    /// プロジェクトのパス → ポート / 立ち上げ中の印。
    pub type alias State = JMap[String, String]

    pub def make(): State \ IO = new JMap()

    /// ゲームに置く入口の名前（engine の App.Headless の冒頭の書き方）。
    pub def entry(): String = "E2e.serveHeadless"

    /// 頼みの答え。
    ///   Ready(port)    … 頼める
    ///   Starting(why)  … 起こしている・待っている（画面は「準備中」を出して、少し後にもう一度頼む）
    ///   Unsupported(why) … このゲーム・この OS では起こせない
    pub enum Readiness with Eq, ToString {
        case Ready(Int32)
        case Starting(String)
        case Unsupported(String)
    }

    /// ゲームが入口（pub def serveHeadless）を持っているか。src の .flix を探す。
    pub def hasEntry(dir: String): Bool \ IO =
        flixFilesUnder(new JFile("${dir}/src"))
            |> List.exists(file -> String.contains(substr = "def serveHeadless()", readText(file)))

    def flixFilesUnder(dir: JFile): List[JFile] \ IO =
        let entries = dir.listFiles();
        if (Object.isNull(entries)) Nil
        else Array.toList(entries) |> List.flatMap(f ->
            if (f.isDirectory()) flixFilesUnder(f)
            else if (String.endsWith(suffix = ".flix", f.getName())) f :: Nil
            else Nil)

    def readText(file: JFile): String \ IO =
        try { new JavaString(Files.readAllBytes(file.toPath()), "UTF-8") } catch { case _: Exception => "" }

    /// 頼める状態にする。頼めればポート、まだなら裏で起こし始めて理由を返す。
    pub def ensure(state: State, game: Game.Runtime, dir: String): Readiness \ IO =
        if (EngineTasks.isWindows())
            Readiness.Unsupported("Windows ではまだ窓なしのゲームを起こせません（bin/headless-run が bash のため）")
        else if (not hasEntry(dir))
            Readiness.Unsupported("このゲームには窓なしの入口がありません（src に pub def serveHeadless() を置く。engine の App.Headless の冒頭）")
        else match aliveSince(state, dir) {
            case Some(port) => Readiness.Ready(port)
            case None =>
                if (Game.isStarting(game, dir))
                    Readiness.Starting("ゲームが起動の途中なので、窓なしのゲームは起動が済んでから起こします")
                else {
                    launchOnce(state, dir);
                    Readiness.Starting("窓なしのゲームを起こしています（初回は jar を作るので 1〜2 分）")
                }
        }

    /// 覚えているポートのうち、いま答える物だけ。
    def aliveSince(state: State, dir: String): Option[Int32] \ IO =
        let value: String = state.get("port|${dir}");
        if (Object.isNull(value)) None
        else Int32.fromString(value) |> Option.filter(answers)

    /// 繋がるか（何も頼まずに閉じる）。
    def answers(port: Int32): Bool \ IO =
        try {
            let socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 300);
            socket.close();
            true
        } catch {
            case _: Exception => false
        }

    /// まだ起こしていなければ裏で起こす（頼まれるたびに何本も起こさない門番）。
    def launchOnce(state: State, dir: String): Unit \ IO =
        let prev: String = state.putIfAbsent("starting|${dir}", "1");
        if (not Object.isNull(prev)) ()
        else {
            let port = freePort();
            discard state.put("port|${dir}", "${port}");
            let executor = Executors.newSingleThreadExecutor();
            discard executor.submit(new Callable[String] {
                def call(_this: Callable[String]): String \ IO = {
                    launch(dir, port);
                    // 終わった（放置で自分から終わった・落ちた）。次の頼みで立て直す。
                    discard state.remove("port|${dir}");
                    discard state.remove("starting|${dir}");
                    "done"
                }
            });
            executor.shutdown()
        }

    /// 起こして、終わるまで待つ。
    def launch(dir: String, port: Int32): Unit \ IO =
        try {
            let args = new ArrayList();
            discard args.add("bash");
            discard args.add("${EngineHome.dir()}/bin/headless-run");
            discard args.add(entry());
            let pb = new ProcessBuilder(args);
            discard pb.directory(new JFile(dir));
            discard pb.redirectErrorStream(true);
            discard new JFile("${dir}/debug").mkdirs();
            discard pb.redirectOutput(new JFile("${dir}/debug/headless.log"));
            let env = pb.environment();
            discard env.remove("DEVELOPER_DIR");
            discard env.remove("SDKROOT");
            discard env.put("HEADLESS_PORT", "${port}");
            discard env.put("HEADLESS_IDLE_SECS", "600");
            discard env.put("HEADLESS_JAVA_OPTS", "-Xmx1g");
            discard env.put("JAVA", EngineTasks.javaExe());
            let proc = pb.start();
            discard proc.waitFor()
        } catch {
            case _: Exception => ()
        }

    /// ログの末尾（起こせなかった時の理由に添える）。
    pub def logTail(dir: String, count: Int32): List[String] \ IO =
        let lines = String.lines(readText(new JFile("${dir}/debug/headless.log")));
        List.drop(List.length(lines) - count, lines)

    def freePort(): Int32 \ IO =
        let socket = new ServerSocket(0);
        let port = socket.getLocalPort();
        socket.close();
        port
}
