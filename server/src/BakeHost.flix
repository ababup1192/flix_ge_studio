// BakeHost — プロジェクトごとの「常駐する焼き係」の世話係。
//
// 焼き係はゲーム自身のコードで作ったプロセス (make bake-server)。エディタが立ち上げて
// 生かしておき、つまみが動くたびに「この値で焼いて」と頼む。焼くのがゲーム自身なので
// **エディタで聴いた音と実機の音が同じ**になる — ここに合成の写しを置くと、
// そちらが古びたときに黙って食い違う。
//
// 立ち上げには 100 秒ほどかかる (コンパイル + JVM)。だから頼まれた時に待たせず、
// 「いま立ち上げ中」を返して先に進ませる。2 回目以降は 50ms ほどで焼ける。
// 焼き係は放置されると自分から終わる (BakeService) ので、消えていたら黙って立て直す。
mod BakeHost {
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

    use Util.Json.Json
    use Util.Json.Json.{JObject, JBool, JString}

    /// プロジェクトのパス → 焼き係のポート / 立ち上げ中の印。
    pub type alias State = JMap[String, String]

    pub def make(): State \ IO = new JMap()

    /// 焼き係に頼む口の名前 (ゲーム側の Makefile がこの名前で用意する)。
    def target(): String = "bake-server"

    /// そのプロジェクトが焼き係を持てるか。project.json の studioTasks 宣言が最優先
    /// (両 OS で起こせる)。宣言が無ければ Makefile の口を見る — こちらは make で起こす
    /// 道なので、make を持たない Windows では「持てない」に倒す (永遠の準備中を防ぐ)。
    pub def canHost(dir: String): Bool \ IO =
        Option.nonEmpty(EngineTasks.declaredEntry(dir, target()))
            or (not EngineTasks.isWindows() and makefileHasTarget(dir))

    /// Makefile に bake-server: の口があるか。
    def makefileHasTarget(dir: String): Bool \ IO =
        try {
            let file = new JFile("${dir}/Makefile");
            if (not file.isFile()) false
            else {
                let text = new JavaString(Files.readAllBytes(file.toPath()), "UTF-8");
                String.contains(substr = "\n${target()}:", text)
            }
        } catch {
            case _: Exception => false
        }

    /// いま覚えているポート (立ち上がっていなければ None)。
    pub def portOf(state: State, dir: String): Option[Int32] \ IO =
        let value: String = state.get("port|${dir}");
        if (Object.isNull(value)) None else Int32.fromString(value)

    /// 焼き係へ 1 音ぶん頼む。
    /// 立ち上がっていなければ裏で立ち上げ始め、(202, 立ち上げ中) を返す。
    /// 焼けたら (200, WAV のバイト列)。
    pub def preview(state: State, dir: String, name: String, body: String):
            (Int32, Result[Json, Array[Int8, Static]]) \ IO =
        if (not canHost(dir))
            (501, Err(failJson("このゲームには描き出しサーバの入口 (make ${target()}) がありません")))
        else match aliveePort(state, dir) {
            case Some(port) => match post(port, name, body) {
                case Some(bytes) => (200, Ok(bytes))
                case None        => {
                    // 焼き係が消えていた (放置で自分から終わった等)。忘れて立て直す。
                    forget(state, dir);
                    ensure(state, dir);
                    (202, Err(startingJson()))
                }
            }
            case None => {
                ensure(state, dir);
                (202, Err(startingJson()))
            }
        }

    def startingJson(): Json =
        JObject(Map#{ "ok" => JBool(false), "starting" => JBool(true),
                      "message" => JString("描き出しサーバを立ち上げています（初回だけ 1〜2 分）") })

    def failJson(message: String): Json =
        JObject(Map#{ "ok" => JBool(false), "message" => JString(message) })

    /// 焼き係がもう焼ける状態か（画面の「準備中」表示の材料）。
    pub def ready(state: State, dir: String): Bool \ IO =
        Option.nonEmpty(aliveePort(state, dir))

    /// 覚えているポートのうち、いま生きている物だけ返す。
    def aliveePort(state: State, dir: String): Option[Int32] \ IO =
        portOf(state, dir) |> Option.filter(port -> health(port))

    /// 素の Socket で 1 往復する (依存もキャストも増やさず「書いて全部読む」で足りる)。
    /// 応答は Content-Length つき + Connection: close なので、読み切りで終わる。
    def health(port: Int32): Bool \ IO =
        try {
            let socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 400);
            socket.setSoTimeout(800);
            let request = "GET /health HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\n"
                + "Connection: close\r\n\r\n";
            socket.getOutputStream().write(request.getBytes("UTF-8"));
            socket.getOutputStream().flush();
            let bytes = socket.getInputStream().readAllBytes();
            socket.close();
            String.startsWith(prefix = "HTTP/1.1 200", new JavaString(bytes, "UTF-8"))
        } catch {
            case _: Exception => false
        }

    /// 1 音ぶん焼いてもらう。届かなければ None (呼び手が立て直す)。
    def post(port: Int32, name: String, body: String): Option[Array[Int8, Static]] \ IO =
        try {
            let payload = body.getBytes("UTF-8");
            let socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 1000);
            socket.setSoTimeout(20000);
            let head = "POST /sfx/preview?name=${name} HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\n"
                + "Content-Type: application/json\r\n"
                + "Content-Length: ${Array.length(payload)}\r\nConnection: close\r\n\r\n";
            let out = socket.getOutputStream();
            out.write(head.getBytes("UTF-8"));
            out.write(payload);
            out.flush();
            // 相手(BakeService)は Content-Length を付けた後も繋ぎっぱなしにする。
            // EOF を待つと、届いている音を待ちぼうけのまま捨ててしまう
            let input = socket.getInputStream();
            let answer = BakeProxy.readHeader(input)
                |> Option.flatMap(header ->
                    if (not String.startsWith(prefix = "HTTP/1.1 200", header)) None
                    else Some(BakeProxy.readBodyBytes(input, BakeProxy.contentLength(header))));
            socket.close();
            answer
        } catch {
            case _: Exception => None
        }

    /// 焼き係を裏で立ち上げる。すでに立ち上げ中なら何もしない
    /// (つまみを動かすたびに何本も起こさないための門番)。
    pub def ensure(state: State, dir: String): Unit \ IO =
        let prev: String = state.putIfAbsent("starting|${dir}", "1");
        if (not Object.isNull(prev)) ()
        else {
            let port = freePort();
            discard state.put("port|${dir}", "${port}");
            let executor = Executors.newSingleThreadExecutor();
            discard executor.submit(new Callable[String] {
                def call(_this: Callable[String]): String \ IO = {
                    launch(dir, port);
                    discard state.remove("starting|${dir}");
                    "done"
                }
            });
            executor.shutdown()
        }

    /// 覚えていたポートを忘れる (焼き係が消えたとき)。
    pub def forget(state: State, dir: String): Unit \ IO =
        discard state.remove("port|${dir}");
        discard state.remove("starting|${dir}")

    /// 空いているポートを 1 つ取る (取った直後に閉じるので、焼き係が同じ番号を掴める)。
    def freePort(): Int32 \ IO =
        let socket = new ServerSocket(0);
        let port = socket.getLocalPort();
        socket.close();
        port

    /// 焼き係を起こす。入口の宣言 (studioTasks の bake-server) があれば java を直接呼び、
    /// 無ければ従来の `make bake-server` (macOS のみ)。焼き係は放置で自分から終わるので、
    /// ここでは終わるまで待つだけ (待ち終えたら次の頼みごとで立て直される)。
    def launch(dir: String, port: Int32): Unit \ IO =
        match (EngineTasks.declaredEntry(dir, target()), EngineTasks.exec()) {
            case (Some(entry), Some(exec)) =>
                launchWith(dir, port,
                    EngineTasks.argv(exec, EngineTasks.isMac(), EngineTasks.Task.EntryPoint(entry)),
                    withEngine = false)
            case _ =>
                if (EngineTasks.isWindows())
                    // canHost は宣言を見て true と言ったのに flix.jar が無い = 同梱の破損。
                    // 黙って何もしないと「永遠の準備中」になるので、理由をログに書き残す。
                    note(dir, "engine の bin/flix.jar が見つからないため描き出しサーバを起こせません (同梱が壊れています)")
                else launchWith(dir, port, "make" :: target() :: Nil, withEngine = true)
        }

    /// 焼き係のログファイルに理由を 1 行残す (起こせなかったときの読み先を揃える)。
    def note(dir: String, message: String): Unit \ IO =
        try {
            discard new JFile("${dir}/debug").mkdirs();
            let _ = run { Fs.WriteFile.write(str = "[bake-host] ${message}\n", "${dir}/debug/bake-server.log") }
                with Fs.WriteFile.runWithFileWrite
                with Fs.FileWrite.runWithIO;
            ()
        } catch {
            case _: Exception => ()
        }

    /// コマンド列で焼き係を起こして待つ。ENGINE はゲームの Makefile が engine の場所を
    /// 知るための環境変数なので、make の道だけに渡す。
    def launchWith(dir: String, port: Int32, cmdArgs: List[String], withEngine: {withEngine = Bool}): Unit \ IO =
        try {
            let args = new ArrayList();
            foreach (arg <- cmdArgs) discard args.add(arg);
            let pb = new ProcessBuilder(args);
            discard pb.directory(new JFile(dir));
            discard pb.redirectErrorStream(true);
            // ログ先の debug/ はテンプレ外のゲームだと無いことがある — 無いままだと
            // 起こす前に例外で無言に終わるので、先に作っておく。
            discard new JFile("${dir}/debug").mkdirs();
            discard pb.redirectOutput(new JFile("${dir}/debug/bake-server.log"));
            let env = pb.environment();
            discard env.remove("DEVELOPER_DIR");
            discard env.remove("SDKROOT");
            discard env.put("BAKE_PORT", "${port}");
            if (withEngine#withEngine)
                match EngineHome.forGames() {
                    case Some(engine) => discard env.put("ENGINE", engine)
                    case None         => ()
                }
            else ();
            let proc = pb.start();
            discard proc.waitFor()
        } catch {
            case _: Exception => ()
        }
}
