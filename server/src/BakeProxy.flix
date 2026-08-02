// BakeProxy — 焼き係(ゲーム側の常駐サーバ)への取り次ぎ。
//
// ブラウザから別ポートへ直に投げると、出所の違い(CORS)と .app の中の事情で
// 届いたり届かなかったりする。Studio のサーバが 1 枚噛んで、同じ出所のまま
// 往復させる。
//
// 宛先は project.json の宣言(bakeUrl)から来る文字列だが、**127.0.0.1 だけ**を
// 通す。宣言はプロジェクトの持ち物とはいえ、任意の URL へ POST できる口を
// 開けたままにはしない。
//
// 焼きは 20〜70 秒かかる。届かない時(サーバが起きていない)は落とさず
// reachable:false を返し、画面が「make cutscene-server で起動してください」と
// 案内できるようにする。
mod BakeProxy {
    import java.io.InputStream
    import java.io.{File => JFile}
    import java.lang.Exception
    import java.lang.ProcessBuilder
    import java.lang.System
    import java.lang.Thread
    import java.util.ArrayList
    import java.util.concurrent.{ConcurrentHashMap => JMap}
    import java.lang.{String => JavaString}
    import java.net.{Socket, InetSocketAddress}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool, JNumber}

    /// 「起こしている最中」の覚え書き (url → 起こし始めた時刻 ms)。
    /// 取り次ぎはリクエストごとの並行スレッドで来るので、この覚え書きが無いと
    /// 焼き係のコンパイル中 (分の単位で不健康) にクリックのたび起こし直しが走り、
    /// java プロセスが積み上がる。Editor.serve が 1 回だけ作って引き回す。
    pub type alias Guard = JMap[String, String]

    pub def makeGuard(): Guard \ IO = new JMap()

    /// 起こし直しを許すまでの猶予 (起こしても health が返らないままこれだけ経ったら、
    /// 前の手は死んだとみなしてもう一度だけ撃てる)。
    def relaunchAfterMs(): Int64 = 600000i64

    /// 焼きの待ち時間の上限(長尺で ~70 秒。余裕を見て 3 分)。
    def timeoutMs(): Int32 = 180000

    /// 取り次ぎの直前に、落ちていた時だけ待つ短い猶予。長い待ちは画面側の
    /// 訊き直しが受け持つ(1 本の往復で何分も抱え込まない)。
    def graceMs(): Int32 = 8000

    /// POST /bake/proxy — body {url, file?, path?, query?}。
    /// path は宣言の口に足す小道(例 "/frame")、query は "cut=5&draft=1" の形。
    /// 応答は焼き係の JSON をそのまま {ok:true, body:…} に包む。
    /// 届かなければ {ok:false, reachable:false}。
    /// POST /bake/wake — body {url, launch?}。**待たずにすぐ返す**:
    ///   (a) 繋がるか確かめる
    ///   (b) 繋がらず launch なら、宣言の手を 1 度だけ撃つ(冪等なので再訪も安全)
    ///   (c) 今の様子をそのまま返す
    /// 長い待ち(初回はコンパイルで数分)は画面側が 2 秒おきに訊き直す —
    /// 1 本の往復で数分を抱え込むと、進んでいるのか固まったのかが誰にも見えない。
    /// **コマンドは画面から受け取らない**(走らせてよい手は宣言だけ)。
    pub def wake(guard: Guard, dir: String, body: String): (Int32, Json) \ IO =
        match parseRequest(body) {
            case None => (400, errorJson("body must be a JSON object {url, launch?}"))
            case Some(req) => match target(req#url) {
            case None => (400, errorJson("bakeUrl must point at 127.0.0.1"))
            case Some((port, _)) =>
                if (healthy(port))
                    (200, wokeJson(true, false, "既に起きています"))
                else if (not (req#launch))
                    (200, wokeJson(false, false, "まだ起きていません(立ち上がりを待っています)"))
                else match bakeCmdOf(dir, req#url) {
                    case None => (200, JObject(Map#{
                        "ok"        => JBool(false),
                        "reachable" => JBool(false),
                        "needsCmd"  => JBool(true),
                        "log"       => JString("起こし方(bakeCmd)が宣言されていません")
                    }))
                    case Some(cmd) => {
                        let started = if (shouldLaunch(guard, req#url)) launch(dir, cmd) else true;
                        (200, wokeJson(false, true,
                            if (started) "『${cmd}』で起こしています" else "『${cmd}』を起こせませんでした"))
                    }
                }
            }
        }

    def wokeJson(reachable: Bool, woke: Bool, log: String): Json =
        JObject(Map#{
            "ok"        => JBool(reachable),
            "reachable" => JBool(reachable),
            "woke"      => JBool(woke),
            "log"       => JString(log)
        })

    /// 起こして待つ(宣言が無ければ何もしない)。起こすのは覚え書きの上で 1 回だけ —
    /// コンパイル中に並行して来た取り次ぎは、撃ち直さずに health を待つだけ。
    def ensureUp(guard: Guard, dir: String, url: String, port: Int32): Unit \ IO =
        match bakeCmdOf(dir, url) {
            case None      => ()
            case Some(cmd) => {
                if (shouldLaunch(guard, url)) discard launch(dir, cmd) else ();
                discard waitForHealth(port, 0);
                // health が返った = 起こし終わり。覚え書きを消して、次に死んだ時
                // また 1 回だけ起こせるようにする
                if (healthy(port)) discard guard.remove(launchKey(url)) else ()
            }
        }

    def launchKey(url: String): String = "launching|${url}"

    /// いま起こしてよいか。putIfAbsent が入れ物ごと排他になるので、同時に来た
    /// スレッドのうち 1 本だけが true を得る。前の手が relaunchAfterMs 経っても
    /// health を返せていなければ、死んだとみなして 1 本だけ取り直せる。
    def shouldLaunch(guard: Guard, url: String): Bool \ IO =
        let now = System.currentTimeMillis();
        let prev: String = guard.putIfAbsent(launchKey(url), "${now}");
        if (Object.isNull(prev))
            true
        else
            match Int64.fromString(prev) {
                case Some(since) if now - since > relaunchAfterMs() =>
                    guard.replace(launchKey(url), prev, "${now}")
                case _ => false
            }

    /// 「起こす手」は、その口(bakeUrl / performUrl)を宣言している資源の物を使う。
    /// 画面から渡させない — 走らせてよい手はプロジェクト自身の宣言だけ。
    def bakeCmdOf(dir: String, url: String): Option[String] \ IO =
        match FileIndex.readInside(dir, "project.json") {
            case Err(_)   => None
            case Ok(text) => match JsonCompat.parse(text) {
                case None       => None
                case Some(json) => match ResourceDecl.fromProjectJson(json) {
                    case Err(_)    => None
                    case Ok(decls) =>
                        decls#resources
                            |> List.filterMap(decl ->
                                if (decl#bakeUrl == Some(url)) decl#bakeCmd
                                else if (decl#performUrl == Some(url)) decl#performCmd
                                else None)
                            |> List.head
                }
            }
        }

    /// 宣言された手をプロジェクトの根で走らせる(シェル経由)。
    /// 出入りは覗かない — 起きたかどうかは health で確かめる。
    /// 宣言された手をプロジェクトの根で走らせ、**待たない**。
    ///
    /// WhyNot: 出入りを繋いだままにしない — 実機を起こす手(make debug)は
    /// 終わらないコマンドで、誰も読まないパイプが詰まった瞬間に相手が止まる。
    /// 書き出しはファイルへ逃がす(様子を見たい時の置き場にもなる)。
    def launch(dir: String, cmd: String): Bool \ IO =
        // 宣言の手はシェルの文字列 (make debug など Unix 前提)。Windows にはそのシェルも
        // make も無いので、走らせるふりをせず false = 「起こせなかった」に倒す。
        if (EngineTasks.isWindows()) false
        else try {
            let args = new ArrayList();
            discard args.add("/bin/sh");
            discard args.add("-lc");
            discard args.add(cmd);
            let pb = new ProcessBuilder(args);
            discard pb.directory(new JFile(dir));
            discard pb.redirectErrorStream(true);
            discard new JFile("${dir}/.studio").mkdirs();
            discard pb.redirectOutput(new JFile("${dir}/.studio/host.log"));
            discard pb.start();
            true
        } catch {
            case _: Exception => false
        }

    /// health が返るまで 0.5 秒おきに見に行く(上限まで)。返るのは待った時間。
    def waitForHealth(port: Int32, waited: Int32): Int32 \ IO =
        if (healthy(port)) waited
        else if (waited >= graceMs()) waited
        else {
            Thread.sleep(500i64);
            waitForHealth(port, waited + 500)
        }

    /// 生きているか(短い往復。閉じない相手でも頭だけ読んで判じる)。
    pub def healthy(port: Int32): Bool \ IO =
        try {
            let socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 300);
            socket.setSoTimeout(700);
            let head = "GET /health HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\n"
                + "Connection: close\r\n\r\n";
            socket.getOutputStream().write(head.getBytes("UTF-8"));
            socket.getOutputStream().flush();
            let alive = match readHeader(socket.getInputStream()) {
                case Some(header) => String.startsWith(prefix = "HTTP/1.1 200", header)
                case None         => false
            };
            socket.close();
            alive
        } catch {
            case _: Exception => false
        }

    pub def respond(guard: Guard, dir: String, body: String): (Int32, Json) \ IO =
        match parseRequest(body) {
            case None => (400, errorJson("body must be a JSON object {url, file?, path?, query?}"))
            case Some(req) => match target(req#url) {
                case None => (400, errorJson("bakeUrl must point at 127.0.0.1: ${req#url}"))
                case Some((port, path)) =>
                    if (not (safeBit(req#path)) or not (safeBit(req#args)))
                        (400, errorJson("path / query に使えない文字があります"))
                    else
                    let fileBit = match req#file {
                        case Some(f) if f != "" => "file=${f}"
                        case _                  => ""
                    };
                    let args = List.filter(bit -> bit != "", fileBit :: req#args :: Nil)
                        |> List.join("&");
                    let suffix = if (args == "") "" else "?${args}";
                    // 前段: 繋がらなければ宣言の手で起こしてから頼む(冪等でも、
                    // 繋がっている時は走らせない — 無駄なプロセスを増やさないため)
                    if (not (healthy(port))) ensureUp(guard, dir, req#url, port) else ();
                    match post(port, "${path}${req#path}${suffix}") {
                        case Some(text) => (200, JObject(Map#{
                            "ok"        => JBool(true),
                            "reachable" => JBool(true),
                            "body"      => JString(text)
                        }))
                        case None => (200, JObject(Map#{
                            "ok"        => JBool(false),
                            "reachable" => JBool(false),
                            "error"     => JString("焼き係に繋がりません")
                        }))
                    }
            }
        }

    /// {url, file?, path?, query?} を解く。
    def parseRequest(body: String): Option[{url = String, file = Option[String], path = String, args = String, launch = Bool}] =
        JsonCompat.parse(body)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj ->
                Map.get("url", obj)
                    |> Option.flatMap(JsonCodec.expectString)
                    |> Option.map(url -> {
                        url   = url,
                        file  = Map.get("file", obj) |> Option.flatMap(JsonCodec.expectString),
                        path  = Map.get("path", obj) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault(""),
                        args  = Map.get("query", obj) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault(""),
                        launch = Map.get("launch", obj) |> Option.flatMap(JsonCodec.expectBool) |> Option.getWithDefault(false)
                    }))

    /// 頼み事に混ぜてよい文字か(改行やスペースで別の要求を差し込ませない)。
    pub def safeBit(bit: String): Bool =
        not (String.contains(substr = "\r", bit)
                or String.contains(substr = "\n", bit)
                or String.contains(substr = " ", bit)
                or String.contains(substr = "..", bit))

    /// "http://127.0.0.1:8792/cutscene" → (8792, "/cutscene")。
    /// 127.0.0.1 (と localhost) 以外・http 以外は None。
    pub def target(url: String): Option[(Int32, String)] =
        let hosts = "http://127.0.0.1:" :: "http://localhost:" :: Nil;
        hosts
            |> List.filter(prefix -> String.startsWith(prefix = prefix, url))
            |> List.head
            |> Option.flatMap(prefix -> {
                let rest = String.dropLeft(String.length(prefix), url);
                let slash = String.indexOf(substr = "/", rest);
                let (portText, path) = match slash {
                    case Some(at) => (String.slice(start = 0, end = at, rest), String.dropLeft(at, rest))
                    case None     => (rest, "/")
                };
                Int32.fromString(portText) |> Option.map(port -> (port, path))
            })

    /// 素の Socket で 1 往復。
    ///
    /// WhyNot: 「閉じられるまで読む」(readAllBytes) にしない — 焼き係は
    /// Content-Length を付けて応答した後も繋ぎっぱなしにする(こちらの
    /// Connection: close を見ない)。EOF を待つと本文が届いているのに
    /// タイムアウトまで固まり、成功した焼きが「繋がりません」になる。
    /// 読み終わりは長さで決める: 頭を 1 バイトずつ空行まで読み、
    /// Content-Length のぶんだけ本文を取る(長さが無い時だけ EOF まで)。
    def post(port: Int32, path: String): Option[String] \ IO =
        try {
            let socket = new Socket();
            socket.connect(new InetSocketAddress("127.0.0.1", port), 1000);
            socket.setSoTimeout(timeoutMs());
            let head = "POST ${path} HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\n"
                + "Content-Length: 0\r\nConnection: close\r\n\r\n";
            let out = socket.getOutputStream();
            out.write(head.getBytes("UTF-8"));
            out.flush();
            let input = socket.getInputStream();
            let answer = readHeader(input)
                |> Option.flatMap(header ->
                    if (not String.startsWith(prefix = "HTTP/1.1 200", header)) None
                    else Some(readBody(input, contentLength(header))));
            socket.close();
            answer
        } catch {
            case _: Exception => None
        }

    /// 頭(空行まで)を 1 バイトずつ。本文へ食い込まないのでこの読み方にする。
    /// 同じ読み方が要る所(効果音の焼き係)からも使えるように開けてある。
    pub def readHeader(input: InputStream): Option[String] \ IO =
        readHead(input, Array.empty(Static, 8192), 0)

    /// 本文をバイトのまま読み切る(WAV のように文字でない応答のため)。
    pub def readBodyBytes(input: InputStream, length: Option[Int32]): Array[Int8, Static] \ IO =
        match length {
            case Some(n) => input.readNBytes(n)
            case None    => input.readAllBytes()
        }

    def readHead(input: InputStream, buf: Array[Int8, Static], at: Int32): Option[String] \ IO =
        if (at >= Array.length(buf))
            None
        else if (input.read(buf, at, 1) < 0)
            None
        else if (at >= 3
                and Array.get(at - 3, buf) == 13i8 and Array.get(at - 2, buf) == 10i8
                and Array.get(at - 1, buf) == 13i8 and Array.get(at, buf) == 10i8)
            Some(new JavaString(buf, 0, at + 1, "UTF-8"))
        else
            readHead(input, buf, at + 1)

    /// 本文。長さが分かっていればその分だけ(相手が閉じなくても読み終われる)。
    def readBody(input: InputStream, length: Option[Int32]): String \ IO =
        match length {
            case Some(n) => new JavaString(input.readNBytes(n), "UTF-8")
            case None    => new JavaString(input.readAllBytes(), "UTF-8")
        }

    /// 頭から Content-Length を読む(大小は問わない — 相手の書き方に合わせない)。
    pub def contentLength(header: String): Option[Int32] =
        String.splitOn(substr = "\r\n", header)
            |> List.filterMap(line -> {
                let lower = String.toLowerCase(line);
                if (String.startsWith(prefix = "content-length:", lower))
                    Int32.fromString(String.trim(String.dropLeft(String.length("content-length:"), lower)))
                else None
            })
            |> List.head

    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
