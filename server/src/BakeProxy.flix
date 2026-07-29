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
    import java.lang.Exception
    import java.lang.{String => JavaString}
    import java.net.{Socket, InetSocketAddress}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool}

    /// 焼きの待ち時間の上限(長尺で ~70 秒。余裕を見て 3 分)。
    def timeoutMs(): Int32 = 180000

    /// POST /bake/proxy — body {url, file?}。応答は焼き係の JSON をそのまま
    /// {ok:true, body:…} に包む。届かなければ {ok:false, reachable:false}。
    pub def respond(body: String): (Int32, Json) \ IO =
        match parseRequest(body) {
            case None => (400, errorJson("body must be a JSON object {url, file?}"))
            case Some((url, file)) => match target(url) {
                case None => (400, errorJson("bakeUrl must point at 127.0.0.1: ${url}"))
                case Some((port, path)) =>
                    let suffix = match file {
                        case Some(f) if f != "" => "?file=${f}"
                        case _                  => ""
                    };
                    match post(port, "${path}${suffix}") {
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

    /// {url, file?} を解く。
    def parseRequest(body: String): Option[(String, Option[String])] =
        JsonCompat.parse(body)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(obj ->
                Map.get("url", obj)
                    |> Option.flatMap(JsonCodec.expectString)
                    |> Option.map(url ->
                        (url, Map.get("file", obj) |> Option.flatMap(JsonCodec.expectString))))

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
