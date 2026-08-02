// Scaffold — まっさらから始める入口 (POST /projects/new)。
// engine の `make new-game` を裏で走らせて新しいゲームを作る
// (走行権とログは bake / game と別の器。進み具合は GET /projects/new/log)。
// 名前・大きさの門番は純粋な関数に分けてある (そこだけテストする)。

mod Scaffold {
    import java.lang.System
    import java.io.{File => JFile}

    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool}

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// プロジェクト名の門番: 小文字始まり、小文字/数字/_ のみ (^[a-z][a-z0-9_]*$)。
    pub def isName(name: String): Bool =
        match String.toList(name) {
            case Nil          => false
            case head :: rest =>
                (head >= 'a' and head <= 'z')
                    and List.forAll(c -> (c >= 'a' and c <= 'z') or Char.isDigit(c) or c == '_', rest)
        }

    /// 画面の設計解像度として許す幅・高さ (64..1024)。
    pub def sizeOk(v: Int32): Bool =
        v >= 64 and v <= 1024

    // ── 新しいゲーム (POST /projects/new) ─────────────────

    /// engine の場所 (Genesis のジャンルカードの絵も同じ解決を使う)。
    pub def engineDir(): String \ IO = EngineHome.dir()

    /// 置き場所の親: body の parent → 覚えているワークスペース → 既定の $HOME/Desktop。
    /// ワークスペースを決めてあれば、新しいゲームは黙ってそこに産まれる (Godot と同じ流儀)。
    def parentOf(fields: Map[String, Json]): String \ IO =
        match Map.get("parent", fields) |> Option.flatMap(JsonCodec.expectString) {
            case Some(parent) => parent
            case None         =>
                Projects.workspace()
                    |> Option.getWithDefault("${System.getProperty("user.home")}/Desktop")
        }

    /// 応答 (status, body)。検分に全部通ったら engine の `make new-game` を裏で起こして 202。
    /// 走っている最中の重ね掛けは launchMake の走行権が 409 で断る。
    pub def startNew(state: Runner.State, bodyText: String): (Int32, Json) \ IO =
        match JsonCompat.parse(bodyText) |> Option.flatMap(JsonCodec.expectObject) {
            case None => (400, failJson("body は {name, title, w, h} の JSON で指定してください"))
            case Some(fields) =>
                let name = Map.get("name", fields) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("");
                let title = Map.get("title", fields) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("");
                let w = JsonCodec.intOr("w", 0, fields);
                let h = JsonCodec.intOr("h", 0, fields);
                // starter = 複製元の公式テンプレート (空 = 既定の game-starter)。
                // 白名簿 (Genesis の登録) の外は通さない。サイズはテンプレートが決める。
                let starter = Map.get("starter", fields)
                    |> Option.flatMap(JsonCodec.expectString)
                    |> Option.getWithDefault("");
                if (not isName(name))
                    (400, failJson("不正な名前です (小文字始まり・小文字/数字/_ のみ): ${name}"))
                else if (String.trim(title) == "")
                    (400, failJson("title を入れてください"))
                else if (starter != "" and not Genesis.isStarter(starter))
                    (400, failJson("知らないテンプレートです: ${starter}"))
                else if (starter == "" and not (sizeOk(w) and sizeOk(h)))
                    (400, failJson("w / h は 64〜1024 で指定してください (w=${w}, h=${h})"))
                else
                    startNewChecked(state, parentOf(fields), name, title, w, h, starter)
        }

    /// 名前・大きさの検分が済んだ後の、置き場所と engine の検分 → 起動。
    /// 産むのは NewGame (server 自身の手)。engine の flix.jar が無い手元開発では
    /// macOS に限り従来の `make new-game` に倒れる (Windows に make は無い)。
    def startNewChecked(state: Runner.State, parent: String, name: String, title: String, w: Int32, h: Int32, starter: String): (Int32, Json) \ IO =
        let gameDir = "${parent}/${name}";
        let engine = engineDir();
        if (not new JFile(parent).isDirectory())
            (400, failJson("親ディレクトリがありません: ${parent}"))
        else if (new JFile(gameDir).exists())
            (409, failJson("すでにあります: ${gameDir}"))
        else if (not new JFile("${engine}/templates").isDirectory())
            (500, failJson("engine が見つかりません (templates 無し): ${engine} — EDITOR_ENGINE で場所を教えてください"))
        else {
            let (status, json) = launchCreate(state, engine, gameDir, name, title, w, h, starter);
            // 産まれる場所を web に教える — 完了後の「開く」はこの dir を使う
            // (ログの target は仕事の名前であってパスではない)。
            let withDir = match json {
                case JObject(fields) => JObject(Map.insert("dir", JString(gameDir), fields))
                case other           => other
            };
            (status, withDir)
        }

    /// 産み方を選んで裏で起こす。starter 指定はテンプレ名だけ違い、W/H はテンプレの
    /// 既定 (トークン札が無いので書き換え自体が空振りして無害)。
    def launchCreate(state: Runner.State, engine: String, gameDir: String, name: String, title: String, w: Int32, h: Int32, starter: String): (Int32, Json) \ IO =
        let busy = "新しいゲームを作っている最中です";
        match EngineTasks.exec() {
            case Some(_) => {
                let template = if (starter == "") "game-starter"
                               else String.dropLeft(String.length("templates/"), starter);
                let request: NewGame.Request = {
                    engine = engine, gameDir = gameDir, name = name, title = title,
                    w = if (w == 0) 480 else w, h = if (h == 0) 300 else h,
                    template = template
                };
                Runner.launchWork(state, "new-game", () -> NewGame.create(state, request), busy = busy)
            }
            case None =>
                if (EngineTasks.isWindows())
                    (500, failJson("engine の bin/flix.jar が見つかりません: ${engine} — 同梱が壊れています"))
                else {
                    let args =
                        if (starter == "")
                            "GAME=${gameDir}" :: "NAME=${name}" :: "TITLE=${title}" :: "W=${w}" :: "H=${h}" :: Nil
                        else
                            "GAME=${gameDir}" :: "NAME=${name}" :: "TITLE=${title}"
                                :: "TEMPLATE=${String.dropLeft(String.length("templates/"), starter)}" :: Nil;
                    Runner.launchMake(state, engine, "new-game", args, busy = busy)
                }
        }

    /// 失敗応答の本文。error を {message} の形にするのは Runner・Atelier と同じ読み方。
    def failJson(message: String): Json =
        JObject(Map#{
            "ok"    => JBool(false),
            "error" => JObject(Map#{"message" => JString(message)})
        })
}
