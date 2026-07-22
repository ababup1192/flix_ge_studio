// Scaffold — まっさらから始めるための 2 つの入口。
// 1) POST /projects/new: engine の `make new-game` を裏で走らせて新しいゲームを作る
//    (走行権とログは bake / game と別の器。進み具合は GET /projects/new/log)。
// 2) POST /scaffold/doc: 選択中プロジェクトに Doc の種 (スキーマ + データ + project.json 宣言)
//    を植える。project.json が読めないときは何も書かずに 500 で止まる (fail-closed) —
//    ここで壊れた宣言を上書きすると、直せたはずの手書きの内容を失うため。
// 名前の門番・宣言の追記・ファイル名の規約は純粋な関数に分けてある (そこだけテストする)。

mod Scaffold {
    import java.lang.System
    import java.io.{File => JFile}
    import java.io.FileOutputStream
    import java.lang.{String => JavaString}
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JString, JNumber, JBool}

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// プロジェクト名・Doc の種類名の門番: 小文字始まり、小文字/数字/_ のみ (^[a-z][a-z0-9_]*$)。
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

    /// Doc のデータファイル名: 指定が無ければ "<kind>.kind.json"。
    /// 指定があれば 1 語の *.json のみ許す (*.schema.json はスキーマの席なので断る)。
    pub def docFileNameOf(kind: String, given: Option[String]): Option[String] =
        match given {
            case None       => Some("${kind}.kind.json")
            case Some(name) =>
                if (Gallery.isSafeName(name)
                        and String.endsWith(suffix = ".json", name)
                        and not String.endsWith(suffix = ".schema.json", name))
                    Some(name)
                else
                    None
        }

    /// project.json (解析済み) の editor.resources へ宣言 1 件を後ろに足す (純粋)。
    /// editor / resources が無ければ作る。形が違う (トップや editor がオブジェクトでない・
    /// resources が配列でない) ときは Err — 黙って潰すと手書きの宣言が消える。
    pub def appendResource(kind: String, fileName: String, title: String, role: String,
                           projectJson: Json): Result[String, Json] =
        forM (
            topObj    <- JsonCodec.expectObject(projectJson)
                             |> Option.toOk("project.json のトップがオブジェクトではありません");
            editorObj <- match Map.get("editor", topObj) {
                case None       => Ok(Map#{})
                case Some(json) => JsonCodec.expectObject(json)
                                       |> Option.toOk("project.json の editor がオブジェクトではありません")
            };
            resources <- match Map.get("resources", editorObj) {
                case None                => Ok(Nil)
                case Some(JArray(items)) => Ok(Vector.toList(items))
                case Some(_)             => Err("project.json の editor.resources が配列ではありません")
            }
        ) yield {
            let decl = JObject(Map#{
                "id"      => JString(kind),
                "pattern" => JString("assets/${fileName}"),
                "title"   => JString(title),
                "schema"  => JString("assets/${kind}.schema.json"),
                "role"    => JString(role)
            });
            let newResources = JArray(List.toVector(List.append(resources, decl :: Nil)));
            let newEditor = JObject(Map.insert("resources", newResources, editorObj));
            JObject(Map.insert("editor", newEditor, topObj))
        }

    /// スキーマの種 (sections 方言)。見本のフィールド sample を 1 つだけ持つ。
    /// description は Atelier の手引き (slotHint) がそのまま拾う一行になる。
    pub def schemaSeed(kind: String, title: String): Json =
        JObject(Map#{
            "description" => JString("${title} の Doc の形の宣言(Studio の sections 方言)。育て方: まず sample を実際の調整値の名前に改名して label / min / max / default を仕事に合わせ、フィールドが増えたら sections にキーを足して order を進める。振る舞い(ルール)はコードに置き、ここには見た目と数値だけを書く。kind は ${kind}。"),
            "version"  => JNumber(JsonCodec.intToBd(1)),
            "sections" => JObject(Map#{
                "sample" => JObject(Map#{
                    "kind"    => JString("field"),
                    "type"    => JString("float"),
                    "label"   => JString("例の値(改名して使う)"),
                    "order"   => JNumber(JsonCodec.intToBd(1)),
                    "min"     => JNumber(JsonCodec.intToBd(0)),
                    "max"     => JNumber(JsonCodec.intToBd(100)),
                    "default" => JNumber(JsonCodec.intToBd(50))
                })
            })
        })

    /// データファイルの種。スキーマの例のフィールドの default と同じ値を持って生まれる。
    pub def docSeed(title: String): Json =
        JObject(Map#{
            "version" => JNumber(JsonCodec.intToBd(1)),
            "note"    => JString("${title} の Doc。スキーマを育ててから値を足す"),
            "sample"  => JNumber(JsonCodec.intToBd(50))
        })

    // ── 新しいゲーム (POST /projects/new) ─────────────────

    /// engine リポの場所: EDITOR_ENGINE か既定の $HOME/Desktop/flix_game_engine。
    /// (Genesis のジャンルカードの絵も同じ解決を使う)
    pub def engineDir(): String \ IO =
        let value = System.getenv("EDITOR_ENGINE");
        if (Object.isNull(value) or value == "")
            "${System.getProperty("user.home")}/Desktop/flix_game_engine"
        else
            value

    /// 置き場所の親: body の parent か既定の $HOME/Desktop。
    def parentOf(fields: Map[String, Json]): String \ IO =
        Map.get("parent", fields)
            |> Option.flatMap(JsonCodec.expectString)
            |> Option.getWithDefault("${System.getProperty("user.home")}/Desktop")

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
                if (not isName(name))
                    (400, failJson("不正な名前です (小文字始まり・小文字/数字/_ のみ): ${name}"))
                else if (String.trim(title) == "")
                    (400, failJson("title を入れてください"))
                else if (not (sizeOk(w) and sizeOk(h)))
                    (400, failJson("w / h は 64〜1024 で指定してください (w=${w}, h=${h})"))
                else
                    startNewChecked(state, parentOf(fields), name, title, w, h)
        }

    /// 名前・大きさの検分が済んだ後の、置き場所と engine の検分 → 起動。
    def startNewChecked(state: Runner.State, parent: String, name: String, title: String, w: Int32, h: Int32): (Int32, Json) \ IO =
        let gameDir = "${parent}/${name}";
        let engine = engineDir();
        if (not new JFile(parent).isDirectory())
            (400, failJson("親ディレクトリがありません: ${parent}"))
        else if (new JFile(gameDir).exists())
            (409, failJson("すでにあります: ${gameDir}"))
        else if (not new JFile("${engine}/Makefile").isFile())
            (500, failJson("engine が見つかりません (Makefile 無し): ${engine} — EDITOR_ENGINE で場所を教えてください"))
        else {
            let (status, json) = Runner.launchMake(state, engine, "new-game",
                "GAME=${gameDir}" :: "NAME=${name}" :: "TITLE=${title}" :: "W=${w}" :: "H=${h}" :: Nil,
                busy = "新しいゲームを作っている最中です");
            // 産まれる場所を web に教える — 完了後の「開く」はこの dir を使う
            // (ログの target は make のターゲット名であってパスではない)。
            let withDir = match json {
                case JObject(fields) => JObject(Map.insert("dir", JString(gameDir), fields))
                case other           => other
            };
            (status, withDir)
        }

    // ── Doc の種 (POST /scaffold/doc) ─────────────────────

    /// 応答 (status, body)。スキーマ + データを assets/ に作り、project.json に宣言を足す。
    /// どちらかのファイルがすでにあれば 409 (手書きの内容を潰さない)。
    pub def doc(dir: String, bodyText: String): (Int32, Json) \ IO =
        match JsonCompat.parse(bodyText) |> Option.flatMap(JsonCodec.expectObject) {
            case None => (400, failJson("body は {kind, title, role} の JSON で指定してください"))
            case Some(fields) =>
                let kind = Map.get("kind", fields) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("");
                let title = Map.get("title", fields) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("");
                let role = Map.get("role", fields) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("tuning");
                let fileNameOpt = docFileNameOf(kind, Map.get("fileName", fields) |> Option.flatMap(JsonCodec.expectString));
                if (not isName(kind))
                    (400, failJson("不正な kind です (小文字始まり・小文字/数字/_ のみ): ${kind}"))
                else if (String.trim(title) == "")
                    (400, failJson("title を入れてください"))
                else if (role != "material" and role != "tuning")
                    (400, failJson("role は material か tuning で指定してください: ${role}"))
                else match fileNameOpt {
                    case None           => (400, failJson("不正な fileName です (1 語の *.json のみ、*.schema.json は不可)"))
                    case Some(fileName) => docChecked(dir, kind, title, role, fileName)
                }
        }

    /// 入力の検分が済んだ後の、実在確認 → project.json 変換 → 書き込み。
    /// project.json の変換に通ってから初めてファイルを書く (途中まで植えて止まる事故を避ける)。
    def docChecked(dir: String, kind: String, title: String, role: String, fileName: String): (Int32, Json) \ IO =
        let schemaRel = "assets/${kind}.schema.json";
        let dataRel = "assets/${fileName}";
        if (new JFile("${dir}/${schemaRel}").exists())
            (409, failJson("すでにあります: ${schemaRel}"))
        else if (new JFile("${dir}/${dataRel}").exists())
            (409, failJson("すでにあります: ${dataRel}"))
        else match JsonCompat.parse(readText("${dir}/project.json")) {
            case None => (500, failJson("project.json が JSON として読めません — 宣言を足せないので何も書きませんでした"))
            case Some(projectJson) => match appendResource(kind, fileName, title, role, projectJson) {
                case Err(message) => (500, failJson("${message} — 何も書きませんでした"))
                case Ok(newProject) => {
                    discard new JFile("${dir}/assets").mkdirs();
                    writeJson("${dir}/${schemaRel}", schemaSeed(kind, title));
                    writeJson("${dir}/${dataRel}", docSeed(title));
                    writeJson("${dir}/project.json", newProject);
                    (200, JObject(Map#{
                        "ok"         => JBool(true),
                        "files"      => JArray(Vector#{JString(schemaRel), JString(dataRel), JString("project.json")}),
                        "wirePrompt" => JString(Atelier.wirePrompt(dir, dataRel))
                    }))
                }
            }
        }

    // ── ファイルの読み書き ────────────────────────────────

    def readText(path: String): String \ IO =
        let file = new JFile(path);
        if (file.isFile()) new JavaString(Files.readAllBytes(file.toPath()), "UTF-8") else ""

    def writeJson(path: String, json: Json): Unit \ IO =
        let out = new FileOutputStream(path);
        out.write((Util.Json.toPrettyString(2, json) + "\n").getBytes("UTF-8"));
        out.close()

    /// 失敗応答の本文。error を {message} の形にするのは Runner・Atelier と同じ読み方。
    def failJson(message: String): Json =
        JObject(Map#{
            "ok"    => JBool(false),
            "error" => JObject(Map#{"message" => JString(message)})
        })
}
