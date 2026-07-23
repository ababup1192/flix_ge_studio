// SpriteColors — POST /sprite/colors。ドット絵 (*.sprite.json) の legend の値が
// 意味色キー (テーマやパレットが持つ名前) のとき、編集画面だけでは実色が分からず、
// ゲーム画面と色が食い違う。ここでプロジェクト内の色票 (doc 自身の palette と
// *.theme.json) を引いて「legend の値 → #rrggbb」の表を返す。
// 解決できない値は表に載せない (fail-open — 画面側は従来の仮色で描き続ける)。
mod SpriteColors {
    use Util.Json.Json
    use Util.Json.Json.{JObject, JString, JBool}

    /// リクエストボディ {path?, doc?} を解いて (HTTP status, 応答 JSON) を作る。
    /// 封筒の壊れだけ 400。doc 側の問題 (パース失敗等) は 200 の ok:false —
    /// 編集画面はタイピング途中の壊れた JSON を送り得るので HTTP エラーにしない。
    pub def respond(dir: String, body: String): (Int32, Json) \ IO =
        match (JsonCompat.parse(body) |> Option.flatMap(JsonCodec.expectObject)) {
            case None      => (400, errorJson("body must be a JSON object {path?, doc?}"))
            case Some(obj) => match PreviewUi.docJsonOf(dir, obj) {
                case Err(PreviewUi.BadRequest.Envelope(message)) => (400, errorJson(message))
                case Err(PreviewUi.BadRequest.Doc(message))      => (200, errorJson(message))
                case Ok(docJson) =>
                    let docPath = Map.get("path", obj) |> Option.flatMap(JsonCodec.expectString);
                    (200, okJson(resolveAll(dir, docPath, docJson)))
            }
        }

    /// legend の値 → 実色の表 (IO 側の入口)。色票の勝ち順は
    /// 「他のテーマ < 同名テーマ (novel.sprite.json ↔ novel.theme.json) < doc 自身の palette」。
    def resolveAll(dir: String, docPath: Option[String], docJson: Json): Map[String, String] \ IO =
        tableOf(themeTops(dir, docPath), PreviewUi.resolvedPalette(dir, docJson), legendValuesOf(docJson))

    /// プロジェクト内の *.theme.json をトップレベル object として読む (後の物が勝つ順)。
    /// 読めない・形違いのテーマは黙って落とす — 表が痩せるだけで、編集は止めない。
    def themeTops(dir: String, docPath: Option[String]): List[Map[String, Json]] \ IO =
        let sibling = docPath |> Option.flatMap(path -> siblingThemeOf(path));
        let themes = FileIndex.listAllJson(dir)
            |> List.filter(path -> String.endsWith(suffix = ".theme.json", path));
        // 同名テーマを列の最後へ回す (tableOf は後勝ちなので、同名が他のテーマに勝つ)
        ((themes |> List.filter(path -> Some(path) != sibling))
            ::: (themes |> List.filter(path -> Some(path) == sibling)))
            |> List.filterMap(path -> FileIndex.readInside(dir, path)
                |> Result.toOption
                |> Option.flatMap(text -> JsonCompat.parse(String.trim(text)))
                |> Option.flatMap(JsonCodec.expectObject))

    /// "assets/novel.sprite.json" → "assets/novel.theme.json"。sprite Doc でないパスは None。
    pub def siblingThemeOf(path: String): Option[String] =
        if (String.endsWith(suffix = ".sprite.json", path))
            Some("${String.dropRight(String.length(".sprite.json"), path)}.theme.json")
        else None

    /// legend の文字列値たち (重複は 1 つに)。legend が無い・形違いは空。
    pub def legendValuesOf(docJson: Json): List[String] =
        JsonCodec.expectObject(docJson)
            |> Option.flatMap(top -> Map.get("legend", top))
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.map(legend -> legend
                |> Map.valuesOf
                |> List.filterMap(JsonCodec.expectString)
                |> List.distinct)
            |> Option.getWithDefault(Nil)

    /// 表づくりの純粋な芯。themeTops は「後の物が勝つ」順で受け、docPalette がさらに勝つ。
    /// legend の値そのものが色表記 ("#rrggbb") なら表引きせずそのまま解決する。
    pub def tableOf(themeTops: List[Map[String, Json]], docPalette: Map[String, String], legendValues: List[String]): Map[String, String] =
        let themeNames = themeTops |> List.foldLeft((acc, top) -> overlay(colorFieldsOf(top), acc), Map.empty());
        let names = overlay(docPalette, themeNames);
        legendValues
            |> List.filterMap(value -> resolveValue(names, value) |> Option.map(hex -> (value, hex)))
            |> List.toMap

    /// base の上に winner を重ねる (同名は winner 勝ち)。
    def overlay(winner: Map[String, String], base: Map[String, String]): Map[String, String] =
        Map.foldLeftWithKey((acc, name, hex) -> Map.insert(name, hex, acc), base, winner)

    /// テーマのトップレベルから、色として読めるフィールドだけを "#rrggbb" で拾う
    /// (note や rainCount のような色でない値はここで落ちる)。
    pub def colorFieldsOf(top: Map[String, Json]): Map[String, String] =
        top |> Map.toList
            |> List.filterMap(entry -> {
                let (name, json) = entry;
                PreviewUi.colorHexOf(json) |> Option.map(hex -> (name, hex))
            })
            |> List.toMap

    /// 値 1 つの解決: 色表記ならそのまま、"@名前" と裸の "名前" は色票を引く。
    /// どれでも解けなければ None (表に載せない)。
    def resolveValue(names: Map[String, String], value: String): Option[String] =
        match PreviewUi.colorHexOf(JString(value)) {
            case Some(hex) => Some(hex)
            case None      =>
                let name = if (String.startsWith(prefix = "@", value)) String.drop(1, value) else value;
                Map.get(name, names)
        }

    /// 成功応答: {ok: true, colors: {"値": "#rrggbb", ...}}。
    def okJson(colors: Map[String, String]): Json =
        JObject(Map#{
            "ok"     => JBool(true),
            "colors" => JObject(colors |> Map.map(hex -> JString(hex)))
        })

    /// 失敗応答 (封筒不正 400 と doc 側の 200 ok:false の共通形 {ok:false, error})。
    def errorJson(message: String): Json =
        JObject(Map#{"ok" => JBool(false), "error" => JString(message)})
}
