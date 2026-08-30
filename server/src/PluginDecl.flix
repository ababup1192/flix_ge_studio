// PluginDecl — project.json の "editor.plugins" (拡張プラグインの宣言) を読む純粋層。
// 拡張プラグインは studio-plugins/<id>/index.html を iframe で配信する仕組みで、
// 既存の editor.resources[].plugin (Elm 内蔵の盤面プレビュー) とは別系統。
// ここはファイルにも HTTP にも触らない: JSON 値 → 宣言の変換と検査だけを持つ。
// 実在確認 (index.html があるか) と配信は Plugins.flix (IO 層) の仕事。

mod PluginDecl {
    use Util.Json.Json
    use Util.Json.Json.{JArray}

    /// 拡張プラグイン 1 件ぶんの宣言。
    /// id は配信ディレクトリ名 (studio-plugins/<id>/) と URL (/plugin-ui/<id>/) を兼ねるので、
    /// 英数字と `-` `_` 以外 (空も) は Err で断る。
    pub type alias Decl = {
        id = String,                    // 宣言を指す短い名前 = ディレクトリ名 (例: "battle-sim")
        title = Option[String],         // 画面に出す表示名。無ければ画面側が id で代用する
        performUrl = Option[String],    // ゲーム側常駐サーバの口 (/bake/wake・/bake/proxy の宛先)
        performCmd = Option[String]     // その口の起こし方。繋がらない時にプロジェクトの根で撃つ
    }

    /// project.json 全体から editor.plugins 宣言を読む。"editor" や "plugins" の不在は
    /// Ok(空) — 宣言の無いゲームも従来どおり (プラグイン無しで) 編集できる。
    /// 形違い (id 欠落・危ない id) は、どの項目かを含む平易な日本語 1 行の Err で返す。
    pub def fromProjectJson(json: Json): Result[String, List[Decl]] =
        match pluginsArray(json) {
            case None => Ok(Nil)
            case Some(JArray(items)) =>
                Vector.toList(items)
                    |> List.zipWithIndex
                    |> Result.traverse(entry -> {
                        let (index, itemJson) = entry;
                        declAt(index, itemJson)
                    })
            case Some(_) => Err("editor.plugins は配列 ([ ]) で書いてください")
        }

    /// editor.plugins[] の各宣言に知らないキーが無いかを見る。タイポ (performURL 等) は
    /// エラーにならず None へ倒れ、「起こし方が宣言されていません」の理由が誰にも
    /// 分からなくなる — ここで名指しすることでだけ気づける。
    pub def keyWarnings(projectJson: Json): List[String] =
        let known = Set#{"id", "title", "performUrl", "performCmd"};
        pluginItems(projectJson) |> List.flatMap(item -> match JsonCodec.expectObject(item) {
            case None      => Nil
            case Some(obj) =>
                let declId = Map.get("id", obj) |> Option.flatMap(JsonCodec.expectString) |> Option.getWithDefault("?");
                Map.keysOf(obj) |> Set.toList
                    |> List.filter(k -> not (String.startsWith(prefix = "//", k)))
                    |> List.filter(k -> not Set.memberOf(k, known))
                    |> List.map(k -> "プラグイン『${declId}』に未知のキー『${k}』があります(使えるのは id / title / performUrl / performCmd)")
        }) ::: duplicateIdWarnings(projectJson)

    /// 同じ id の宣言が 2 つ以上あれば 1 行で知らせる。配信先 (studio-plugins/<id>/) が
    /// 同じ場所を指すので、どちらの宣言が効いているのか分からなくなるため。
    def duplicateIdWarnings(projectJson: Json): List[String] =
        let ids = pluginItems(projectJson)
            |> List.filterMap(item -> JsonCodec.expectObject(item)
                |> Option.flatMap(obj -> Map.get("id", obj))
                |> Option.flatMap(JsonCodec.expectString));
        ids |> List.distinct
            |> List.filter(id -> List.count(x -> x == id, ids) > 1)
            |> List.map(id -> "editor.plugins に id『${id}』が重複して宣言されています(配信先が同じ場所を指します)")

    /// id がディレクトリ名と URL の部品として安全か。英数字と `-` `_` だけ (空でない) を通す —
    /// `#` `?` `%` や空白が混ざると iframe の src (URL) が壊れるため、許可リストで絞る。
    pub def isSafeId(id: String): Bool =
        id != "" and String.forAll(isSafeIdChar, id)

    /// isSafeId 用: [A-Za-z0-9_-] の 1 文字か。
    def isSafeIdChar(c: Char): Bool =
        (c >= 'a' and c <= 'z')
            or (c >= 'A' and c <= 'Z')
            or (c >= '0' and c <= '9')
            or c == '-' or c == '_'

    // ── 分解 ──────────────────────────────────────────────

    /// project.json のトップレベルから editor.plugins の値を取り出す。
    /// 途中のどこかが無い・形が違うのは None (= 宣言なし)。
    def pluginsArray(json: Json): Option[Json] =
        JsonCodec.expectObject(json)
            |> Option.flatMap(topObj -> Map.get("editor", topObj))
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(editorObj -> Map.get("plugins", editorObj))

    /// keyWarnings 用: plugins 配列の要素たち (形が違えば空)。
    def pluginItems(json: Json): List[Json] =
        match pluginsArray(json) {
            case Some(JArray(items)) => Vector.toList(items)
            case _                   => Nil
        }

    /// plugins の要素 1 件を Decl に読む。id は必須 + 安全検査。
    def declAt(index: Int32, json: Json): Result[String, Decl] =
        let location = "editor.plugins[${index}]";
        forM (
            declObj <- JsonCodec.expectObject(json)
                |> Option.toOk("${location} はオブジェクト ({ }) で書いてください");
            id      <- Map.get("id", declObj)
                |> Option.flatMap(JsonCodec.expectString)
                |> Option.toOk("${location} に id (文字列) がありません");
            safeId  <- if (isSafeId(id)) Ok(id)
                       else Err("${location} の id『${id}』は使えません(英数字と `-` `_` だけ・空は不可)")
        ) yield {
            id         = safeId,
            title      = optionalString("title", declObj),
            performUrl = optionalString("performUrl", declObj),
            performCmd = optionalString("performCmd", declObj)
        }

    /// 任意の文字列フィールド。
    // WhyNot: 文字列以外が書かれていても Err にせず None に倒す — title / performUrl は
    // 無くても一覧と配信は続けられる。必須の id だけ厳しくする (ResourceDecl と同じ流儀)。
    def optionalString(key: String, declObj: Map[String, Json]): Option[String] =
        Map.get(key, declObj) |> Option.flatMap(JsonCodec.expectString)
}
