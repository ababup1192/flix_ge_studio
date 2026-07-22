// TestScaffold — まっさらの入口のルールだけ固定する: 名前の門番・宣言の追記・ファイル名の規約。
// プロセス起動やファイル書きは curl での通し確認に任せ、ここは純粋な判定と変換だけを見る。

mod TestScaffold {
    use Util.Json.Json.{JObject, JArray, JString}

    /// 名前は ^[a-z][a-z0-9_]*$。大文字・数字始まり・記号・空・経路いじりは通さない。
    @Test
    def testIsName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false, false, false),
            (Scaffold.isName("breakout2"),
             Scaffold.isName("my_game"),
             Scaffold.isName("Breakout"),
             Scaffold.isName("2games"),
             Scaffold.isName(""),
             Scaffold.isName("a-b"),
             Scaffold.isName("../x")))

    /// データファイル名: 省略時は <kind>.kind.json。指定は 1 語の *.json だけで、
    /// *.schema.json (スキーマの席) と経路入りは断る。
    @Test
    def testDocFileNameOf(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("enemy.kind.json"), Some("foes.json"), None, None),
            (Scaffold.docFileNameOf("enemy", None),
             Scaffold.docFileNameOf("enemy", Some("foes.json")),
             Scaffold.docFileNameOf("enemy", Some("enemy.schema.json")),
             Scaffold.docFileNameOf("enemy", Some("../enemy.json"))))

    /// 宣言の追記 (メモリ内変換): 既存の宣言の後ろに 1 件足し、既存は保つ。
    /// editor が無い project.json には editor.resources ごと作る。
    /// resources が配列でない壊れた形は Err (fail-closed)。
    @Test
    def testAppendResource(): Unit \ Assert =
        let existing = JObject(Map#{
            "title"  => JString("t"),
            "editor" => JObject(Map#{
                "resources" => JArray(Vector#{JObject(Map#{"id" => JString("old"), "pattern" => JString("assets/old.json")})})
            })
        });
        let bare = JObject(Map#{"title" => JString("t")});
        let broken = JObject(Map#{"editor" => JObject(Map#{"resources" => JString("oops")})});
        let idsOf = json -> match Result.map(declIds, Scaffold.appendResource("enemy", "enemy.kind.json", "てき", "material", json)) {
            case Ok(ids) => ids
            case Err(_)  => Nil
        };
        Assert.assertEq(
            expected = ("old" :: "enemy" :: Nil, "enemy" :: Nil, true),
            (idsOf(existing),
             idsOf(bare),
             Result.isErr(Scaffold.appendResource("enemy", "enemy.kind.json", "てき", "material", broken))))

    /// 変換後の editor.resources の id の列。
    def declIds(json: Util.Json.Json): List[String] =
        JsonCodec.expectObject(json)
            |> Option.flatMap(o -> Map.get("editor", o))
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(o -> Map.get("resources", o))
            |> Option.flatMap(JsonCodec.decodeList(item ->
                JsonCodec.expectObject(item)
                    |> Option.flatMap(o -> Map.get("id", o))
                    |> Option.flatMap(JsonCodec.expectString)))
            |> Option.getWithDefault(Nil)
}
