// TestTalk — 会話の画面の口の決まり（受けるパスの門番・始まりの行・撮った答えの読み）を固定する。
// .yarn の文法と検査は engine の bin/fge yarn（Go のテスト）、絵はゲームの仕事。

mod TestTalk {
    use fge::JsonCodec
    use Util.Json.Json.{JObject, JString, JNumber}

    /// プロジェクトの中の .yarn だけを受ける。
    @Test
    def testIsYarnPath(): Unit \ Assert =
        Assert.assertEq(expected = (true, false, false, false),
            (Talk.isYarnPath("assets/talk/novel.yarn"), Talk.isYarnPath("../x.yarn"),
             Talk.isYarnPath("/etc/x.yarn"), Talk.isYarnPath("assets/talk/dialogue.json")))

    /// build に渡すのは .yarn のフォルダ（直下なら .）。
    @Test
    def testFolderOf(): Unit \ Assert =
        Assert.assertEq(expected = ("assets/talk", "."), (Talk.folderOf("assets/talk/novel.yarn"), Talk.folderOf("a.yarn")))

    /// 窓なしのゲームへは flow の start の行、起動中のゲームへは /start のクエリ（同じ始まり talk）。
    @Test
    def testStartLineAndGotoRequest(): Unit \ Assert =
        Assert.assertEq(expected = ("start talk node=clock line=40", "POST /start?name=talk&node=clock&line=40"),
            (Talk.startLine("clock", 40), Talk.gotoRequest("clock", 40)))

    /// クエリと flow の行に載せられない字のノード名は断る。
    @Test
    def testIsNodeName(): Unit \ Assert =
        Assert.assertEq(expected = (true, false, false), (Talk.isNodeName("end_butler"), Talk.isNodeName("a b"), Talk.isNodeName("")))

    /// 撮った答えは ok png= の行のパス・無ければ error か FAIL の行が理由。
    @Test
    def testShotOutcome(): Unit \ Assert =
        Assert.assertEq(
            expected = (Ok("debug/shot/shot-1.png"), Err("error: 始まり 'talk': ノード 'x' はシナリオに無い")),
            (Talk.shotOutcome("ok stepped=0\n[shot]\nok png=debug/shot/shot-1.png\n"),
             Talk.shotOutcome("error: 始まり 'talk': ノード 'x' はシナリオに無い\n")))

    /// 1 枚撮りは橋に待つ秒数を渡す（0.51 の橋が読む。0.50 の橋は知らない引数を読み捨てる）。
    @Test
    def testShotRequestPassesWait(): Unit \ Assert =
        Assert.assertEq(expected = "POST /step?view=shot&wait=${Talk.shotWaitSecs()}", Talk.shotRequest())

    /// 起こしている間の答えは starting のまま、段（jar / boot）と経過秒、jar なら何回目と前回の秒数を足す（古い画面は starting と一言だけ読む）。
    @Test
    def testBootingJsonHasStageAndElapsed(): Unit \ Assert =
        let fieldsOf = json -> match json {
            case JObject(fields) => (Map.get("headless", fields), Map.get("stage", fields), Map.get("elapsed", fields),
                                     Map.get("round", fields), Map.get("lastBuildSeconds", fields))
            case _ => (None, None, None, None, None)
        };
        Assert.assertEq(
            expected = (Some(JString("starting")), Some(JString("jar")), Some(JNumber(JsonCodec.intToBd(37))),
                        Some(JNumber(JsonCodec.intToBd(2))), Some(JNumber(JsonCodec.intToBd(95)))),
            fieldsOf(Talk.bootingJson(Headless.BootStage.BuildingJar(2, Some(95i64)), 37i64, 0)))

    /// 変数の値を決めて撮る時は、始まりの行の後ろに $名前=値 を並べる（engine 0.51 の始まりの引数）。
    @Test
    def testStartLineWithVars(): Unit \ Assert =
        Assert.assertEq(expected = ("start talk node=op line=3 $gold=5 $met=true", "start talk node=op line=3"),
            (Talk.startLineWith("op", 3, ("$gold", "5") :: ("$met", "true") :: Nil), Talk.startLineWith("op", 3, Nil)))

    /// 本文の vars は字の値だけ受け、空白を含む字・$ から書かない名前は理由つきで断る（始まりの 1 行は空白で区切る）。
    @Test
    def testVarsOfRefusesSpacesAndBadNames(): Unit \ Assert =
        let ok = Talk.varsOf(Map#{"$gold" => JString("5")});
        let spaced = Talk.varsOf(Map#{"$name" => JString("a b")});
        let badName = Talk.varsOf(Map#{"gold" => JString("1")});
        Assert.assertEq(expected = (Ok(("$gold", "5") :: Nil), false, false),
            (ok, Result.isOk(spaced), Result.isOk(badName)))
}
