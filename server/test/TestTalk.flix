// TestTalk — 会話の画面の口の決まり（受けるパスの門番・始まりの行・撮った答えの読み）を固定する。
// .yarn の文法と検査は engine の bin/fge yarn（Go のテスト）、絵はゲームの仕事。

mod TestTalk {

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
}
