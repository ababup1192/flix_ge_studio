// TestEngineUpdate — 道具の呼び方と、返ってきた出力の読み方だけを固定する。
// 実際の通信と展開は engine の道具 (fge fetch-engine) 側でテストしてある。

mod TestEngineUpdate {

    /// 走らせる道具は今の engine の物。Windows は別の名前。
    @Test
    def testToolPath(): Unit \ Assert =
        Assert.assertEq(
            expected = ("/engines/0.31.0/bin/fge-go", "/engines/0.31.0/bin/fge-go.exe"),
            (EngineUpdate.toolPath("/engines/0.31.0", false),
             EngineUpdate.toolPath("/engines/0.31.0", true)))

    /// 見に行くときは今のバージョンを渡す (それより新しいときだけ知らせが来る)。
    @Test
    def testCheckArgs(): Unit \ Assert =
        Assert.assertEq(
            expected = "/bin/fge-go" :: "fetch-engine" :: "--check" :: "--current" :: "0.31.0" :: "--json" :: Nil,
            EngineUpdate.checkArgs("/bin/fge-go", "0.31.0"))

    /// 落とすときは持ち越す元として今の置き場を渡す。
    @Test
    def testInstallArgs(): Unit \ Assert =
        Assert.assertEq(
            expected = "/bin/fge-go" :: "fetch-engine" :: "--version" :: "0.32.0"
                :: "--into" :: "/engines" :: "--carry-from" :: "/engines/0.31.0" :: "--json" :: Nil,
            EngineUpdate.installArgs({tool = "/bin/fge-go", version = "0.32.0",
                                      versionsDir = "/engines", carryFrom = "/engines/0.31.0"}))

    /// 進み具合の行が前に並ぶので、最後の JSON の行を採る。
    @Test
    def testResultOfTakesTheLastJsonLine(): Unit \ Assert =
        let lines = "[fetch-engine] zip を照合しました" :: "{\"installed\": false}"
            :: "{\"installed\": true, \"version\": \"0.32.0\"}" :: Nil;
        Assert.assertEq(
            expected = Some("0.32.0"),
            EngineUpdate.resultOf(lines) |> Option.flatMap(EngineUpdate.installedVersion))

    /// 何行にも割れた JSON は読み取らない (載せ替えの道具はそちらの形で出す)。
    @Test
    def testResultOfIgnoresSplitJson(): Unit \ Assert =
        let lines = "{" :: "  \"installed\": true," :: "  \"version\": \"0.32.0\"" :: "}" :: Nil;
        Assert.assertEq(
            expected = true,
            Option.isEmpty(EngineUpdate.resultOf(lines)))

    /// 済んでいないときはバージョンを返さない (指し先を進めない)。
    @Test
    def testInstalledVersionNeedsTheDoneMark(): Unit \ Assert =
        Assert.assertEq(
            expected = None,
            EngineUpdate.resultOf("{\"installed\": false, \"version\": \"0.32.0\"}" :: Nil)
                |> Option.flatMap(EngineUpdate.installedVersion))

    /// JSON の行が 1 つも無ければ何も読めない。
    @Test
    def testResultOfWithoutJson(): Unit \ Assert =
        Assert.assertEq(
            expected = true,
            Option.isEmpty(EngineUpdate.resultOf("!! 落とせません" :: Nil)))

    /// 載せ替えの呼び方。engine 側にしか無い置き場をこちらで組む。
    @Test
    def testUpgradeArgs(): Unit \ Assert =
        Assert.assertEq(
            expected = "/bin/fge-go" :: "upgrade-game" :: "--root" :: "/engines/0.32.0"
                :: "--game" :: "/games/kaidan" :: "--version" :: "0.32.0"
                :: "--fpkg" :: "/engines/0.32.0/engine_full/artifact/engine_full.fpkg"
                :: "--toml" :: "/engines/0.32.0/engine_full/flix.toml"
                :: "--dest" :: "lib/github/ababup1192/flix_game_engine/0.32.0"
                :: "--fpkg-name" :: "flix_game_engine-0.32.0.fpkg"
                :: "--toml-name" :: "flix_game_engine-0.32.0.toml"
                :: "--json" :: Nil,
            EngineUpdate.upgradeArgs({tool = "/bin/fge-go", engineDir = "/engines/0.32.0",
                                      game = "/games/kaidan", version = "0.32.0"}))

    /// 今のバージョンが読めないときは帯を出さない (押しても必ず失敗するため)。
    @Test
    def testShouldOfferNeedsAKnownCurrentVersion(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            (EngineUpdate.shouldOffer({current = "0.31.0", available = true}),
             EngineUpdate.shouldOffer({current = "", available = true}),
             EngineUpdate.shouldOffer({current = "0.31.0", available = false})))

    /// 押せるのは、置き場から走っている macOS のときだけ。
    @Test
    def testCanUpdate(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false),
            (EngineUpdate.canUpdate({onWindows = false, fromStore = true}),
             EngineUpdate.canUpdate({onWindows = true, fromStore = true}),
             EngineUpdate.canUpdate({onWindows = false, fromStore = false})))

    /// 指し先を進めるのは、終了コードが 0 で、道具が置いたと言ったときだけ。
    @Test
    def testNextPointerNeedsBothTheCodeAndTheDoneMark(): Unit \ Assert =
        let done = "{\"installed\": true, \"version\": \"0.32.0\"}" :: Nil;
        Assert.assertEq(
            expected = (Some("0.32.0"), None, None, None),
            (EngineUpdate.nextPointer(0, done),
             EngineUpdate.nextPointer(1, done),
             EngineUpdate.nextPointer(0, "{\"installed\": false}" :: Nil),
             EngineUpdate.nextPointer(0, Nil)))

    /// 載せ替えの終了コードの読み分け (0 上げ切った / 3 追随待ち / それ以外は失敗)。
    @Test
    def testUpgradeNote(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, true),
            (String.contains(substr = "緑になりました", EngineUpdate.upgradeNote(0)),
             String.contains(substr = "追随が要ります", EngineUpdate.upgradeNote(3)),
             String.contains(substr = "通りませんでした", EngineUpdate.upgradeNote(1))))

    /// body の読み方。置けない字のバージョンは受けない。
    @Test
    def testRequestedVersion(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("0.32.0"), None, None, None),
            (EngineUpdate.requestedVersion("{\"version\": \"0.32.0\"}"),
             EngineUpdate.requestedVersion("{\"version\": \"../etc\"}"),
             EngineUpdate.requestedVersion("{}"),
             EngineUpdate.requestedVersion("not json")))
}
