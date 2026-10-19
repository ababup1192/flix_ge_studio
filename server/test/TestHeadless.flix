// TestHeadless — 窓なしのゲームの世話係の決まり（頼みをどうするか・終わった後の状態）を固定する。
// プロセスを起こす・ポートに繋がる IO は Headless.answer / startInBackground の仕事で、ここでは Seen を手で置く。
//
//   Idle ─頼み─▶ Booting ─答えた─▶ Running ─終わった─▶ Idle
//                  └─ ready 前に終わった ─▶ Crashed(n) ─ n < 2 なら次の頼みで起こし直す / 2 以上は Failed のまま

mod TestHeadless {
    use Headless.{Launch, Decision, Readiness, BootStage}

    /// 起こした時刻 0 から now ミリ秒後の様子（ポートは答えない・ゲームは起動の途中でない）。
    def quietAt(now: Int64): Headless.Seen =
        {now = now, answering = false, gameStarting = false, logTail = "boom" :: Nil, stage = BootStage.StartingGame}

    // ── 頼みをどうするか ──────────────────────────────────

    /// 起こしていなければ 1 本起こす。ゲームが起動の途中なら起こさずに待たせる。
    @Test
    def testIdleStartsUnlessGameStarting(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false),
            (Headless.decide(Launch.Idle, quietAt(0i64)) == Decision.Start(0),
             Headless.decide(Launch.Idle, { gameStarting = true | quietAt(0i64) }) == Decision.Start(0)))

    /// 起こしている 1 本のポートが答えたら Running の印を付ける・答えなければ起こし直さずに、段と経過秒で待たせる。
    @Test
    def testBootingWaitsThenMarksRunning(): Unit \ Assert =
        Assert.assertEq(
            expected = (Decision.MarkRunning(5000), Decision.Answer(Readiness.Booting(BootStage.StartingGame, 1i64, 0))),
            (Headless.decide(Launch.Booting(5000, 0i64, 0), { answering = true | quietAt(1000i64) }),
             Headless.decide(Launch.Booting(5000, 0i64, 0), quietAt(1000i64))))

    /// 起こしてから stallMillis たっても答えなければ、ログの末尾つきで「長く起きない」を答える（起こし直さない）。
    @Test
    def testBootingTooLongIsStalledWithLog(): Unit \ Assert =
        Assert.assertEq(
            expected = Decision.Answer(Readiness.Stalled(Headless.stalledMessage(Headless.stallMillis()), "boom" :: Nil)),
            Headless.decide(Launch.Booting(5000, 0i64, 0), quietAt(Headless.stallMillis())))

    /// 1 回落ちただけなら次の頼みで 1 回だけ起こし直す・続けて maxCrashesInRow 回落ちたら理由を答えたまま待つ。
    @Test
    def testCrashRelaunchesOnceThenWaits(): Unit \ Assert =
        let limit = Headless.maxCrashesInRow();
        Assert.assertEq(
            expected = (Decision.Start(limit - 1), Decision.Answer(Readiness.Failed("落ちた", "boom" :: Nil))),
            (Headless.decide(Launch.Crashed(limit - 1, "落ちた", "boom" :: Nil), quietAt(0i64)),
             Headless.decide(Launch.Crashed(limit, "落ちた", "boom" :: Nil), quietAt(0i64))))

    /// 「もう一度起こす」は落ちた 1 本・長く起きない 1 本を起こし直す（落ちた回数は持ち越す）。
    /// 起きている途中（まだ長くない）は起こし直さない — 押すたびに JVM を増やさない。
    @Test
    def testRestartOnlyWhenCrashedOrStalled(): Unit \ Assert =
        Assert.assertEq(
            expected = (Decision.Start(Headless.maxCrashesInRow()), Decision.Start(1),
                        Decision.Answer(Readiness.Booting(BootStage.StartingGame, 1i64, 0))),
            (Headless.decideRestart(Launch.Crashed(Headless.maxCrashesInRow(), "落ちた", Nil), quietAt(0i64)),
             Headless.decideRestart(Launch.Booting(5000, 0i64, 1), quietAt(Headless.stallMillis())),
             Headless.decideRestart(Launch.Booting(5000, 0i64, 0), quietAt(1000i64))))

    /// 段は様子のまま答える（jar を作っている間は BuildingJar・経過秒は起こした時刻から）。
    @Test
    def testBootingAnswersStageFromSeen(): Unit \ Assert =
        Assert.assertEq(
            expected = Decision.Answer(Readiness.Booting(BootStage.BuildingJar, 42i64, 1)),
            Headless.decide(Launch.Booting(5000, 0i64, 1), { stage = BootStage.BuildingJar | quietAt(42500i64) }))

    // ── 段の決まり（bin/headless-run の行と jar を作る鍵）────

    /// jar を作ると言って鍵がある間だけ BuildingJar。作り終えて鍵が外れたら StartingGame。
    @Test
    def testBootStageJarWhileLocked(): Unit \ Assert =
        let log = "Found 'flix.toml'." :: "[headless-run] jar を作る（src か lib が変わった）" :: Nil;
        Assert.assertEq(
            expected = (BootStage.BuildingJar, BootStage.StartingGame),
            (Headless.bootStageOf(log, true), Headless.bootStageOf(log, false)))

    /// jar を作らない回は headless-run が何も言わない — 鍵が（ほかの make の物で）あっても StartingGame。
    @Test
    def testBootStageSilentLogIsStarting(): Unit \ Assert =
        Assert.assertEq(expected = BootStage.StartingGame, Headless.bootStageOf(Nil, true))

    /// ほかの make が jar を作り終えるのを待つ間も BuildingJar（その後に同じ jar で起こす）。
    @Test
    def testBootStageWaitingForOtherBuild(): Unit \ Assert =
        Assert.assertEq(expected = BootStage.BuildingJar,
            Headless.bootStageOf("[headless-run] pid 123 が jar を作っているので待つ" :: Nil, true))

    /// jar を使えずに flix run へ回った回は、鍵なしでもコンパイルしているので BuildingJar。
    @Test
    def testBootStageFlixRunIsBuilding(): Unit \ Assert =
        Assert.assertEq(expected = BootStage.BuildingJar,
            Headless.bootStageOf("[headless-run] flix run で流すので HEADLESS_JAVA_OPTS（-Xmx1g）は効かない" :: Nil, false))

    /// 一言は段で変わる: jar を作る時だけ「1〜2 分」・起こしているだけなら「数秒」。経過秒も言う。
    @Test
    def testBootingMessageByStage(): Unit \ Assert =
        let jar = Headless.bootingMessage(BootStage.BuildingJar, 37i64, 0);
        let boot = Headless.bootingMessage(BootStage.StartingGame, 3i64, 0);
        Assert.assertEq(
            expected = (true, true, false, true),
            (String.contains(substr = "1〜2 分", jar) and String.contains(substr = "37 秒", jar),
             String.contains(substr = "数秒", boot) and String.contains(substr = "3 秒", boot),
             String.contains(substr = "jar", boot),
             String.contains(substr = "もう一度", Headless.bootingMessage(BootStage.StartingGame, 0i64, 1))))

    // ── 終わった後の状態 ──────────────────────────────────

    /// ready 前に終わったら落ちた回数を 1 足して理由とログを残す・ready の後に終わったら Idle（次の頼みで黙って立て直す）。
    @Test
    def testAfterExitCrashedOrIdle(): Unit \ Assert =
        let exit = {port = 5000, why = Headless.exitReason(1), logTail = "boom" :: Nil};
        Assert.assertEq(
            expected = (Launch.Crashed(2, Headless.exitReason(1), "boom" :: Nil), Launch.Idle),
            (Headless.afterExit(exit, Launch.Booting(5000, 0i64, 1)), Headless.afterExit(exit, Launch.Running(5000))))

    /// 起こし直した後に古い 1 本（別のポート）の終わりが届いても、今の 1 本の状態は変えない。
    @Test
    def testAfterExitIgnoresOldProcess(): Unit \ Assert =
        let exit = {port = 4000, why = Headless.exitReason(137), logTail = Nil};
        Assert.assertEq(expected = Launch.Booting(5000, 0i64, 1), Headless.afterExit(exit, Launch.Booting(5000, 0i64, 1)))

    /// ログの末尾に混ざる端末の色の印は外す（画面に字のまま出さない）。
    @Test
    def testWithoutColorCodes(): Unit \ Assert =
        Assert.assertEq(expected = "Found 'flix.toml'.", Headless.withoutColorCodes("Found '\u001b[38;2;68;147;200mflix.toml\u001b[0m'."))
}
