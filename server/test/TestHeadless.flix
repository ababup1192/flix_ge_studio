// TestHeadless — 窓なしのゲームの世話係の決まり（頼みをどうするか・終わった後の状態）を固定する。
// プロセスを起こす・ポートに繋がる IO は Headless.answer / startInBackground の仕事で、ここでは Seen を手で置く。
//
//   Idle ─頼み─▶ Booting ─答えた─▶ Running ─終わった─▶ Idle
//                  └─ ready 前に終わった ─▶ Crashed(n) ─ n < 2 なら次の頼みで起こし直す / 2 以上は Failed のまま

mod TestHeadless {
    use Headless.{Launch, Decision, Readiness}

    /// 起こした時刻 0 から now ミリ秒後の様子（ポートは答えない・ゲームは起動の途中でない）。
    def quietAt(now: Int64): Headless.Seen =
        {now = now, answering = false, gameStarting = false, logTail = "boom" :: Nil}

    // ── 頼みをどうするか ──────────────────────────────────

    /// 起こしていなければ 1 本起こす。ゲームが起動の途中なら起こさずに待たせる。
    @Test
    def testIdleStartsUnlessGameStarting(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false),
            (Headless.decide(Launch.Idle, quietAt(0i64)) == Decision.Start(0),
             Headless.decide(Launch.Idle, { gameStarting = true | quietAt(0i64) }) == Decision.Start(0)))

    /// 起こしている 1 本のポートが答えたら Running の印を付ける・答えなければ起こし直さずに待たせる。
    @Test
    def testBootingWaitsThenMarksRunning(): Unit \ Assert =
        Assert.assertEq(
            expected = (Decision.MarkRunning(5000), Decision.Answer(Readiness.Starting(Headless.bootingMessage(0)))),
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
                        Decision.Answer(Readiness.Starting(Headless.bootingMessage(0)))),
            (Headless.decideRestart(Launch.Crashed(Headless.maxCrashesInRow(), "落ちた", Nil), quietAt(0i64)),
             Headless.decideRestart(Launch.Booting(5000, 0i64, 1), quietAt(Headless.stallMillis())),
             Headless.decideRestart(Launch.Booting(5000, 0i64, 0), quietAt(1000i64))))

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
