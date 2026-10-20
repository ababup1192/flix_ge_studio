// TestHeadless — 窓なしのゲームの世話係の決まり（頼みをどうするか・起こしている段・終わった後の状態）を固定する。
// プロセスを起こす・ポートに繋がる・鍵の pid を読む IO は Headless.answer の仕事で、ここでは Seen を手で置く。
//
//   Idle ─頼み─▶ Booting ─答えた─▶ Running ─終わった─▶ Idle
//                  └─ ready 前に終わった ─▶ Crashed(n) ─ n < 2 なら次の頼みで起こし直す / 2 以上は Failed のまま
//   Booting の段: Starting（起こしている）⇄ Building（作り手の pid が居る間。替わったら数え直して回数を 1 足す）

mod TestHeadless {
    use Headless.{Launch, Decision, Readiness, BootStage, BootPhase}

    /// now ミリ秒の様子（ポートは答えない・ゲームは起動の途中でない・誰も jar を作っていない）。
    def quietAt(now: Int64): Headless.Seen =
        {now = now, answering = false, gameStarting = false, logTail = "boom" :: Nil, builder = None, lastBuild = None}

    /// 時刻 0 に起こした 1 本（まだ jar の作り手を見ていない）。
    def booted(failures: Int32): Launch = Launch.Booting(5000, failures, BootPhase.Starting(0i64, 0))

    /// 答えだけ取り出す（状態を進めた答えも、そのままの答えも）。
    def answerOf(decision: Decision): Option[Readiness] = match decision {
        case Decision.Answer(r)     => Some(r)
        case Decision.Observe(_, r) => Some(r)
        case _                      => None
    }

    // ── 頼みをどうするか ──────────────────────────────────

    /// 起こしていなければ 1 本起こす。ゲームが起動の途中なら起こさずに待たせる。
    @Test
    def testIdleStartsUnlessGameStarting(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false),
            (Headless.decide(Launch.Idle, quietAt(0i64)) == Decision.Start(0),
             Headless.decide(Launch.Idle, { gameStarting = true | quietAt(0i64) }) == Decision.Start(0)))

    /// 起こしている 1 本のポートが答えたら Running の印を付ける・答えなければ起こす段の経過秒で待たせる。
    @Test
    def testBootingWaitsThenMarksRunning(): Unit \ Assert =
        Assert.assertEq(
            expected = (Decision.MarkRunning(5000), Some(Readiness.Booting(BootStage.StartingGame, 1i64, 0))),
            (Headless.decide(booted(0), { answering = true | quietAt(1000i64) }),
             answerOf(Headless.decide(booted(0), quietAt(1000i64)))))

    /// 起こす段で stallMillis たっても答えなければ、ログの末尾つきで「長く起きない」を答える（起こし直さない）。
    @Test
    def testStartingTooLongIsStalledWithLog(): Unit \ Assert =
        let now = Headless.stallMillis();
        Assert.assertEq(
            expected = Some(Readiness.Stalled(Headless.stalledMessage(BootPhase.Starting(0i64, 0), now), "boom" :: Nil)),
            answerOf(Headless.decide(booted(0), quietAt(now))))

    /// jar を作っている間は「長く起きない」を数えない — 作り手が生きていれば stallMillis を過ぎても待つ。
    @Test
    def testBuildingDoesNotCountTowardStall(): Unit \ Assert =
        let launch = Launch.Booting(5000, 0, BootPhase.Building(73404, 0i64, 1));
        let now = Headless.stallMillis() * 2i64;
        Assert.assertEq(
            expected = Some(Readiness.Booting(BootStage.BuildingJar(1, Some(95i64)), now / 1000i64, 0)),
            answerOf(Headless.decide(launch, { builder = Some(73404), lastBuild = Some(95i64) | quietAt(now) })))

    /// 作っている段にも上限（buildStallMillis）はある（作り手が固まったまま待ち続けない）。
    @Test
    def testBuildingHasItsOwnLongLimit(): Unit \ Assert =
        let phase = BootPhase.Building(73404, 0i64, 1);
        Assert.assertEq(
            expected = (false, true),
            (Headless.isStalled(Headless.buildStallMillis() - 1i64, phase), Headless.isStalled(Headless.buildStallMillis(), phase)))

    /// 作り終えて作り手が居なくなったら、起こす段の見切りはそこから数える（作っていた時間を足さない）。
    @Test
    def testStallCountsFromWhenBuildEnded(): Unit \ Assert =
        let builtUntil = Headless.stallMillis() * 3i64;
        let after = Headless.observe(builtUntil, None, BootPhase.Building(73404, 0i64, 1));
        Assert.assertEq(
            expected = (BootPhase.Starting(builtUntil, 1), false),
            (after, Headless.isStalled(builtUntil + Headless.stallMillis() - 1i64, after)))

    /// 作り手の pid が替わったら経過を数え直し、何回目かを 1 足す（「作り直しています（2 回目）」）。
    @Test
    def testNewBuilderRestartsCountAndRound(): Unit \ Assert =
        Assert.assertEq(
            expected = (BootPhase.Building(73404, 0i64, 1), BootPhase.Building(80001, 9000i64, 2), BootPhase.Building(80001, 9000i64, 2)),
            (Headless.observe(0i64, Some(73404), BootPhase.Starting(0i64, 0)),
             Headless.observe(9000i64, Some(80001), BootPhase.Building(73404, 0i64, 1)),
             Headless.observe(12000i64, Some(80001), BootPhase.Building(80001, 9000i64, 2))))

    /// 作り終えた後にまた作り手が現れても回数を続けて数える（作り終えた直後の作り直し）。
    @Test
    def testBuildAgainAfterStartingCountsOn(): Unit \ Assert =
        Assert.assertEq(expected = BootPhase.Building(80001, 5000i64, 2),
            Headless.observe(5000i64, Some(80001), BootPhase.Starting(1000i64, 1)))

    /// 作り手は鍵の生きている pid。鍵が無くても、jar を使わず flix run でコンパイルしている時は 0。
    @Test
    def testBuilderOf(): Unit \ Assert =
        let flixRun = "[headless-run] flix run で流すので HEADLESS_JAVA_OPTS（-Xmx1g）は効かない" :: Nil;
        Assert.assertEq(expected = (Some(73404), Some(0), None),
            (Headless.builderOf(Some(73404), Nil), Headless.builderOf(None, flixRun), Headless.builderOf(None, "ok" :: Nil)))

    /// 1 回落ちただけなら次の頼みで 1 回だけ起こし直す・続けて maxCrashesInRow 回落ちたら理由を答えたまま待つ。
    @Test
    def testCrashRelaunchesOnceThenWaits(): Unit \ Assert =
        let limit = Headless.maxCrashesInRow();
        Assert.assertEq(
            expected = (Decision.Start(limit - 1), Decision.Answer(Readiness.Failed("落ちた", "boom" :: Nil))),
            (Headless.decide(Launch.Crashed(limit - 1, "落ちた", "boom" :: Nil), quietAt(0i64)),
             Headless.decide(Launch.Crashed(limit, "落ちた", "boom" :: Nil), quietAt(0i64))))

    /// 「もう一度起こす」は落ちた 1 本・長く起きない 1 本を起こし直す（落ちた回数は持ち越す）。
    /// 起きている途中（まだ長くない）・jar を作っている途中は起こし直さない — 押すたびに JVM を増やさない。
    @Test
    def testRestartOnlyWhenCrashedOrStalled(): Unit \ Assert =
        let building = Launch.Booting(5000, 0, BootPhase.Building(73404, 0i64, 1));
        Assert.assertEq(
            expected = (Decision.Start(Headless.maxCrashesInRow()), Decision.Start(1), false, false),
            (Headless.decideRestart(Launch.Crashed(Headless.maxCrashesInRow(), "落ちた", Nil), quietAt(0i64)),
             Headless.decideRestart(booted(1), quietAt(Headless.stallMillis())),
             Headless.decideRestart(booted(0), quietAt(1000i64)) == Decision.Start(0),
             Headless.decideRestart(building, { builder = Some(73404) | quietAt(Headless.stallMillis()) }) == Decision.Start(0)))

    // ── 一言（目安を決め打ちしない）────────────────────────

    /// jar を作る一言は前回の秒数が分かる時だけ目安を言い、2 人目の作り手からは「作り直しています（2 回目）」。
    @Test
    def testBootingMessageForBuild(): Unit \ Assert =
        let first = Headless.bootingMessage(BootStage.BuildingJar(1, Some(95i64)), 37i64, 0);
        let unknown = Headless.bootingMessage(BootStage.BuildingJar(1, None), 37i64, 0);
        let again = Headless.bootingMessage(BootStage.BuildingJar(2, None), 4i64, 0);
        Assert.assertEq(
            expected = (true, false, true),
            (String.contains(substr = "前回は 95 秒。いま 37 秒", first),
             String.contains(substr = "分", unknown) or String.contains(substr = "前回", unknown),
             String.contains(substr = "作り直しています（2 回目）", again)))

    /// 起こす一言は経過秒だけ（「数秒」の目安も言わない）。前に落ちていれば「もう一度」。
    @Test
    def testBootingMessageForStart(): Unit \ Assert =
        Assert.assertEq(
            expected = ("窓なしのゲームを起こしています（3 秒）", true),
            (Headless.bootingMessage(BootStage.StartingGame, 3i64, 0),
             String.contains(substr = "もう一度", Headless.bootingMessage(BootStage.StartingGame, 0i64, 1))))

    // ── 終わった後の状態 ──────────────────────────────────

    /// ready 前に終わったら落ちた回数を 1 足して理由とログを残す・ready の後に終わったら Idle（次の頼みで黙って立て直す）。
    @Test
    def testAfterExitCrashedOrIdle(): Unit \ Assert =
        let exit = {port = 5000, why = Headless.exitReason(1), logTail = "boom" :: Nil};
        Assert.assertEq(
            expected = (Launch.Crashed(2, Headless.exitReason(1), "boom" :: Nil), Launch.Idle),
            (Headless.afterExit(exit, booted(1)), Headless.afterExit(exit, Launch.Running(5000))))

    /// 起こし直した後に古い 1 本（別のポート）の終わりが届いても、今の 1 本の状態は変えない。
    @Test
    def testAfterExitIgnoresOldProcess(): Unit \ Assert =
        let exit = {port = 4000, why = Headless.exitReason(137), logTail = Nil};
        Assert.assertEq(expected = booted(1), Headless.afterExit(exit, booted(1)))

    /// ログの末尾に混ざる端末の色の印は外す（画面に字のまま出さない）。
    @Test
    def testWithoutColorCodes(): Unit \ Assert =
        Assert.assertEq(expected = "Found 'flix.toml'.", Headless.withoutColorCodes("Found '\u001b[38;2;68;147;200mflix.toml\u001b[0m'."))
}
