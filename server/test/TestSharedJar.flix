// TestSharedJar — 共通 jar を裏で作る係の決まり（いつ作るか・保存が重なった時・どの仕事を jar で流すか）を固定する。
// プロセスを起こす IO は SharedJar.step / startInBackground の仕事で、ここでは時刻と終了コードを手で置く。
//
//   Idle ─開いた─▶ Building ─済んだ─▶ Ready / Failed ─60 秒後の様子見・保存─▶ Building
//                    └─ 作っている間に保存 ─▶ BuildingThenAgain ─済んだ─▶ Building（もう 1 回）

mod TestSharedJar {
    use SharedJar.Build
    use EngineTasks.Task

    /// 作るだけの口がある headless-run の本文（窓ありの口も）。
    def newScript(): Option[String] = Some("case \"$1\" in --build-only) ;; --window) ;; esac")

    // ── いつ作るか ────────────────────────────────────────

    /// 開いた時（まだ確かめていない）は作り始める。作っている間の様子見は何もしない。
    @Test
    def testLookStartsOnlyWhenIdle(): Unit \ Assert =
        Assert.assertEq(
            expected = ((Build.Building(5i64), true), (Build.Building(1i64), false)),
            (SharedJar.onLook(5i64, Build.Idle), SharedJar.onLook(5i64, Build.Building(1i64))))

    /// 済んだ jar は recheckMillis たつまで確かめ直さない（Studio の外で src を直した時はその後に拾う）。
    @Test
    def testLookRechecksAfterInterval(): Unit \ Assert =
        let wait = SharedJar.recheckMillis();
        Assert.assertEq(
            expected = ((Build.Ready(0i64), false), (Build.Building(wait), true)),
            (SharedJar.onLook(wait - 1i64, Build.Ready(0i64)), SharedJar.onLook(wait, Build.Ready(0i64))))

    /// 作っている間の保存は、もう 1 本の印だけ付ける（JVM を 2 本並べない）。済んだらもう 1 本作る。
    @Test
    def testSaveWhileBuildingBuildsAgainAfter(): Unit \ Assert =
        let (during, startNow) = SharedJar.onSave(9i64, Build.Building(1i64));
        Assert.assertEq(
            expected = ((Build.BuildingThenAgain(1i64), false), (Build.Building(20i64), true)),
            ((during, startNow), SharedJar.afterBuild(20i64, {code = 0, tail = Nil}, during)))

    /// 済んだ・作れなかった後の保存はすぐ作り始める。
    @Test
    def testSaveAfterDoneStarts(): Unit \ Assert =
        Assert.assertEq(
            expected = ((Build.Building(9i64), true), (Build.Building(9i64), true)),
            (SharedJar.onSave(9i64, Build.Ready(1i64)), SharedJar.onSave(9i64, Build.Failed(1i64, Nil))))

    /// 作り終えたら終了コードで Ready / Failed（ログの末尾を持つ）。
    @Test
    def testAfterBuildReadyOrFailed(): Unit \ Assert =
        Assert.assertEq(
            expected = ((Build.Ready(7i64), false), (Build.Failed(7i64, "MethodTooLarge" :: Nil), false)),
            (SharedJar.afterBuild(7i64, {code = 0, tail = Nil}, Build.Building(1i64)),
             SharedJar.afterBuild(7i64, {code = 3, tail = "MethodTooLarge" :: Nil}, Build.Building(1i64))))

    /// jar に効くのは src・lib・flix.toml・packages.lock だけ（assets と .yarn は走る時に読む）。
    @Test
    def testAffectsJar(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false),
            (SharedJar.affectsJar("src/Main.flix"), SharedJar.affectsJar("flix.toml"),
             SharedJar.affectsJar("assets/talk/opening.yarn"), SharedJar.affectsJar("assets/x.json")))

    // ── どの仕事を jar で流すか ──────────────────────────

    /// 描き出し・パレット・宣言した入口は jar で、窓ありは --window main で流す。型検査とテストは flix のまま。
    @Test
    def testRouteWithNewEngine(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("SceneRender.all" :: Nil), Some("--window" :: "main" :: Nil),
                        Some("SfxPreview.main" :: Nil), None),
            (SharedJar.routeOf(newScript(), false, Task.Render), SharedJar.routeOf(newScript(), false, Task.Debug),
             SharedJar.routeOf(newScript(), false, Task.EntryPoint("SfxPreview.main")), SharedJar.routeOf(newScript(), false, Task.Check)))

    /// 口の無い古い headless-run・無い・Windows は今までどおり flix run（知らない旗を古い版に渡さない）。
    @Test
    def testRouteFallsBackToFlixRun(): Unit \ Assert =
        Assert.assertEq(
            expected = (None, None, None),
            (SharedJar.routeOf(Some("headless-run <モジュール.関数>"), false, Task.Render),
             SharedJar.routeOf(None, false, Task.Render),
             SharedJar.routeOf(newScript(), true, Task.Render)))

    /// 作るだけの口はあっても窓ありの口が無い版では、プレイだけ flix run に戻る。
    @Test
    def testDebugNeedsWindowFlag(): Unit \ Assert =
        Assert.assertEq(expected = None, SharedJar.routeOf(Some("--build-only"), false, Task.Debug))

    /// headless-run へ渡す旗の組は 1 つ（jar の指紋に JAVA と HEADLESS_JAVA_OPTS が入るので、仕事ごとに変えると
    /// 呼ぶたびに jar を作り直す）。窓ありの旗も足さない（--window が自分で付ける）。
    @Test
    def testJarEnvIsOneSet(): Unit \ Assert =
        Assert.assertEq(expected = ("JAVA", "/j/java") :: ("HEADLESS_JAVA_OPTS", "") :: Nil, SharedJar.jarEnv("/j/java"))
}
