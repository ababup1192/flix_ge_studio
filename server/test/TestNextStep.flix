// TestNextStep — 「次のやること」を決める優先順位と、素材の鮮度判定のルールを固定する。
// 文言そのものは仕様ではないので見ない (id と nav だけを見る)。
// 検査と基準の追随は Changes が自動でやるため、提案は「人の判断が要る事」だけ:
// 候補あり → pick / 未読の知らせ → changed /
// 生まれたて → 走っていなければ launch・走っていれば arrange / それ以外 → create。

mod TestNextStep {

    def checks(candidates: Int32, stale: Bool, diffs: Int32): NextStep.Checks =
        { atelierCandidates = candidates, staleGallery = stale, diffCount = diffs,
          referenceEmpty = false, hasStarterDoc = false, gameRunning = false }

    def idNav(suggestion: NextStep.Suggestion): (String, String) =
        (suggestion#id, suggestion#nav)

    /// 優先順位: 候補あり → pick が最優先 (知らせが同時に立っていても勝つ —
    /// 選んでいる最中の人を遮らない)。次に未読の知らせ → changed、どれも無し → create。
    /// stale や diff はもう提案の材料ではない (自動化された)。
    @Test
    def testSuggestPriority(): Unit \ Assert =
        Assert.assertEq(
            expected = (("pick", "atelier"), ("changed", "changes"),
                        ("create", "atelier"), ("create", "atelier")),
            (idNav(NextStep.suggest(checks(2, true, 3), 5)),
             idNav(NextStep.suggest(checks(0, false, 0), 3)),
             idNav(NextStep.suggest(checks(0, true, 3), 0)),
             idNav(NextStep.suggest(checks(0, false, 0), 0))))

    /// 鮮度: 素材の最新 > 絵の最古 で stale。絵が無くても素材があれば stale。
    /// 素材が無ければ焼く物が無いので stale ではない。同時刻は stale ではない。
    @Test
    def testIsStale(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false),
            (NextStep.isStale(10i64 :: 30i64 :: Nil, 20i64 :: 25i64 :: Nil),
             NextStep.isStale(10i64 :: Nil, Nil),
             NextStep.isStale(Nil, 20i64 :: Nil),
             NextStep.isStale(Nil, Nil),
             NextStep.isStale(20i64 :: Nil, 20i64 :: Nil)))

    /// 産まれたてテンプレート (雛形 Doc が居て、他にやる事が無い): ゲームが
    /// 走っていなければ launch、走っていれば arrange。pick と changed はどちらより優先。
    /// 雛形 Doc が無ければ create のまま (走っているかは関係しない)。
    @Test
    def testLaunchArrange(): Unit \ Assert =
        def withStarter(candidates: Int32, unseen: Int32, running: Bool): (String, String) =
            idNav(NextStep.suggest({ atelierCandidates = candidates, staleGallery = false,
                                    diffCount = 0, referenceEmpty = false, hasStarterDoc = true,
                                    gameRunning = running }, unseen));
        Assert.assertEq(
            expected = (("launch", "launch"), ("arrange", "arrange"),
                        "pick", "changed", "create"),
            (withStarter(0, 0, false),
             withStarter(0, 0, true),
             fst(withStarter(1, 0, false)),
             fst(withStarter(0, 2, true)),
             NextStep.suggest({ atelierCandidates = 0, staleGallery = false,
                               diffCount = 0, referenceEmpty = false, hasStarterDoc = false,
                               gameRunning = true }, 0)#id))
}
