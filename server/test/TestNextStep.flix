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

    /// 鮮度: 前の描き出しを覚えていないときは、素材の最新と絵の**最新**を比べる。
    /// 3 番目が要: 古い PNG が 1 枚混ざっているだけでは stale にしない (最古と比べると真になる)。
    /// 絵が無くても素材があれば stale。素材が無ければ作る物が無い。同時刻は stale ではない。
    @Test
    def testIsStale(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false, false),
            (NextStep.isStale(10i64 :: 30i64 :: Nil, 20i64 :: 25i64 :: Nil),
             NextStep.isStale(10i64 :: Nil, Nil),
             NextStep.isStale(30i64 :: Nil, 10i64 :: 90i64 :: Nil),
             NextStep.isStale(Nil, 20i64 :: Nil),
             NextStep.isStale(Nil, Nil),
             NextStep.isStale(20i64 :: Nil, 20i64 :: Nil)))

    /// 前の描き出しの時点を覚えていれば、絵の側は判定に関わらない — 描き出しが書き直さない
    /// 古い PNG が gallery に混ざっていても、作り直しは要らないと読む。
    @Test
    def testNeedsBakeIgnoresForeignImages(): Unit \ Assert =
        Assert.assertEq(
            expected = false,
            NextStep.needsBake(50i64 :: Nil, 10i64 :: 90i64 :: Nil, Some(50i64)))

    /// 前の描き出しを起こした時点より後にアセットが動いたら作り直す。同時刻は作り直さない。
    @Test
    def testNeedsBakeAfterStamp(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false),
            (NextStep.needsBake(60i64 :: Nil, 90i64 :: Nil, Some(50i64)),
             NextStep.needsBake(50i64 :: Nil, 90i64 :: Nil, Some(50i64))))

    /// 絵が 1 枚も無いときは、前の描き出しを覚えていても描き出す (人が gallery を消した形)。
    /// アセットが無ければ作る物が無い。
    @Test
    def testNeedsBakeWithoutImages(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false),
            (NextStep.needsBake(50i64 :: Nil, Nil, Some(50i64)),
             NextStep.needsBake(Nil, Nil, None)))

    /// パレットは描き出しが毎回作り直す生成物なので、鮮度の入力から外す
    /// (外さないと、そのパレット自身が次の描き出しの理由になり続ける)。
    @Test
    def testIsSourceAsset(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, true, false),
            (NextStep.isSourceAsset("mochi.sprite.json"),
             NextStep.isSourceAsset("mochi.palette.json"),
             NextStep.isSourceAsset("palette.json"),
             NextStep.isSourceAsset("title.png")))

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
