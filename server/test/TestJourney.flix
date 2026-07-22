// TestJourney — 「つぎの一手」を決める優先順位と、素材の鮮度判定のルールを固定する。
// 文言そのものは仕様ではないので見ない (id と nav だけを見る)。

mod TestJourney {

    def checks(candidates: Int32, stale: Bool, diffs: Int32): Journey.Checks =
        { atelierCandidates = candidates, staleGallery = stale, diffCount = diffs }

    def idNav(suggestion: Journey.Suggestion): (String, String) =
        (suggestion#id, suggestion#nav)

    /// 優先順位: 候補あり → pick が最優先 (鮮度や差分が同時に立っていても勝つ)。
    /// 次に stale → bake、次に差分 → bless、どれも無し → create。
    @Test
    def testSuggestPriority(): Unit \ Assert =
        Assert.assertEq(
            expected = (("pick", "atelier"), ("bake", "gallery"),
                        ("bless", "gallery"), ("create", "atelier")),
            (idNav(Journey.suggest(checks(2, true, 3))),
             idNav(Journey.suggest(checks(0, true, 3))),
             idNav(Journey.suggest(checks(0, false, 3))),
             idNav(Journey.suggest(checks(0, false, 0)))))

    /// 鮮度: 素材の最新 > 絵の最古 で stale。絵が無くても素材があれば stale。
    /// 素材が無ければ焼く物が無いので stale ではない。同時刻は stale ではない。
    @Test
    def testIsStale(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false),
            (Journey.isStale(10i64 :: 30i64 :: Nil, 20i64 :: 25i64 :: Nil),
             Journey.isStale(10i64 :: Nil, Nil),
             Journey.isStale(Nil, 20i64 :: Nil),
             Journey.isStale(Nil, Nil),
             Journey.isStale(20i64 :: Nil, 20i64 :: Nil)))
}
