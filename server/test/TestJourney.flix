// TestJourney — 「つぎの一手」を決める優先順位と、素材の鮮度判定のルールを固定する。
// 文言そのものは仕様ではないので見ない (id と nav だけを見る)。

mod TestJourney {

    def checks(candidates: Int32, stale: Bool, diffs: Int32): Journey.Checks =
        { atelierCandidates = candidates, staleGallery = stale, diffCount = diffs,
          goldenEmpty = false, hasStarterDoc = false }

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


    /// 産まれたて (お手本が空) は、通常の祝福ではなく説明つきの firstBless になる。
    /// 候補が居るときはそちらが勝つ (優先順位は据え置き)。
    @Test
    def testFirstBless(): Unit \ Assert =
        Assert.assertEq(
            expected = ("firstBless", "bless", "pick"),
            (Journey.suggest({ atelierCandidates = 0, staleGallery = false, diffCount = 1, goldenEmpty = true, hasStarterDoc = false })#id,
             Journey.suggest({ atelierCandidates = 0, staleGallery = false, diffCount = 1, goldenEmpty = false, hasStarterDoc = false })#id,
             Journey.suggest({ atelierCandidates = 2, staleGallery = false, diffCount = 1, goldenEmpty = true, hasStarterDoc = false })#id))

    /// 産まれたてスターター (見本 Doc が居て、他にやる事が無い) は create でなく design。
    /// pick・bake・bless・firstBless はどれも design より優先。見本 Doc が無ければ create のまま。
    @Test
    def testDesign(): Unit \ Assert =
        def withStarter(candidates: Int32, stale: Bool, diffs: Int32, golden: Bool): String =
            Journey.suggest({ atelierCandidates = candidates, staleGallery = stale,
                              diffCount = diffs, goldenEmpty = golden, hasStarterDoc = true })#id;
        Assert.assertEq(
            expected = (("design", "atelier"), "create", "pick", "bake", "bless", "firstBless"),
            ((idNav(Journey.suggest({ atelierCandidates = 0, staleGallery = false,
                                      diffCount = 0, goldenEmpty = false, hasStarterDoc = true }))),
             Journey.suggest(checks(0, false, 0))#id,
             withStarter(1, false, 0, false),
             withStarter(0, true, 0, false),
             withStarter(0, false, 1, false),
             withStarter(0, false, 1, true)))
}
