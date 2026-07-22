// TestAtelier — 昇格のルールだけ固定する: 互換判定・退避の名前づけ・経路の門番。
// ファイルの入れ替えや HTTP は curl での通し確認に任せ、ここは判定だけを見る。

mod TestAtelier {

    /// 互換: スロットに entityId があれば一致が条件 (名前が違っても通る・不一致や無しは弾く)。
    /// スロットに entityId が無ければ ".<kind>.json" の尻尾一致に落ちる (1 語 JSON は落ちない)。
    @Test
    def testIsCompatible(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, true, false, false),
            (Atelier.isCompatible(Some("prologue.sprites"), "prologue.sprite.json",
                                  Some("prologue.sprites"), "villager.a.sprite.json"),
             Atelier.isCompatible(Some("prologue.sprites"), "prologue.sprite.json",
                                  Some("other.sprites"), "villager.a.sprite.json"),
             Atelier.isCompatible(Some("prologue.sprites"), "prologue.sprite.json",
                                  None, "villager.a.sprite.json"),
             Atelier.isCompatible(None, "spring.theme.json", None, "moss.theme.json"),
             Atelier.isCompatible(None, "spring.theme.json", None, "villager.sprite.json"),
             Atelier.isCompatible(None, "plain.json", None, "other.json")))

    /// 退避の名前: "<base>.prev-N.<kind>.json"。多段の base も kind の手前で切る。
    @Test
    def testRetiredName(): Unit \ Assert =
        Assert.assertEq(
            expected = ("prologue.prev-1.sprite.json", "water.prev-3.shader.json", "plain.prev-2.json"),
            (Atelier.retiredName("prologue.sprite.json", 1),
             Atelier.retiredName("water.shader.json", Atelier.prevKeep()),
             Atelier.retiredName("plain.json", 2)))

    /// 退避ファイルの見分け: prev-数字 の節だけが退避 (似た名前・数字なしは違う)。
    @Test
    def testIsPrevName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false),
            (Atelier.isPrevName("prologue.prev-1.sprite.json"),
             Atelier.isPrevName("plain.prev-12.json"),
             Atelier.isPrevName("prologue.sprite.json"),
             Atelier.isPrevName("preview.sprite.json"),
             Atelier.isPrevName("x.prev-.json")))

    /// 経路の門番: 候補は atelier/ 直下、スロットは assets/ 直下の JSON 1 語のみ。
    /// `..`・深い経路・絶対経路・別の置き場は弾く。
    @Test
    def testPathGuards(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false, true, false, false),
            (Atelier.isCandidatePath("atelier/villager.a.sprite.json"),
             Atelier.isCandidatePath("atelier/../assets/prologue.sprite.json"),
             Atelier.isCandidatePath("atelier/deep/x.sprite.json"),
             Atelier.isCandidatePath("/atelier/x.sprite.json"),
             Atelier.isSlotPath("assets/prologue.sprite.json"),
             Atelier.isSlotPath("atelier/prologue.sprite.json"),
             Atelier.isSlotPath("assets/PixelMplus10-Regular.ttf")))

    /// プレビューの置き場: 候補は "<名前>.png"、スロットは "<名前>.current.png"。
    /// 門番落ちの経路 (深い経路・別の置き場) は None。
    @Test
    def testPreviewName(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("debug/atelier/villager.a.sprite.png"),
                        Some("debug/atelier/prologue.sprite.current.png"),
                        None, None),
            (Atelier.previewName("atelier/villager.a.sprite.json"),
             Atelier.previewName("assets/prologue.sprite.json"),
             Atelier.previewName("atelier/../assets/prologue.sprite.json"),
             Atelier.previewName("debug/atelier/x.png")))

    /// 焼き直しの要否: PNG が無い (0)・JSON より古い、は要。同時刻・より新しい、は不要。
    @Test
    def testNeedsBake(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false),
            (Atelier.needsBake(100i64, 0i64),
             Atelier.needsBake(100i64, 99i64),
             Atelier.needsBake(100i64, 100i64),
             Atelier.needsBake(100i64, 101i64)))
}
