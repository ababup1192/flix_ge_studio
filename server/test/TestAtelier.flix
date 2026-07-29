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
             Atelier.isCompatible(None, "spring.theme.json", None, "spring.a.theme.json"),
             Atelier.isCompatible(None, "spring.theme.json", None, "villager.sprite.json"),
             Atelier.isCompatible(None, "autumn.theme.json", None, "summer.a.theme.json")))

    /// バージョンの名前: "<base>.vN.<kind>.json"。多段の base も kind の手前で切る。
    /// 名前からスロット名へも戻せる (アーカイブの「vN に戻す」の宛先)。
    @Test
    def testArchivedName(): Unit \ Assert =
        Assert.assertEq(
            expected = ("prologue.v1.sprite.json", "water.v12.shader.json", "plain.v2.json",
                        "prologue.sprite.json"),
            (Atelier.archivedName("prologue.sprite.json", 1),
             Atelier.archivedName("water.shader.json", 12),
             Atelier.archivedName("plain.json", 2),
             Atelier.slotNameOfVersion("prologue.v3.sprite.json")))

    /// バージョン番号: 自分のスロットの最大 + 1。他スロットの物・候補名は数えない。
    /// 無ければ v1 から。歯抜け (v1, v3) でも最大基準で v4。
    @Test
    def testNextVer(): Unit \ Assert =
        Assert.assertEq(
            expected = (1, 4, 2),
            (Atelier.nextVerFrom(Nil, "prologue.sprite.json"),
             Atelier.nextVerFrom("prologue.v1.sprite.json" :: "prologue.v3.sprite.json"
                 :: "water.v9.shader.json" :: "villager.a.sprite.json" :: Nil,
                 "prologue.sprite.json"),
             Atelier.nextVerFrom("water.v1.shader.json" :: Nil, "water.shader.json")))

    /// バージョンの見分け: v数字 の節がある名前だけ (候補名 villager.a や v だけの節は違う)。
    @Test
    def testIsVersionName(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, false, false, false),
            (Atelier.isVersionName("prologue.v1.sprite.json"),
             Atelier.isVersionName("plain.v12.json"),
             Atelier.isVersionName("villager.a.sprite.json"),
             Atelier.isVersionName("prologue.sprite.json"),
             Atelier.isVersionName("x.v.json")))

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

    /// プロンプトの組み立て: スキル名・スロット・案数 (1〜9 に丸め)・方向性が本文に載る。
    /// 方向性の省略は「おまかせ」、entityId 無しは「なし」に落ちる。
    @Test
    def testAtelierPrompt(): Unit \ Assert =
        let prompt = Atelier.atelierPrompt("/proj", "assets/prologue.sprite.json",
            Some("prologue.sprites"), "assets/sprite.schema.json", 3, Some("冬らしく"));
        let fallback = Atelier.atelierPrompt("/proj", "assets/x.theme.json", None,
            "assets/theme.schema.json", 99, None);
        Assert.assertEq(
            expected = (true, true, true, true, true, true, true, true),
            (String.contains(substr = "/atelier-draft", prompt),
             String.contains(substr = "assets/prologue.sprite.json", prompt),
             String.contains(substr = "entityId: prologue.sprites", prompt),
             String.contains(substr = "案数: 3", prompt),
             String.contains(substr = "方向性: 冬らしく", prompt),
             String.contains(substr = "案数: 9", fallback),
             String.contains(substr = "方向性: おまかせ", fallback),
             String.contains(substr = "/wire-doc", Atelier.wirePrompt("/proj", "assets/x.kind.json"))))

    /// 自作コピーの名前づけ: スロットの種類の尻尾を受け継ぐ。名前の門番は
    /// 英数・_ - と途中の . だけを通す (区切り・.. ・端の . は弾く)。
    @Test
    def testCopyNaming(): Unit \ Assert =
        Assert.assertEq(
            expected = ("villager.d.sprite.json", "moss.theme.json", true, false, false, false, false),
            (Atelier.copyTargetName("prologue.sprite.json", "villager.d"),
             Atelier.copyTargetName("spring.theme.json", "moss"),
             Atelier.isCopyBaseName("villager.d"),
             Atelier.isCopyBaseName(""),
             Atelier.isCopyBaseName("a/b"),
             Atelier.isCopyBaseName("a..b"),
             Atelier.isCopyBaseName(".hidden")))

    /// 素材の選別: role="material" の宣言に pattern が当たるスロットだけが対象。
    /// role="tuning" も role 欠落 (fail-open で tuning 扱い) も外れる。
    @Test
    def testMaterialFiltering(): Unit \ Assert =
        let decl = (id, pattern, role) ->
            { id = id, pattern = pattern, title = None, schemaPath = None,
              plugin = None, builtin = false, role = role,
              bakeUrl = None, bakeCmd = None, performCmd = None, performUrl = None };
        let decls = decl("sprite", "assets/*.sprite.json", Some("material"))
            :: decl("balance", "assets/balance.kind.json", Some("tuning"))
            :: decl("terrain", "assets/terrain.kind.json", None)
            :: Nil;
        // 先勝ち: 同じファイルに当たる素材宣言が並んでいたら、宣言順で最初のものを採る
        // (村の prologueTheme が汎用 theme より前に置かれる並び順を前提にしている)。
        let overlapping = decl("prologueTheme", "assets/*.theme.json", Some("material"))
            :: decl("theme", "assets/*.theme.json", Some("material"))
            :: Nil;
        Assert.assertEq(
            expected = (Some("sprite"), None, None, None, Some("prologueTheme")),
            (ResourceDecl.materialDeclFor(decls, "assets/prologue.sprite.json") |> Option.map(d -> d#id),
             ResourceDecl.materialDeclFor(decls, "assets/balance.kind.json") |> Option.map(d -> d#id),
             ResourceDecl.materialDeclFor(decls, "assets/terrain.kind.json") |> Option.map(d -> d#id),
             ResourceDecl.materialDeclFor(decls, "assets/other.kind.json") |> Option.map(d -> d#id),
             ResourceDecl.materialDeclFor(overlapping, "assets/prologue.theme.json") |> Option.map(d -> d#id)))

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
