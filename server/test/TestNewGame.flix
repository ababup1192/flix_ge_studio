// TestNewGame — 新しいゲームを産むときの「文字の書き換え」だけ固定する。
// ファイルの複製や産声の確認 (check/test/bake) は通し確認に任せ、ここは純粋な変換だけを見る。
// 期待する結果は engine の make new-game (perl の置換) と同じ形。

mod TestNewGame {

    /// テスト用の材料一式。
    def request(): NewGame.Request =
        { engine = "/engines/e", gameDir = "/games/g", name = "mygame",
          title = "My Game's", w = 240, h = 320, template = "game-starter" }

    /// プレースホルダの置き換え: __NAME__/__TITLE__/__W__/__H__ と、ウィンドウの大きさ __WW__/__WH__ (2 倍)。
    /// 題名にアポストロフィが入ってもそのまま入る (シェルを通らないので割れない)。
    @Test
    def testApplyTokens(): Unit \ Assert =
        Assert.assertEq(
            expected = "name=mygame title=My Game's w=240 h=320 win=480x640 engine=/engines/e",
            NewGame.applyTokens(request(),
                "name=__NAME__ title=__TITLE__ w=__W__ h=__H__ win=__WW__x__WH__ engine=__ENGINE__"))

    /// local.mk の本文: engine の場所は必ず書き、java は教わったときだけ書く。
    /// Windows のパス (\ 区切り) は / に直って入る (make/sh が読める形)。
    @Test
    def testLocalMkText(): Unit \ Assert =
        Assert.assertEq(
            expected = (
                "# このマシンの実行環境 (git には入れない — .gitignore 済み)。Studio が生成。\nENGINE ?= /engines/e\n",
                "# このマシンの実行環境 (git には入れない — .gitignore 済み)。Studio が生成。\nENGINE ?= C:/Studio/resources/engine\nJAVA ?= C:/Studio/resources/jre/bin/java.exe\n"),
            (NewGame.localMkText("/engines/e", None),
             NewGame.localMkText("C:\\Studio\\resources\\engine", Some("C:\\Studio\\resources\\jre\\bin\\java.exe"))))

    /// local.mk を書くのは「Makefile があって local.mk がまだ無くて engine の場所が分かる」ときだけ。
    /// 既にある local.mk は上書きしない。engine が分からなければ何も書かない。
    @Test
    def testShouldWriteLocalMk(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, false, false, false),
            (NewGame.shouldWriteLocalMk(true, false, Some("/engines/e")),
             NewGame.shouldWriteLocalMk(true, true, Some("/engines/e")),
             NewGame.shouldWriteLocalMk(false, false, Some("/engines/e")),
             NewGame.shouldWriteLocalMk(true, false, None)))

    /// 書き換え対象は文字ファイル (.flix/.json/.toml/.md/Makefile) だけ。絵や音は写すだけ。
    @Test
    def testIsTokenTarget(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, true, true, true, false, false),
            (NewGame.isTokenTarget("Main.flix"),
             NewGame.isTokenTarget("project.json"),
             NewGame.isTokenTarget("flix.toml"),
             NewGame.isTokenTarget("README.md"),
             NewGame.isTokenTarget("Makefile"),
             NewGame.isTokenTarget("title.png"),
             NewGame.isTokenTarget("bgm.wav")))

    /// flix.toml から version の中身を取り出す。前後の空白は許し、無ければ None。
    @Test
    def testVersionFromToml(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("0.15.0"), None),
            (NewGame.versionFromToml("[package]\nname = \"x\"\nversion     = \"0.15.0\"\n"),
             NewGame.versionFromToml("[package]\nname = \"x\"\n")))

    /// engine の Makefile から VERSION := を読む。規則は engine の bin/fge status と同じ:
    /// 行頭 VERSION → 空白 → := → 次の 1 語。行頭に # や空白が挟まる行は読まない。
    @Test
    def testVersionFromMakefile(): Unit \ Assert =
        Assert.assertEq(
            expected = (Some("0.23.4"), Some("2.0"), None, None),
            (NewGame.versionFromMakefile("# engine\nVERSION := 0.23.4\nGAME ?= x\n"),
             NewGame.versionFromMakefile("VERSION:=2.0\n"),
             NewGame.versionFromMakefile("# VERSION := 1.0\n  VERSION := 9.9\n"),
             NewGame.versionFromMakefile("VERSIONX := 1.0\n")))

    /// SKILL.md の frontmatter から description: を取り出す。trim して囲みの " を
    /// 1 個ずつ外す (中の " は残す)。無ければ "" に倒れる。
    @Test
    def testSkillDescriptionOf(): Unit \ Assert =
        Assert.assertEq(
            expected = ("絵の手順。", "そのまま", ""),
            (NewGame.skillDescriptionOf("---\nname: v\ndescription: \"絵の手順。\"\nsync: 1\n---\n本文"),
             NewGame.skillDescriptionOf("description:   そのまま  \n"),
             NewGame.skillDescriptionOf("---\nname: v\n---\n")))

    /// description から一覧に載せる 1 文を選ぶ。「〜使う」で終わる文があればそれ、
    /// 無ければ先頭の 1 文（engine の bin/fge sync-agents の triggerSentence と同じ規則）。
    @Test
    def testTriggerSentence(): Unit \ Assert =
        Assert.assertEq(
            expected = ("絵を描くときに使う。", "能書き。", "句点なし。"),
            (NewGame.triggerSentence("能書き。絵を描くときに使う。あとがき。"),
             NewGame.triggerSentence("能書き。あとがき。"),
             NewGame.triggerSentence("句点なし")))

    /// スキル一覧の節: 見出し + 空行 + 「- `.claude/skills/名前/SKILL.md` — 説明」を
    /// 渡した順 (呼び手が名前昇順で渡す) に 1 行ずつ。末尾に改行は付かない。
    @Test
    def testSkillsSection(): Unit \ Assert =
        Assert.assertEq(
            expected = "## 配られているスキル一覧（sync-agents が自動生成。手で編集しない）\n\n- `.claude/skills/bake-loop/SKILL.md` — 絵の直し方\n- `.claude/skills/flix-docs/SKILL.md` — Flix の決まり",
            NewGame.skillsSection(("bake-loop", "絵の直し方") :: ("flix-docs", "Flix の決まり") :: Nil))

    /// AGENTS.md の組み立て: 刻印 + 空行 + 共通部 + 空行 + スキル一覧の節、
    /// ゲーム固有部があれば空行で続く。終端は改行 1 個
    /// (engine の bin/fge sync-agents の buildAgentsMD とバイト一致する形)。
    @Test
    def testAgentsText(): Unit \ Assert =
        Assert.assertEq(
            expected = (
                "<!-- generated by flix_game_engine agents-pack (engine v1.0). 編集しないで。共通部は engine の agents-pack/AGENTS.core.md、ゲーム固有部は AGENTS.local.md を編集して再 sync -->\n\ncore\n\n## 配られているスキル一覧（sync-agents が自動生成。手で編集しない）\n\n- `.claude/skills/a/SKILL.md` — 説明\n\nlocal\n",
                "<!-- generated by flix_game_engine agents-pack (engine v1.0). 編集しないで。共通部は engine の agents-pack/AGENTS.core.md、ゲーム固有部は AGENTS.local.md を編集して再 sync -->\n\ncore\n\n## 配られているスキル一覧（sync-agents が自動生成。手で編集しない）\n\n- `.claude/skills/a/SKILL.md` — 説明\n"),
            (NewGame.agentsText("1.0", "core\n\n", ("a", "説明") :: Nil, Some("local\n\n")),
             NewGame.agentsText("1.0", "core\n", ("a", "説明") :: Nil, None)))

    /// manifest の 3 節 (copy / copyIfAbsent / copyDirs) が (src, dst) の一覧に解ける。
    @Test
    def testManifestPlan(): Unit \ Assert =
        Assert.assertEq(
            expected = Some((
                ("bin/lint-view.py", "bin/lint-view.py") :: Nil,
                ("agents-pack/settings.json", ".claude/settings.json") :: Nil,
                ("agents-pack/skills", ".claude/skills") :: ("agents-pack/skills", ".agents/skills") :: Nil)),
            NewGame.manifestPlan("{\"version\": 1, \"note\": \"x\", \"copy\": [{\"src\": \"bin/lint-view.py\", \"dst\": \"bin/lint-view.py\"}], \"copyIfAbsent\": [{\"src\": \"agents-pack/settings.json\", \"dst\": \".claude/settings.json\"}], \"copyDirs\": [{\"src\": \"agents-pack/skills\", \"dst\": \".claude/skills\"}, {\"src\": \"agents-pack/skills\", \"dst\": \".agents/skills\"}]}")
                |> Option.map(plan -> (plan#copyList, plan#copyIfAbsentList, plan#copyDirsList)))

    /// 節が 1 つでも欠けた manifest は全体 None (半端に配らず fail-open へ倒すため)。
    @Test
    def testManifestPlanMissingSection(): Unit \ Assert =
        Assert.assertEq(
            expected = None,
            NewGame.manifestPlan("{\"copy\": [], \"copyIfAbsent\": []}")
                |> Option.map(plan -> plan#copyDirsList))

    /// 要素に dst が無い manifest も None (壊れた一覧の一部だけ配らない)。
    @Test
    def testManifestPlanBrokenEntry(): Unit \ Assert =
        Assert.assertEq(
            expected = None,
            NewGame.manifestPlan("{\"copy\": [{\"src\": \"bin/a.py\"}], \"copyIfAbsent\": [], \"copyDirs\": []}")
                |> Option.map(plan -> plan#copyList))
}
