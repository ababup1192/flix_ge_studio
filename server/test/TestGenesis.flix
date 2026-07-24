// TestGenesis — 家族の札と公式プロンプトのルールを固定する。
// 文言そのものは仕様ではない (id・並び・必須パラメータ・骨格が運ばれる事だけを見る)。

mod TestGenesis {

    def ids(): List[String] =
        Genesis.families() |> List.map(f -> f#id)

    /// 札は 9 枚で、先頭は action(いちばん人気 = 通し体験の入口)、
    /// 末尾は free(家族に当てはまらない受け皿)。並び = 人気順は仕様。
    @Test
    def testFamiliesShape(): Unit \ Assert =
        Assert.assertEq(
            expected = (9, Some("action"), Some("free")),
            (List.length(ids()), List.head(ids()), List.last(ids())))

    /// family は必須・未知の家族は断る。free は direction が必須。
    @Test
    def testPromptGates(): Unit \ Assert =
        let status = params -> { let (s, _) = Genesis.promptJson(params); s };
        Assert.assertEq(
            expected = (400, 400, 400, 200),
            (status(Map#{}),
             status(Map#{"family" => "villageSim"}),
             status(Map#{"family" => "free"}),
             status(Map#{"family" => "free", "direction" => "猫が魚を集める"})))

    /// 公式テンプレートの登録: action / novel / rpg / puzzle は starter 持ち、free は持たない。
    @Test
    def testStarters(): Unit \ Assert =
        let starterOf = id -> Genesis.families()
            |> List.filter(f -> f#id == id) |> List.head
            |> Option.map(f -> f#starter != "");
        Assert.assertEq(
            expected = (Some(true), Some(true), Some(true), Some(true), Some(false)),
            (starterOf("action"), starterOf("novel"), starterOf("rpg"), starterOf("puzzle"), starterOf("free")))

    /// 複製元の白名簿: 登録済みテンプレートだけ通す(空・未知・似た経路は弾く)。
    @Test
    def testIsStarter(): Unit \ Assert =
        Assert.assertEq(
            expected = (true, true, true, false, false, false),
            (Genesis.isStarter("templates/novel-starter"),
             Genesis.isStarter("templates/rpg-starter"),
             Genesis.isStarter("templates/tetris-starter"),
             Genesis.isStarter(""),
             Genesis.isStarter("templates/unknown"),
             Genesis.isStarter("../templates/novel-starter")))

    /// 公式プロンプトは骨格と土台(テンプレート変形)を必ず運ぶ。
    /// あなたの言葉(world 等)はパラメータで埋まり、空なら欄が残る。
    @Test
    def testFamilyPromptCarriesSkeleton(): Unit \ Assert =
        let rpg = Genesis.families() |> List.filter(f -> f#id == "rpg") |> List.head;
        match rpg {
            case None => Assert.assertEq(expected = true, false)
            case Some(f) =>
                let filled = Genesis.familyPrompt(f, Map#{"world" => "夜の温泉街"});
                let blank = Genesis.familyPrompt(f, Map#{});
                Assert.assertEq(
                    expected = (true, true, true, true),
                    (String.contains(substr = f#skeleton, filled),
                     String.contains(substr = "templates/game-starter", filled),
                     String.contains(substr = "夜の温泉街", filled),
                     String.contains(substr = "あなたの言葉", blank)))
        }
}
