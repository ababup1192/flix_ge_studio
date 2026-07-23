// TestExtend — 「ゲームを広げる」依頼文のルールを固定する。
// 文言そのものは仕様ではない (3 口・門番・節の骨格が運ばれる事だけを見る)。

mod TestExtend {

    /// kind は必須で、知らない広げ方は断る。3 口 (scene / mechanic / material) は通る。
    @Test
    def testPromptGates(): Unit \ Assert =
        let status = params -> { let (s, _) = Extend.respond("題", "/proj", params); s };
        Assert.assertEq(
            expected = (400, 400, 200, 200, 200),
            (status(Map#{}),
             status(Map#{"kind" => "chapter"}),
             status(Map#{"kind" => "scene"}),
             status(Map#{"kind" => "mechanic"}),
             status(Map#{"kind" => "material"})))

    /// どの口の依頼文も 4 節の骨格・対象 (題名と場所)・共通規約を必ず運び、
    /// あなたの言葉の欄は完了条件より後 (ユーザーが書き足す末尾の空欄)。
    @Test
    def testPromptCarriesSkeleton(): Unit \ Assert =
        let allCarry = Extend.kinds() |> List.forAll(k -> {
            let prompt = Extend.extendPrompt(k, "ほしあつめ", "/proj");
            let ordered = match (String.indexOfLeft(substr = "【完了条件】", prompt),
                                 String.indexOfLeft(substr = "【あなたの言葉】", prompt)) {
                case (Some(doneAt), Some(wordsAt)) => doneAt < wordsAt
                case _                             => false
            };
            String.contains(substr = "【やること】", prompt)
                and String.contains(substr = "【守ること】", prompt)
                and String.contains(substr = "ほしあつめ", prompt)
                and String.contains(substr = "/proj", prompt)
                and String.contains(substr = "module-index.md", prompt)
                and String.contains(substr = "/critique-code", prompt)
                and ordered
        });
        Assert.assertEq(expected = true, allCarry)

    /// material だけの追加の完了条件: editor.resources への登録 (アトリエに並ぶ)
    /// が焼き込まれる。scene には要らない。
    @Test
    def testMaterialRegistersResource(): Unit \ Assert =
        let carries = id -> Extend.kinds()
            |> List.filter(k -> k#id == id) |> List.head
            |> Option.map(k -> String.contains(substr = "editor.resources",
                Extend.extendPrompt(k, "題", "/proj")));
        Assert.assertEq(
            expected = (Some(true), Some(false)),
            (carries("material"), carries("scene")))
}
