// TestEngineTasks — タスク表のルールだけ固定する: 引数列の組み立て・headless の使い分け・
// 追加の環境変数。java や flix.jar の実体の解決 (環境変数や置き場の存在) は通し確認に任せる。
//
// 期待する引数列は engine の bin/flix ラッパが展開していたコマンドと同じ形 + UTF-8 固定:
//   java [-XstartOnFirstThread(mac)] [-Djava.awt.headless=true(窓なし)]
//        -Xss16m -XX:MaxRAMPercentage=50 -Dstdout.encoding=UTF-8 -Dstderr.encoding=UTF-8
//        -jar flix.jar <flix引数>

mod TestEngineTasks {
    use EngineTasks.Task

    /// argv の全体形: macOS の実機再生は窓の旗だけ・焼きは窓の旗 + headless。
    /// macOS 以外は窓の旗が消える (他の OS の JVM はこの旗で起動に失敗するため)。
    /// 出力の UTF-8 固定と積むスタックの広げ (-Xss16m) はどの OS のどの仕事にも必ず付く
    /// (Windows の文字化け対策と、型検査の深い再帰でスタックが溢れるのを防ぐため)。
    @Test
    def testArgv(): Unit \ Assert =
        let exec = {javaExe = "/jre/bin/java", flixJar = "/engine/bin/flix.jar"};
        let always = "-Xss16m" :: "-XX:MaxRAMPercentage=50" :: "-Dstdout.encoding=UTF-8" :: "-Dstderr.encoding=UTF-8" :: Nil;
        Assert.assertEq(
            expected = (
                ("/jre/bin/java" :: "-XstartOnFirstThread" :: always) ::: ("-jar" :: "/engine/bin/flix.jar" :: "run" :: Nil),
                ("/jre/bin/java" :: "-XstartOnFirstThread" :: "-Djava.awt.headless=true" :: always) ::: ("-jar" :: "/engine/bin/flix.jar" :: "run" :: "--entrypoint" :: "Bake.all" :: Nil),
                ("/jre/bin/java" :: "-Djava.awt.headless=true" :: always) ::: ("-jar" :: "/engine/bin/flix.jar" :: "run" :: "--entrypoint" :: "Bake.all" :: Nil),
                ("/jre/bin/java" :: always) ::: ("-jar" :: "/engine/bin/flix.jar" :: "run" :: Nil)),
            (EngineTasks.argv(exec, true, Task.Debug),
             EngineTasks.argv(exec, true, Task.Bake),
             EngineTasks.argv(exec, false, Task.Bake),
             EngineTasks.argv(exec, false, Task.Debug)))

    /// java の選び方: EDITOR_JAVA が最優先・無ければ自分の JVM・どちらも無ければ
    /// PATH の java。空白だけの EDITOR_JAVA は「教わっていない」扱い。
    @Test
    def testResolveJava(): Unit \ Assert =
        Assert.assertEq(
            expected = ("/jre/bin/java", "/own/bin/java", "/own/bin/java", "java"),
            (EngineTasks.resolveJava(Some("/jre/bin/java"), Some("/own/bin/java")),
             EngineTasks.resolveJava(None, Some("/own/bin/java")),
             EngineTasks.resolveJava(Some("  "), Some("/own/bin/java")),
             EngineTasks.resolveJava(None, None)))

    /// 引数列に空白入りのパスが来てもそのまま 1 語で残る (リスト渡しなのでクォート不要)。
    @Test
    def testArgvKeepsSpaces(): Unit \ Assert =
        let exec = {javaExe = "C:\\Program Files\\Studio\\jre\\bin\\java.exe", flixJar = "C:\\Program Files\\Studio\\engine\\bin\\flix.jar"};
        Assert.assertEq(
            expected = "C:\\Program Files\\Studio\\jre\\bin\\java.exe" :: "-Djava.awt.headless=true" :: "-Xss16m" :: "-XX:MaxRAMPercentage=50" :: "-Dstdout.encoding=UTF-8" :: "-Dstderr.encoding=UTF-8" :: "-jar" :: "C:\\Program Files\\Studio\\engine\\bin\\flix.jar" :: "check" :: Nil,
            EngineTasks.argv(exec, false, Task.Check))

    /// flix コンパイラへの引数: 標準の仕事は決まった形、EntryPoint は指名した入口で走る。
    @Test
    def testFlixArgs(): Unit \ Assert =
        Assert.assertEq(
            expected = (
                "run" :: Nil,
                "check" :: Nil,
                "test" :: Nil,
                "run" :: "--entrypoint" :: "Palette.write" :: Nil,
                "run" :: "--entrypoint" :: "Bake.atelier" :: Nil,
                "run" :: "--entrypoint" :: "SfxPreview.main" :: Nil),
            (EngineTasks.flixArgs(Task.Debug),
             EngineTasks.flixArgs(Task.Check),
             EngineTasks.flixArgs(Task.Test),
             EngineTasks.flixArgs(Task.Palette),
             EngineTasks.flixArgs(Task.AtelierPreview),
             EngineTasks.flixArgs(Task.EntryPoint("SfxPreview.main"))))

    /// 実窓を開くのは実機再生 (Debug) だけ。他は全部 headless。
    @Test
    def testIsHeadless(): Unit \ Assert =
        Assert.assertEq(
            expected = (false, true, true, true, true, true, true),
            (EngineTasks.isHeadless(Task.Debug),
             EngineTasks.isHeadless(Task.Check),
             EngineTasks.isHeadless(Task.Test),
             EngineTasks.isHeadless(Task.Palette),
             EngineTasks.isHeadless(Task.Bake),
             EngineTasks.isHeadless(Task.AtelierPreview),
             EngineTasks.isHeadless(Task.EntryPoint("X.y"))))

    /// 追加の環境変数は実機再生の DEBUG=true だけ。
    @Test
    def testEnvOf(): Unit \ Assert =
        Assert.assertEq(
            expected = (("DEBUG", "true") :: Nil, (Nil: List[(String, String)])),
            (EngineTasks.envOf(Task.Debug), EngineTasks.envOf(Task.Bake)))

    /// studioTasks 宣言の読み方: 名前の一致した仕事の entrypoint だけ返す。
    /// 宣言が無い・別の仕事・形が壊れている、は None (宣言なしに倒す)。
    @Test
    def testEntryFromProjectJson(): Unit \ Assert =
        let declared = "{\"title\":\"x\",\"studioTasks\":{\"bake-server\":{\"entrypoint\":\"SfxPreview.main\"},\"gallery-sounds\":{\"entrypoint\":\"SfxBake.all\"}}}";
        Assert.assertEq(
            expected = (Some("SfxPreview.main"), Some("SfxBake.all"), None, None, None),
            (EngineTasks.entryFromProjectJson("bake-server", declared),
             EngineTasks.entryFromProjectJson("gallery-sounds", declared),
             EngineTasks.entryFromProjectJson("gallery-prologue", declared),
             EngineTasks.entryFromProjectJson("bake-server", "{\"title\":\"x\"}"),
             EngineTasks.entryFromProjectJson("bake-server", "こわれた json")))
}
