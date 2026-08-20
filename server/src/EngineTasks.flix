// EngineTasks — ゲームに対して行う仕事 (走らせる・検査する・焼く) を、make を経由せず
// 「java -jar flix.jar …」の引数列として組み立てる係。
// 引数はリストのまま ProcessBuilder へ渡すので、置き場のパスに空白が入っていても割れない
// (make + シェル経由で必要だったクォートや別名貼りが要らなくなる)。
// 引数列の組み立ては純粋な関数に分けてある (そこだけテストする)。

mod EngineTasks {
    import java.lang.System
    import java.lang.Exception
    import java.lang.{String => JavaString}
    import java.io.{File => JFile}
    import java.nio.file.Files

    /// ゲームに頼める仕事の一覧。ここに無い仕事 (ゲーム固有の焼きなど) は
    /// EntryPoint で入口の名前を直接指す。
    pub enum Task with Eq {
        /// 実機再生 (実際のウィンドウを開く)。
        case Debug
        /// 型検査だけ。
        case Check
        /// テストを回す。
        case Test
        /// パレット (色の対応表) を書き出す。焼きの前段。
        case Palette
        /// 絵と音を焼き出す。
        case Render
        /// アトリエの試し焼き。
        case AtelierPreview
        /// 任意の入口を指名して走らせる (project.json の studioTasks 宣言が使う)。
        case EntryPoint(String)
    }

    /// java と flix.jar の実体の場所。argv を組むのに必要な 2 つで、いつも一緒に使う。
    pub type alias Exec = {javaExe = String, flixJar = String}

    // ── 純粋なルール (ここだけテストする) ──────────────────

    /// 仕事ごとの flix コンパイラへの引数 (java 引数の後ろに続く部分)。
    pub def flixArgs(task: Task): List[String] =
        match task {
            case Task.Debug            => "run" :: Nil
            case Task.Check            => "check" :: Nil
            case Task.Test             => "test" :: Nil
            case Task.Palette          => "run" :: "--entrypoint" :: "Palette.write" :: Nil
            case Task.Render           => "run" :: "--entrypoint" :: "SceneRender.all" :: Nil
            case Task.AtelierPreview   => "run" :: "--entrypoint" :: "SceneRender.atelier" :: Nil
            case Task.EntryPoint(name) => "run" :: "--entrypoint" :: name :: Nil
        }

    /// 実際のウィンドウを開かない仕事か。開かない仕事には headless を付け、描画の裏方 (AWT) が
    /// ウィンドウ側の準備を待って固まらないようにする。
    pub def isHeadless(task: Task): Bool =
        match task {
            case Task.Debug => false
            case _          => true
        }

    /// 仕事ごとの追加の環境変数。実機再生だけ DEBUG=true (ゲームが開発中の顔で起動する)。
    pub def envOf(task: Task): List[(String, String)] =
        match task {
            case Task.Debug => ("DEBUG", "true") :: Nil
            case _          => Nil
        }

    /// ヒープと GC の旗。実機再生 (Play ボタン) だけは ZGC で走らせる。
    /// ZGC は止まる時間 (GC でゲームが固まる一瞬) がヒープの大きさに関係なく短く、
    /// 遊んでいる間のカクつきが出にくい。
    /// ZGenerational は JDK 21 では既定でないので明示する (JDK 23 で既定・24 で唯一になる)。
    /// 世代別にすると、同じ物理ページを 3 つのアドレスへ映す仕組みが無くなり、
    /// OS から見えるメモリの量が実際に使っている量と合う。
    /// 上限は割合でなく絶対値で決める。ZGC は CPU を節約するために上限まで目一杯使う
    /// 設計なので、上限がそのまま実際の使用量になる — 割合だと機械の大きさで実消費が変わり、
    /// メモリ 16GB の機械では 25% = 4GB まで太る (実測: 上限 4GB でヒープ 3.2GB・見かけ 4.8GB)。
    /// ただし上限そのものを絞ると、コンパイルが通らなくなる。flix run はコンパイラとゲームが
    /// 1 つの JVM を分け合うので、上限はコンパイル時の山にも耐える必要がある
    /// (実測: 上限 512MB だと割り当て待ちでスレッドが止まり、ウィンドウが開かないまま)。
    /// そこで上限は 2GB のまま、普段の目標だけ SoftMaxHeapSize で 512MB に下げる —
    /// ZGC はこの値を超えないよう努めるが、足りないときは上限まで一時的に使う。
    /// ZUncommitDelay を既定の 300 秒から縮めるのは、コンパイルで膨らんだぶんが
    /// 遊び始めてから 5 分間 OS へ返らないため。短くしすぎない (30 秒など) のは、
    /// 場面の切り替えのたびに返却と確保を往復すると、その確保でスレッドが待たされるから。
    /// ウィンドウを開かない仕事 (検査・テスト・焼き) は据え置き:
    /// flix run はコンパイラとゲームが 1 つの JVM を分け合い、コンパイラの残した山が
    /// ヒープに居座る。既定の上限 (物理メモリの 25%) だとメモリ 8GB の機械で 2GB しか
    /// 無く、大きなゲームの焼きが詰まる。50% なら Studio 本体と同居しても押し出し合わない。
    pub def memoryArgs(task: Task): List[String] =
        match task {
            case Task.Debug =>
                "-XX:+UseZGC" :: "-XX:+ZGenerational" :: "-Xmx2g"
                    :: "-XX:SoftMaxHeapSize=512m" :: "-XX:ZUncommitDelay=60" :: Nil
            case _          => "-XX:MaxRAMPercentage=50" :: Nil
        }

    /// 起動する 1 本のコマンドの完全な引数列。
    /// -XstartOnFirstThread は macOS のウィンドウの決まりごと (他の OS の JVM はこの旗を知らず
    /// 起動に失敗する) なので、macOS のときだけ付ける。
    /// 出力の文字コードは UTF-8 に固定する — Windows の JVM は放っておくと土地の文字コード
    /// (CP932 など) で喋り、読み手 (UTF-8 固定) の側で文字化けするため。
    /// -Xss16m は削らない。Flix の型検査は式の深さぶん再帰するので、スレッドの既定スタックが
    /// 1MB の Windows では少し大きいゲームで StackOverflowError になる
    /// (macOS/arm64 は既定 2MB なので手元では気づけない)。
    /// ヒープと GC の旗は仕事で分ける (memoryArgs)。
    pub def argv(exec: Exec, isMacHost: Bool, task: Task): List[String] =
        (exec#javaExe :: Nil)
            ::: (if (isMacHost) "-XstartOnFirstThread" :: Nil else Nil)
            ::: (if (isHeadless(task)) "-Djava.awt.headless=true" :: Nil else Nil)
            ::: ("-Xss16m" :: Nil)
            ::: memoryArgs(task)
            ::: ("-Dstdout.encoding=UTF-8" :: "-Dstderr.encoding=UTF-8" :: Nil)
            ::: ("-jar" :: exec#flixJar :: Nil)
            ::: flixArgs(task)

    /// project.json の本文から studioTasks 宣言の入口を引く。宣言はゲーム固有の仕事
    /// (bake-server や gallery-sounds など) を「どの入口で走らせるか」だけを言う:
    ///   "studioTasks": { "bake-server": { "entrypoint": "SfxPreview.main" } }
    /// 形が違う・無い、は None (宣言なし)。
    pub def entryFromProjectJson(taskName: String, text: String): Option[String] =
        DocJson.parse(text)
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(Map.get("studioTasks"))
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(Map.get(taskName))
            |> Option.flatMap(JsonCodec.expectObject)
            |> Option.flatMap(Map.get("entrypoint"))
            |> Option.flatMap(JsonCodec.expectString)

    /// そのゲームがパレット (色の対応表) を持つか。焼きの前段に Palette.write を
    /// 走らせるかの判定 — 持たないゲームで走らせると、必ず失敗する 1 コンパイルを
    /// 毎回の焼きに足してしまう。
    pub def hasPalette(dir: String): Bool \ IO =
        new JFile("${dir}/src/Palette.flix").isFile()

    /// dir のゲームが taskName の入口を宣言していれば返す。読めないは None。
    pub def declaredEntry(dir: String, taskName: String): Option[String] \ IO =
        try {
            let file = new JFile("${dir}/project.json");
            if (not file.isFile()) None
            else entryFromProjectJson(taskName, new JavaString(Files.readAllBytes(file.toPath()), "UTF-8"))
        } catch {
            case _: Exception => None
        }

    // ── 実行環境の解決 ────────────────────────────────────

    /// java の選び方 (純粋)。app が教えた EDITOR_JAVA が最優先。無ければ server
    /// 自身を動かしている JVM — .app やポータブル zip は PATH に本物の java が
    /// 居ない (macOS は Apple のスタブが「Unable to locate a Java Runtime」を
    /// 返す) ので、PATH の java は最後の手にする。
    pub def resolveJava(given: Option[String], own: Option[String]): String =
        match given {
            case Some(value) if String.trim(value) != "" => value
            case _ => own |> Option.getWithDefault("java")
        }

    /// ゲームを走らせる java の実体。EDITOR_JAVA > 自分の JVM (java.home) > PATH。
    pub def javaExe(): String \ IO =
        let given = System.getenv("EDITOR_JAVA");
        let home = System.getProperty("java.home");
        let exe = if (isWindows()) "java.exe" else "java";
        let own =
            if (Object.isNull(home)) None
            else {
                let file = new JFile("${home}/bin/${exe}");
                if (file.isFile()) Some(file.getAbsolutePath()) else None
            };
        resolveJava(if (Object.isNull(given)) None else Some(given), own)

    /// engine の flix コンパイラ jar。無ければ None = タスク表では走らせられない
    /// (macOS なら従来の make に倒れる)。
    pub def flixJar(): Option[String] \ IO =
        let jar = new JFile("${EngineHome.dir()}/bin/flix.jar");
        if (jar.isFile()) Some(jar.getAbsolutePath()) else None

    /// この server が macOS で動いているか。
    pub def isMac(): Bool \ IO =
        System.getProperty("os.name") |> String.startsWith(prefix = "Mac")

    /// この server が Windows で動いているか。
    pub def isWindows(): Bool \ IO =
        System.getProperty("os.name") |> String.startsWith(prefix = "Windows")

    /// java と flix.jar が両方揃っていれば Exec を返す。揃っていなければ None。
    pub def exec(): Option[Exec] \ IO =
        flixJar() |> Option.map(jar -> {javaExe = javaExe(), flixJar = jar})
}
