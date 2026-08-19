// EngineHome — ゲームを走らせる・絵を書き出す・新しく作るのに使う engine の置き場を
// 1 か所で決める係。
// 使うのは EngineStore が .app の外へ写した engines/<バージョン>/。まだ写していない間だけ、
// app が EDITOR_ENGINE で教えてくる同梱先に倒れる。手元の engine リポで開発している
// ときは既定に倒れる。
//
// ゲームの Makefile は `ENGINE ?= …` を持つので、ここで決めた場所を環境変数 ENGINE として
// 渡せば、ゲーム側の直書きより優先される (Runner と BakeHost が渡す)。

mod EngineHome {
    import java.lang.System
    import java.lang.ProcessBuilder
    import java.lang.Exception
    import java.io.{File => JFile}
    import java.util.ArrayList

    /// engine の置き場。engines/<バージョン>/ (EngineStore の指し先) → EDITOR_ENGINE
    /// (app が同梱先を教える) → 手元の既定、の順で決める。
    pub def dir(): String \ IO =
        let value = System.getenv("EDITOR_ENGINE");
        let given = if (Object.isNull(value)) None else Some(value);
        resolve(EngineStore.currentDir(), given, "${System.getProperty("user.home")}")

    /// ゲームの make に渡す ENGINE の値。走らせる手を持たない置き場なら None =
    /// 何も渡さない。make に倒れる道 (macOS 専用) だけが使う — タスク表 (EngineTasks) の
    /// 道はシェルを通らないので、この別名貼り (ln) もクォートの心配も要らない。
    // WhyNot: 使えない場所でも渡してしまう、はしない — ゲームの Makefile が持つ既定で
    // 動ける見込みを、こちらの空振りで潰さないため。
    pub def forGames(): Option[String] \ IO =
        let home = dir();
        if (not usable(home)) None
        else if (isStorePath(System.getProperty("user.home"), home))
            // WhyNot: 別名を貼れなかったときに実パスへ倒れ切らない — 名前に空白のある
            // ホーム (`/Users/Foo Bar/…`) では渡した先で道が千切れる。
            match linkedAlias(home) {
                case Some(link) => Some(link)
                case None       => if (needsAlias(home)) None else Some(home)
            }
        else if (not needsAlias(home)) Some(home)
        else linkedAlias(home)

    /// 別名 (~/.flix_ge_studio/engine) を今の置き場へ張り直す。指し先が変わった直後に呼ぶ。
    pub def relinkAlias(): Unit \ IO =
        let home = dir();
        if (usable(home)) discard linkedAlias(home) else ()

    /// その置き場が「走らせる手」を持っているか。
    pub def usable(home: String): Bool \ IO =
        new JFile("${home}/bin/flix").canExecute()

    /// 空白を含まない別名 ($HOME/.flix_ge_studio/engine) を貼り直して返す。
    /// 貼れなければ None = ENGINE を渡さない (渡しても割れる道なので賭けない)。
    def linkedAlias(home: String): Option[String] \ IO =
        let base = "${System.getProperty("user.home")}/.flix_ge_studio";
        let link = "${base}/engine";
        discard new JFile(base).mkdirs();
        // -sfn で毎回貼り替える。Studio を入れ替えても古い場所を指したままにしない。
        if (runCommand("ln" :: "-sfn" :: home :: link :: Nil) and usable(link)) Some(link) else None

    /// 外部コマンドを 1 回走らせて成否だけ返す (Editor.runCommand と同じ作法)。
    def runCommand(args: List[String]): Bool \ IO =
        try {
            let list = new ArrayList();
            foreach (arg <- args) discard list.add(arg);
            let pb = new ProcessBuilder(list);
            let env = pb.environment();
            discard env.remove("DEVELOPER_DIR");
            discard env.remove("SDKROOT");
            pb.start().waitFor() == 0
        } catch {
            case _: Exception => false
        }

    // ── 純粋なルール ──────────────────────────────────────

    /// 置き場を決める規則。空文字は「教わっていない」と同じに扱う。
    pub def resolve(stored: Option[String], given: Option[String], userHome: String): String =
        match (stored, given) {
            case (Some(path), _)  if String.trim(path) != ""  => path
            case (_, Some(value)) if String.trim(value) != "" => value
            case _ => "${userHome}/Desktop/flix_game_engine"
        }

    /// その置き場が engines/<バージョン>/ の中か。
    // WhyNot: 置き場の中なら空白の有無に関わらず別名を通す — この値はゲームの local.mk に
    // 書き込まれて残るので、バージョンを含む実パスを書くと、更新で指し先が変わったあとも
    // ゲームだけ古いバージョンを指したままになるため。
    pub def isStorePath(userHome: String, home: String): Bool =
        String.startsWith(prefix = "${EngineStore.versionsDir(userHome)}/", home)

    /// そのまま渡すと割れる道か。ゲームの Makefile は `FLIX := $(ENGINE)/bin/flix` を
    /// クォート無しで展開するので、空白が 1 つでもあると別々の語に千切れる
    /// (Studio の .app は "Flix GE Studio.app" — 必ずここに当たる)。
    pub def needsAlias(home: String): Bool =
        String.contains(substr = " ", home)
}
