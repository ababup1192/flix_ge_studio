// Flix GE Studio ランチャー (Tauri v2)。
//
// 役割はランチャーに徹する (ロジックは editor_server 側):
//   1. 空きポートを選ぶ
//   2. editor_server の実行可能 jar を java で子プロセス起動 (EDITOR_PORT/EDITOR_WEB を渡す)
//   3. /health を 200 が返るまでポーリング
//   4. WebView をその URL に向ける
//   5. アプリ終了時に子プロセスを道連れに kill (java の残留 = ポート占有を防ぐ)
//      — Unix はプロセスグループ + シグナル、Windows は Job Object (win.rs)。
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[cfg(windows)]
mod win;

use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicI32, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

use tauri::{Manager, RunEvent, WebviewUrl, WebviewWindowBuilder};

// フロントへ返す「見えたゲームプロセスの生情報」1 件。
// 意味づけ (どれがどのプロジェクトか) はしない = cwd をそのまま渡す。
#[derive(serde::Serialize)]
struct RunningGame {
    pid: u32,
    cwd: String,
}

// 走っているゲームの列挙。ゲームの目印は LWJGL/GLFW 必須の起動フラグ
// -XstartOnFirstThread (editor_server は headless なので持たない)。
// プロセス列挙は sysinfo。ただし macOS では sysinfo が他プロセスの引数/cwd を
// 権限不足で読めない (task_for_pid が要る) ので、引数は ps、cwd は lsof に
// フォールバックする。どちらも権限不要で全プロセスを読める外部コマンド。
#[cfg(unix)]
#[tauri::command]
fn list_running_games() -> Vec<RunningGame> {
    use sysinfo::{ProcessesToUpdate, System};
    let mut sys = System::new();
    sys.refresh_processes(ProcessesToUpdate::All, true);

    let mut games = Vec::new();
    for (pid, proc_) in sys.processes() {
        let pid = pid.as_u32();
        // sysinfo の引数が空なら ps で取り直す (macOS の権限壁の回避)。
        let cmdline = {
            let joined = proc_
                .cmd()
                .iter()
                .map(|s| s.to_string_lossy())
                .collect::<Vec<_>>()
                .join(" ");
            if joined.is_empty() {
                cmdline_via_ps(pid)
            } else {
                joined
            }
        };
        // ゲームは実ウィンドウを開くので headless を付けない。editor_server は
        // bin/flix 経由だと -XstartOnFirstThread も付くが常に headless なので、
        // headless の有無でゲームと確実に切り分けられる。
        let is_game = cmdline.contains("-XstartOnFirstThread")
            && !cmdline.contains("-Djava.awt.headless=true");
        if !is_game {
            continue;
        }
        // cwd も同様に sysinfo → lsof の順で拾う。
        let cwd = proc_
            .cwd()
            .map(|p| p.to_string_lossy().into_owned())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| cwd_via_lsof(pid));
        games.push(RunningGame { pid, cwd });
    }
    games
}

// Windows には -XstartOnFirstThread の目印が無い (macOS 専用フラグ)。
// 走っているゲームは server の書き残し (GET /running-games) が知っているので、
// ランチャー側は空を返すだけでよい。
#[cfg(not(unix))]
#[tauri::command]
fn list_running_games() -> Vec<RunningGame> {
    Vec::new()
}

// sysinfo が引数を取れない時の保険。ps は権限不要で全引数を返す。
#[cfg(unix)]
fn cmdline_via_ps(pid: u32) -> String {
    Command::new("ps")
        .args(["-p", &pid.to_string(), "-o", "command="])
        .env_remove("DEVELOPER_DIR")
        .env_remove("SDKROOT")
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .map(|s| s.trim().to_string())
        .unwrap_or_default()
}

// sysinfo が cwd を取れない時の保険。lsof の cwd 行の最終列がそれ。
#[cfg(unix)]
fn cwd_via_lsof(pid: u32) -> String {
    Command::new("lsof")
        .args(["-a", "-d", "cwd", "-p", &pid.to_string(), "-Fn"])
        .env_remove("DEVELOPER_DIR")
        .env_remove("SDKROOT")
        .output()
        .ok()
        .and_then(|out| {
            String::from_utf8(out.stdout).ok().and_then(|text| {
                // -Fn 出力は "n<path>" 行。cwd の実体パスをその接頭辞から取る。
                text.lines()
                    .find_map(|line| line.strip_prefix('n').map(str::to_string))
            })
        })
        .unwrap_or_default()
}

// 起動した java のプロセスグループ ID (= リーダーの pid)。
// シグナルハンドラからも触れるよう atomic のグローバルに置く (0 = 未起動)。
static SERVER_PGID: AtomicI32 = AtomicI32::new(0);

// ランチャー自身のログ。stderr へ出しつつ、Windows ではファイルにも残す —
// windows_subsystem = "windows" だとコンソールが無く stderr が消えるので、
// 「白紙のまま何も起きない」の理由がログでしか追えない。
fn log_line(message: &str) {
    eprintln!("{message}");
    #[cfg(windows)]
    {
        if let Some(dir) = log_dir() {
            let _ = std::fs::create_dir_all(&dir);
            if let Ok(mut file) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(dir.join("launcher.log"))
            {
                use std::io::Write;
                let _ = writeln!(file, "{message}");
            }
        }
    }
}

// ログ置き場。zip の展開先は読み取り専用のことがあるので、ユーザーの領域に置く。
#[cfg(windows)]
fn log_dir() -> Option<PathBuf> {
    std::env::var_os("LOCALAPPDATA").map(|base| PathBuf::from(base).join("FlixGEStudio").join("logs"))
}

// java の実行ファイル名 (Windows だけ拡張子つき)。
fn java_name() -> &'static str {
    if cfg!(windows) {
        "java.exe"
    } else {
        "java"
    }
}

// 実行に要る在処 (java 実行ファイル・jar・配信 dist・同梱 engine)。
// engine は固めた配布物のときだけ Some — 固めていない開発起動では server 側の
// 既定 ($HOME/Desktop/flix_game_engine) に任せる。
struct Assets {
    java: PathBuf,
    jar: PathBuf,
    web: PathBuf,
    engine: Option<PathBuf>,
}

// 固めた配布物のリソース置き場を実行時に解決する。
//   macOS:   <App>.app/Contents/MacOS/<bin> の隣 → Contents/Resources/
//   Windows: <展開先>/<bin>.exe の隣 → resources/   (ポータブル zip の自前レイアウト)
// AppHandle はまだ無い (health を待ってから WebView を開く設計) ので自前で辿る。
fn resolve_assets() -> Assets {
    let bundled = std::env::current_exe().ok().and_then(|exe| {
        let resources = if cfg!(windows) {
            exe.parent()?.join("resources")
        } else {
            exe.parent()?.parent()?.join("Resources")
        };
        let java = resources.join("jre").join("bin").join(java_name());
        let jar = resources.join("editor_server.jar");
        let web = resources.join("dist");
        let engine = resources.join("engine");
        // 同梱 JRE が居れば「固めた配布物として起動された」と判断する。
        if java.exists() && jar.exists() {
            Some(Assets {
                java,
                jar,
                web,
                // コンパイラ jar まで揃っている時だけ同梱 engine と認める (欠けたまま
                // 渡すと server が中身の無い ENGINE を掴んで転ぶ)。
                engine: engine
                    .join("bin")
                    .join("flix.jar")
                    .exists()
                    .then_some(engine),
            })
        } else {
            None
        }
    });

    bundled.unwrap_or_else(dev_assets)
}

// 固めていない状態 (cargo run / cargo tauri dev) で使うパス。
// 実行ファイルの位置から studio ルートを辿り、studio 内の成果物を指す。
//   jar = <studio>/server/artifact/editor_server.jar   (make jar)
//   web = <studio>/web/dist                            (make web)
//   java = <studio>/app/runtime/jre/bin/java → STUDIO_JAVA → JAVA_HOME → PATH の java
fn dev_assets() -> Assets {
    // <studio>/app/src-tauri/target/{debug,release}/editor-app → 5 つ上が <studio>
    let studio = std::env::current_exe()
        .ok()
        .and_then(|exe| Some(exe.parent()?.parent()?.parent()?.parent()?.parent()?.to_path_buf()));

    let (jar, web, studio_java) = match studio {
        Some(root) => (
            root.join("server").join("artifact").join("editor_server.jar"),
            root.join("web").join("dist"),
            Some(root.join("app").join("runtime").join("jre").join("bin").join(java_name())),
        ),
        None => (
            PathBuf::from("server/artifact/editor_server.jar"),
            PathBuf::from("web/dist"),
            None,
        ),
    };

    let java = studio_java
        .filter(|p| p.exists())
        .or_else(|| std::env::var_os("STUDIO_JAVA").map(PathBuf::from))
        .or_else(|| {
            std::env::var_os("JAVA_HOME").map(|home| PathBuf::from(home).join("bin").join(java_name()))
        })
        .unwrap_or_else(|| PathBuf::from(java_name()));

    Assets {
        java,
        jar,
        web,
        engine: None,
    }
}

struct ServerProc(Mutex<Option<Child>>);

#[cfg(unix)]
extern "C" {
    fn kill(pid: i32, sig: i32) -> i32;
}

// java 子プロセスをそのプロセスグループごと kill する。
// java を process_group(0) でグループリーダーにしてあるので、pid をそのまま
// 負の pgid として kill に渡せば、java が孫プロセスを作っていても巻き込んで倒せる。
#[cfg(unix)]
fn kill_process_group(child: &mut Child) {
    let pid = child.id() as i32;
    unsafe {
        // -pid = プロセスグループ全体。まず SIGTERM、取りこぼしに SIGKILL。
        kill(-pid, 15); // SIGTERM
    }
    std::thread::sleep(Duration::from_millis(300));
    unsafe {
        kill(-pid, 9); // SIGKILL
    }
    let _ = child.wait();
    SERVER_PGID.store(0, Ordering::SeqCst);
}

// Windows は Job Object が子孫ごと倒す (win.rs)。
#[cfg(not(unix))]
fn kill_process_group(child: &mut Child) {
    #[cfg(windows)]
    win::terminate_all();
    let _ = child.kill();
    let _ = child.wait();
}

// ターミナルから launcher が SIGTERM/SIGINT/SIGHUP で倒される経路は Tauri の
// RunEvent::Exit を通らない。その時でも java を道連れにするための保険。
// シグナルハンドラ内で許されるのは async-signal-safe な処理だけなので、
// グローバルに保存した pgid へ kill(2) を撃つだけにして、そのまま既定動作で死ぬ。
#[cfg(unix)]
extern "C" fn on_term_signal(_sig: i32) {
    extern "C" {
        fn _exit(code: i32) -> !;
    }
    let pgid = SERVER_PGID.load(Ordering::SeqCst);
    if pgid > 0 {
        unsafe {
            kill(-pgid, 9); // SIGKILL: 確実に道連れ
        }
    }
    // GUI の run loop が自前のシグナル処理を持っていて再 raise が効かないことがある。
    // _exit は async-signal-safe で無条件に自分を終わらせるので、これで launcher も確実に落ちる。
    unsafe {
        _exit(0);
    }
}

#[cfg(unix)]
fn install_signal_handlers() {
    extern "C" {
        fn signal(sig: i32, handler: extern "C" fn(i32)) -> usize;
    }
    // 15=SIGTERM 2=SIGINT 1=SIGHUP
    for sig in [15, 2, 1] {
        unsafe {
            signal(sig, on_term_signal);
        }
    }
}

// Windows に Unix シグナルは無い。強制終了の道連れは Job Object (KILL_ON_JOB_CLOSE) が担う。
#[cfg(not(unix))]
fn install_signal_handlers() {}

// TcpListener に 0 番ポートでバインドして OS が割り当てた空きポートを得る。
// listener はここで drop するので、直後に java が同じポートを掴める。
fn pick_free_port() -> std::io::Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    Ok(listener.local_addr()?.port())
}

fn spawn_editor_server(
    java: &Path,
    jar: &Path,
    port: u16,
    web: &str,
    engine: Option<&Path>,
) -> std::io::Result<Child> {
    let mut cmd = Command::new(java);
    cmd.args([
        "-Djava.awt.headless=true",
        "-Dstdout.encoding=UTF-8",
        "-Dstderr.encoding=UTF-8",
        "-jar",
        &jar.to_string_lossy(),
    ])
    .env("EDITOR_PORT", port.to_string())
    .env("EDITOR_WEB", web)
    // server がゲームを走らせる・焼くのに使う java (同梱 JRE)。
    .env("EDITOR_JAVA", java)
    // EDITOR_DIR はあえて渡さない = プロジェクト未選択で起動し画面で選ばせる。
    // この Mac のシェルには存在しない nix パスの DEVELOPER_DIR/SDKROOT が刺さっており、
    // 子プロセスに漏らすと悪さをするので外す。
    .env_remove("DEVELOPER_DIR")
    .env_remove("SDKROOT");

    // ゲームを走らせる・焼く・新しく作るのに使う engine の在処。同梱していれば
    // 配布物の中を指す = 手元に engine リポが無くても Studio だけで完結する。
    if let Some(dir) = engine {
        cmd.env("EDITOR_ENGINE", dir);
    }

    // server の口は普段は捨てる。Windows だけファイルに残す — コンソールが無く、
    // 起動失敗の理由 (web UI: 無効 など) がそこにしか出ないため。
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x0800_0000;
        cmd.creation_flags(CREATE_NO_WINDOW);
        match server_log_files() {
            Some((out, err)) => {
                cmd.stdout(out).stderr(err);
            }
            None => {
                cmd.stdout(Stdio::null()).stderr(Stdio::null());
            }
        }
    }
    #[cfg(not(windows))]
    {
        cmd.stdout(Stdio::null()).stderr(Stdio::null());
    }

    // 子を新しいプロセスグループのリーダーにする → 終了時にグループごと kill できる。
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        cmd.process_group(0);
    }

    let child = cmd.spawn()?;
    // 子孫 (server が起こすゲーム) ごと Job に入れて、道連れの網を張る。
    #[cfg(windows)]
    win::assign(&child);
    Ok(child)
}

// server の stdout/stderr の書き先 (Windows のみ)。同じファイルへ 2 本とも流す。
#[cfg(windows)]
fn server_log_files() -> Option<(std::fs::File, std::fs::File)> {
    let dir = log_dir()?;
    std::fs::create_dir_all(&dir).ok()?;
    let file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(dir.join("server.log"))
        .ok()?;
    let clone = file.try_clone().ok()?;
    Some((file, clone))
}

// /health が 200 を返すまで待つ。java の cold start ぶんを見て ~20 秒でタイムアウト。
// 素の TcpStream で 1 往復する — curl は Windows で `-o /dev/null` が書けず使えない。
fn wait_for_health(port: u16, timeout: Duration) -> bool {
    let deadline = Instant::now() + timeout;
    while Instant::now() < deadline {
        if health_ok(port) {
            return true;
        }
        std::thread::sleep(Duration::from_millis(300));
    }
    false
}

fn health_ok(port: u16) -> bool {
    use std::io::{Read, Write};
    let addr = std::net::SocketAddr::from(([127, 0, 0, 1], port));
    let Ok(mut stream) = std::net::TcpStream::connect_timeout(&addr, Duration::from_millis(800))
    else {
        return false;
    };
    let _ = stream.set_read_timeout(Some(Duration::from_secs(2)));
    let request =
        format!("GET /health HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nConnection: close\r\n\r\n");
    if stream.write_all(request.as_bytes()).is_err() {
        return false;
    }
    let mut buf = [0u8; 32];
    let n = stream.read(&mut buf).unwrap_or(0);
    // curl -f と同じ判定: 2xx/3xx を「起きた」とみなす。
    let head = String::from_utf8_lossy(&buf[..n]).to_string();
    head.starts_with("HTTP/1.1 2") || head.starts_with("HTTP/1.1 3")
}

fn main() {
    #[cfg(windows)]
    {
        win::init_job();
        // WebView2 が無い Windows では窓を開けない。白紙で黙るのではなく案内して終わる。
        if tauri::webview_version().is_err() {
            win::message_box(
                "Flix GE Studio",
                "WebView2 ランタイムが見つかりません。\n\
                 https://developer.microsoft.com/microsoft-edge/webview2/ から\n\
                 「エバーグリーン ブートストラッパー」をインストールして、もう一度開いてください。",
            );
            std::process::exit(1);
        }
    }

    let assets = resolve_assets();
    if !assets.jar.exists() {
        log_line(&format!(
            "[editor_app] editor_server の jar が見つかりません: {}\n\
             先にビルドしてください: make jar (flix_ge_studio ルートで)",
            assets.jar.display()
        ));
        std::process::exit(1);
    }
    log_line(&format!(
        "[editor_app] java={} jar={} web={} engine={}",
        assets.java.display(),
        assets.jar.display(),
        assets.web.display(),
        assets
            .engine
            .as_deref()
            .map(|p| p.display().to_string())
            .unwrap_or_else(|| "(同梱なし)".into())
    ));

    let port = pick_free_port().expect("空きポートの取得に失敗");
    log_line(&format!("[editor_app] 選択ポート = {port}"));

    let web = assets.web.to_string_lossy().into_owned();
    let child = match spawn_editor_server(
        &assets.java,
        &assets.jar,
        port,
        &web,
        assets.engine.as_deref(),
    ) {
        Ok(c) => {
            log_line(&format!(
                "[editor_app] editor_server 起動 pid={} port={port}",
                c.id()
            ));
            // process_group(0) で子は自分自身を pgid とするリーダーになる。
            SERVER_PGID.store(c.id() as i32, Ordering::SeqCst);
            install_signal_handlers();
            c
        }
        Err(e) => {
            log_line(&format!("[editor_app] editor_server の起動に失敗: {e}"));
            #[cfg(windows)]
            win::message_box(
                "Flix GE Studio",
                "内部サーバを起動できませんでした。\n\
                 %LOCALAPPDATA%\\FlixGEStudio\\logs\\launcher.log を確認してください。",
            );
            std::process::exit(1);
        }
    };

    log_line("[editor_app] /health を待機中 (最大 20 秒)...");
    if !wait_for_health(port, Duration::from_secs(20)) {
        log_line("[editor_app] editor_server が起動しませんでした。子プロセスを片付けて終了します。");
        let mut child = child;
        kill_process_group(&mut child);
        #[cfg(windows)]
        win::message_box(
            "Flix GE Studio",
            "内部サーバが応答しませんでした。\n\
             %LOCALAPPDATA%\\FlixGEStudio\\logs\\server.log を確認してください。",
        );
        std::process::exit(1);
    }
    log_line("[editor_app] /health OK。WebView を開きます。");

    let url = format!("http://127.0.0.1:{port}/");
    let external: tauri::Url = url.parse().expect("URL の生成に失敗");

    let app = tauri::Builder::default()
        .plugin(tauri_plugin_dialog::init())
        .manage(ServerProc(Mutex::new(Some(child))))
        .invoke_handler(tauri::generate_handler![list_running_games])
        .setup(move |app| {
            WebviewWindowBuilder::new(app, "main", WebviewUrl::External(external))
                .title("Flix GE Studio")
                .inner_size(1200.0, 820.0)
                .build()?;
            Ok(())
        })
        .build(tauri::generate_context!())
        .expect("Tauri アプリのビルドに失敗");

    app.run(|app_handle, event| {
        if let RunEvent::Exit = event {
            if let Some(mut c) = app_handle.state::<ServerProc>().0.lock().unwrap().take() {
                kill_process_group(&mut c);
                log_line("[editor_app] editor_server (プロセスグループ) を kill しました。");
            }
        }
    });
}
