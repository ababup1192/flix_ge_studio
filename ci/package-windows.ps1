# Windows 用ポータブル zip を組み立てる (GitHub Actions の windows-latest で実行)。
# 前提: server/artifact/server.jar と web/dist はビルド済み、$FlixJar は取得済み。
# やること:
#   1. jlink で Windows 用の最小 JRE を作る (モジュール一覧は Makefile から読む — 二重管理しない)
#   2. app/src-tauri/resources/ に jar / dist / jre / engine 一式をステージ
#      (engine は bash の bin/flix を入れない — server は flix.jar を直接 java で呼ぶ)
#   3. cargo build --release (tauri のビルドスクリプトが resources の存在を検査するので、この順)
#   4. FlixGEStudio/ フォルダに exe + resources を並べ、README を添えて zip 化
param(
    [Parameter(Mandatory = $true)][string]$EngineRepo,
    [Parameter(Mandatory = $true)][string]$FlixJar
)

$ErrorActionPreference = "Stop"
$root = Split-Path -Parent $PSScriptRoot

function Step($message) { Write-Host "==> $message" }

# ── 1. jlink ─────────────────────────────────────────────
$modulesLine = Select-String -Path "$root/Makefile" -Pattern '^JRE_MODULES := (.+)$'
if (-not $modulesLine) { throw "Makefile に JRE_MODULES が見つかりません" }
$modules = $modulesLine.Matches[0].Groups[1].Value
$jreDir = "$root/build/jre"
if (Test-Path $jreDir) { Remove-Item -Recurse -Force $jreDir }
New-Item -ItemType Directory -Force -Path "$root/build" | Out-Null
Step "jlink ($modules)"
& "$env:JAVA_HOME/bin/jlink" --add-modules $modules `
    --no-header-files --no-man-pages --strip-debug --compress=2 `
    --output $jreDir
if ($LASTEXITCODE -ne 0) { throw "jlink に失敗しました" }
if (-not (Test-Path "$jreDir/bin/java.exe")) { throw "jlink の出力に bin/java.exe がありません" }

# ── 2. resources のステージ ──────────────────────────────
$resources = "$root/app/src-tauri/resources"
if (Test-Path $resources) { Remove-Item -Recurse -Force $resources }
New-Item -ItemType Directory -Force -Path $resources | Out-Null

Step "resources をステージ"
Copy-Item "$root/server/artifact/server.jar" "$resources/editor_server.jar"
Copy-Item -Recurse "$root/web/dist" "$resources/dist"
Copy-Item -Recurse $jreDir "$resources/jre"

$engineStage = "$resources/engine"
# engine 一式の組み立て。運ぶ物の一覧は engine の bin/lint-rules/stage-engine.json が
# source of truth で、組み立ても最後の照合 (check-refs --bundle) も engine の bin/fge stage-engine が
# 持つ。ここが渡すのは engine の外から来る元だけ:
#   --flix-jar   … Flix コンパイラ本体
#   --maven-seed … lwjgl (Maven) の取り寄せ済みの種。new-game がゲームの lib/ へ写す。
#                  出どころに $EngineRepo/lib を使わないのは、engine リポの lib/ が
#                  生成物で空だから。これが無いと、生まれたゲームの初回ビルドが Maven へ
#                  取りに行き、回線が細い所や会社の proxy の内側では黙って何分も止まる。
# --windows は bash 前提の物 (bin/flix・reference-*.sh) を外し、代わりに bin/fge-go.exe を
# 入れる。呼ぶのは engine の bin/fge.cmd (中身は engine 自身の bin/fge-go.exe)。
# WhyNot: ここに Copy-Item を並べないのは、mac の stage-engine と 2 つの一覧を抱える
# ことになり、片方だけ痩せても誰も気づかないため (bin/lint-rules の入れ忘れで、
# 生まれたゲームの検査が全部止まった実例がある)。
Step "engine 一式をステージ (fge stage-engine)"
& "$EngineRepo/bin/fge.cmd" stage-engine --out "$engineStage" --windows `
    --flix-jar "$FlixJar" --maven-seed "$root/server/lib"
if ($LASTEXITCODE -ne 0) { throw "engine のステージに失敗しました (stage-engine)" }

# ── 3. cargo build ───────────────────────────────────────
Step "cargo build --release"
Push-Location "$root/app/src-tauri"
cargo build --release
$cargoExit = $LASTEXITCODE
Pop-Location
if ($cargoExit -ne 0) { throw "cargo build に失敗しました" }

# ── 4. ポータブルフォルダと zip ──────────────────────────
Step "ポータブルフォルダを組み立て"
$portable = "$root/build/FlixGEStudio"
if (Test-Path $portable) { Remove-Item -Recurse -Force $portable }
New-Item -ItemType Directory -Force -Path $portable | Out-Null
Copy-Item "$root/app/src-tauri/target/release/editor-app.exe" "$portable/Flix GE Studio.exe"
Copy-Item -Recurse $resources "$portable/resources"

@"
Flix GE Studio (Windows ポータブル版)

使い方:
  1. この zip をどこかのフォルダに展開する
  2. 「Flix GE Studio.exe」をダブルクリックする

画面が白紙のまま・すぐ閉じてしまうとき:
  - WebView2 ランタイムが必要です (Windows 11 は標準搭載)。無い場合は
    https://developer.microsoft.com/microsoft-edge/webview2/ の
    「エバーグリーン ブートストラッパー」をインストールしてください。
  - ログは %LOCALAPPDATA%\FlixGEStudio\logs\ に出ます
    (launcher.log = 起動の様子、server.log = 内部サーバの様子)。
"@ | Set-Content -Encoding UTF8 "$portable/README-windows.txt"

Step "zip 化"
$zip = "$root/FlixGEStudio-windows-x64.zip"
if (Test-Path $zip) { Remove-Item -Force $zip }
Compress-Archive -Path $portable -DestinationPath $zip
Step "完了: $zip"
