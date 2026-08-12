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
New-Item -ItemType Directory -Force -Path "$engineStage/bin" | Out-Null
Copy-Item $FlixJar "$engineStage/bin/flix.jar"
# 生まれたゲームへ配る道具一式 (agents-pack/manifest.json の copy が指す物。
# 欠けると server の配布が黙って飛ばし、生まれたゲームに関所やフックが付かない)
Copy-Item "$EngineRepo/bin/lint-*.py" "$engineStage/bin/"
Copy-Item "$EngineRepo/bin/img-digest.py" "$engineStage/bin/"
Copy-Item "$EngineRepo/bin/status.py" "$engineStage/bin/"
Copy-Item "$EngineRepo/bin/checkd" "$engineStage/bin/"
Copy-Item "$EngineRepo/bin/explain-error" "$engineStage/bin/"
Copy-Item "$EngineRepo/bin/precommit.py" "$engineStage/bin/"
Copy-Item "$EngineRepo/bin/sync-agents.py" "$engineStage/bin/"
New-Item -ItemType Directory -Force -Path "$engineStage/bin/githooks" | Out-Null
Copy-Item "$EngineRepo/bin/githooks/pre-commit" "$engineStage/bin/githooks/"
New-Item -ItemType Directory -Force -Path "$engineStage/.claude/hooks" | Out-Null
Copy-Item "$EngineRepo/.claude/hooks/after-flix-edit.py" "$engineStage/.claude/hooks/"
Copy-Item "$EngineRepo/.claude/hooks/session-diet.py" "$engineStage/.claude/hooks/"
# 版刻印の出どころ (VERSION :=)。無くても engine_full/flix.toml に倒れるが、正を入れておく
Copy-Item "$EngineRepo/Makefile" "$engineStage/Makefile"
Copy-Item "$EngineRepo/flix.toml" "$engineStage/flix.toml"
New-Item -ItemType Directory -Force -Path "$engineStage/engine_full/artifact" | Out-Null
Copy-Item "$EngineRepo/engine_full/flix.toml" "$engineStage/engine_full/flix.toml"
Copy-Item "$EngineRepo/engine_full/artifact/engine_full.fpkg" "$engineStage/engine_full/artifact/engine_full.fpkg"
Copy-Item -Recurse "$EngineRepo/agents-pack" "$engineStage/agents-pack"
Copy-Item -Recurse "$EngineRepo/templates" "$engineStage/templates"
# lwjgl (Maven) の取り寄せ済みの種。new-game がゲームの lib/ へ写す。
# 出どころに $EngineRepo/lib を使わないのは、engine リポの lib/ が生成物で空だから。
# これが無いと、生まれたゲームの初回ビルドが Maven へ取りに行き、回線が細い所や
# 会社の proxy の内側では黙って何分も止まる (見張り番の打ち切りに当たる)。
New-Item -ItemType Directory -Force -Path "$engineStage/lib" | Out-Null
Copy-Item -Recurse "$root/server/lib/cache" "$engineStage/lib/cache"
Copy-Item -Recurse "$root/server/lib/external" "$engineStage/lib/external"
# テンプレ内のビルド成果物 (lib) は同梱しない — new-game が engine_full を種付けする。
Get-ChildItem -Path "$engineStage/templates" -Directory -Recurse -Filter "lib" |
    ForEach-Object { Remove-Item -Recurse -Force $_.FullName }

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
