#!/usr/bin/env bash
# up.sh — editor_server(バックエンド)と vite dev(フロント)を 1 コマンドで起動する。
# 使い方: [DIR=<ゲームプロジェクトdir>] [ENGINE=<flix_game_engine dir>] bash scripts/up.sh
# 既定の編集対象は ~/Desktop/flix_ge_shooting。Ctrl+C で両方まとめて止まる。
# BSD userland 前提(macOS)。GNU 拡張は使わない。
set -u
set -m  # 子を別プロセスグループにして、終了時にグループごと止める

log()  { echo "[up] $*"; }
die()  { echo "[up] エラー: $*" >&2; exit 1; }

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ENGINE="${ENGINE:-$ROOT/../flix_game_engine}"
FRONT_PORT=5174
SERVER_PORT=8787
ORIGIN="http://localhost:${FRONT_PORT}"

# 編集対象プロジェクト(省略時は shooting)。~ は shell が展開しないことがあるので自前で。
DIR="${DIR:-$HOME/Desktop/flix_ge_shooting}"
case "$DIR" in
  "~")   DIR="$HOME" ;;
  "~/"*) DIR="$HOME/${DIR#\~/}" ;;
esac
GAME_DIR="$(cd "$DIR" 2>/dev/null && pwd)" || die "DIR が見つかりません: $DIR"
[ -d "$ENGINE/editor_server" ] || die "editor_server が見つかりません: $ENGINE/editor_server (ENGINE=... で指定)"

# 既に埋まっているポートは掃除する(前回の起動が残っていても一発で上がるように)。
for p in "$SERVER_PORT" "$FRONT_PORT"; do
  pids="$(lsof -ti tcp:"$p" 2>/dev/null || true)"
  [ -n "$pids" ] && { log "ポート $p を使用中のプロセスを停止: $pids"; kill $pids 2>/dev/null || true; }
done

# 終了時に子プロセスをまとめて片付ける。
CHILDREN=()
cleanup() {
  log "停止中…"
  for pid in "${CHILDREN[@]:-}"; do
    [ -n "$pid" ] && kill "$pid" 2>/dev/null || true
  done
  # ポートに残ったものも念のため
  for p in "$SERVER_PORT" "$FRONT_PORT"; do
    pids="$(lsof -ti tcp:"$p" 2>/dev/null || true)"
    [ -n "$pids" ] && kill $pids 2>/dev/null || true
  done
  exit 0
}
trap cleanup INT TERM

log "editor_server 起動(:$SERVER_PORT / 対象: $GAME_DIR)"
# bin/flix は flix.jar を nix store から探すので、engine の devbox 環境(flake:
# github:Cj-bc/flix.nix)の中でないと「flix.jar が見つかりません」になる。
# engine 側の make editor を devbox run で包んで起動し、CORS 許可オリジンだけ前置で渡す。
if command -v devbox >/dev/null 2>&1 && [ -f "$ENGINE/devbox.json" ]; then
  ( cd "$ENGINE" \
    && EDITOR_ALLOW_ORIGINS="$ORIGIN" devbox run -- make editor DIR="$GAME_DIR" PORT="$SERVER_PORT" ) &
else
  # devbox が無い環境向けフォールバック(devbox shell 内から呼ぶなど)。
  ( cd "$ENGINE/editor_server" \
    && EDITOR_DIR="$GAME_DIR" EDITOR_PORT="$SERVER_PORT" EDITOR_ALLOW_ORIGINS="$ORIGIN" "$ENGINE/bin/flix" run ) &
fi
CHILDREN+=("$!")

# サーバが 8787 で応答するまで待つ(初回はコンパイルで時間がかかる)。
log "editor_server の起動待ち…"
for _ in $(seq 1 60); do
  curl -s -m 2 "http://localhost:${SERVER_PORT}/health" >/dev/null 2>&1 && break
  sleep 2
done

log "vite dev 起動(:$FRONT_PORT)"
( cd "$ROOT" && devbox run dev ) &
CHILDREN+=("$!")

log ""
log "  ブラウザで開く →  $ORIGIN/"
log "  (127.0.0.1 でなく localhost。CORS が localhost だけ許可)"
log "  Ctrl+C で両方停止"
log ""

wait
