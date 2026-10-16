#!/usr/bin/env bash
# 同梱する engine の版を決め、engine リポにそのタグがあるか確かめる。
#   使い方: ci/resolve-engine-version.sh [上書きの版]
#   版の出どころ: 引数（workflow_dispatch の入力）> リポジトリ直下の engine-version
#   出力: GITHUB_OUTPUT へ version=0.35.31 と ref=v0.35.31（無ければ標準出力）
#   環境: ENGINE_TOKEN … engine リポを読むトークン（private のため）
# 版が書かれていない・形が違う・engine にタグが無いときは理由を出して止まる。
# engine の main の最新へ黙って倒さない（ゲームの master とずれると make と Studio を
# 行き来するたびに全部コンパイルし直しになるため）。
set -euo pipefail

ENGINE_REPO="ababup1192/flix_game_engine"
FILE="$(dirname "$0")/../engine-version"

fail() { echo "::error title=engine の版::$1" >&2; exit 1; }

src="workflow_dispatch の入力 engine_version"
v="${1:-}"
if [ -z "${v}" ]; then
  src="engine-version"
  [ -f "${FILE}" ] || fail "engine-version がありません。リポジトリ直下に同梱する engine の版（例 0.35.31）を 1 行で書いてください（README の「リリースの手順」）。"
  v="$(grep -v '^[[:space:]]*#' "${FILE}" | tr -d '[:space:]')"
fi
v="${v#v}"
[ -n "${v}" ] || fail "${src} に版が書かれていません。ゲームの master の flix.toml にある flix_game_engine の version（例 0.35.31）を書いてください。"
[[ "${v}" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || fail "${src} の版「${v}」の形が違います。0.35.31 のように数字 3 つで書いてください。"

url="https://github.com/${ENGINE_REPO}.git"
[ -n "${ENGINE_TOKEN:-}" ] && url="https://x-access-token:${ENGINE_TOKEN}@github.com/${ENGINE_REPO}.git"
tags="$(git ls-remote --tags --refs "${url}" 'v*' | sed 's#.*refs/tags/##' | sort -V)" \
  || fail "engine リポ ${ENGINE_REPO} のタグを読めませんでした（ENGINE_REPO_TOKEN を確かめてください）。"
latest="$(printf '%s\n' "$tags" | tail -1)"
if ! printf '%s\n' "$tags" | grep -qx "v${v}"; then
  fail "engine に v${v} のタグがありません（${src}）。最近のタグ: $(printf '%s\n' "$tags" | tail -5 | tr '\n' ' ')"
fi
if [ "v${v}" != "${latest}" ]; then
  echo "::notice title=engine の版::v${v} を同梱します（engine の最新は ${latest}）。"
else
  echo "::notice title=engine の版::v${v} を同梱します（engine の最新と同じ）。"
fi

out="${GITHUB_OUTPUT:-/dev/stdout}"
{ echo "version=${v}"; echo "ref=v${v}"; } >> "${out}"
