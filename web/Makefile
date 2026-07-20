# flix_ge_resource_editor — リソースエディタ(フロント)。
# editor_server(バックエンド)と vite dev を 1 コマンドで起動する。

.PHONY: up test build help

# editor_server + vite dev を 1 発で起動。編集対象は DIR で変えられる。
#   make up
#   make up DIR=~/Desktop/other_project
#   make up ENGINE=/path/to/flix_game_engine
up:
	@DIR="$(DIR)" ENGINE="$(ENGINE)" bash scripts/up.sh

test:
	@devbox run test
	@devbox run -- npx vitest run

build:
	@devbox run build

help:
	@echo "make up      editor_server + リソースエディタを起動 (http://localhost:5174/)"
	@echo "make up DIR=~/path   別プロジェクトを編集対象にして起動"
	@echo "make test    elm-test + vitest"
	@echo "make build   vite build → dist/"
