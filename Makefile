# flix_ge_studio — エディタ 3 部品 (server / web / app) を 1 本のビルドにまとめる。
#
#   server/ … editor_server (Flix・常駐 HTTP バックエンド)。fatjar に固める。
#   web/    … resource editor (Elm/Vite フロント)。dist/ に固める。
#   app/    … Tauri ランチャー (Rust)。jar+web+JRE を同梱した .app にする。
#
# よく使う:
#   make jar   → server/artifact/server.jar          (Flix fatjar)
#   make web   → web/dist                            (Vite build)
#   make jre   → app/runtime/jre                     (jlink 最小 JRE)
#   make app   → app/src-tauri/target/.../Flix GE Studio.app (全部同梱)
#   make dev   → server を web/dist 配信で開発起動
#   make clean → 生成物を消す
#
# BSD userland (macOS) 前提。GNU 拡張は使わない。
.PHONY: all jar web jre app dev clean help

# Flix コンパイラは engine リポの bin/flix ラッパ経由 (nix store の flix.jar を
# 借りて手元の java で実行する)。fpkg 取得の rate limit 回避に GITHUB_TOKEN を渡す。
ENGINE      ?= /Users/abab/Desktop/flix_game_engine
FLIX        := $(ENGINE)/bin/flix
GITHUB_TOKEN ?= $(shell gh auth token 2>/dev/null)

# jlink で最小 JRE を作るための JDK ホーム (devbox の zulu-21)。
JAVA_BIN  := $(ENGINE)/.devbox/nix/profile/default/bin/java
JDK_HOME  := $(shell dirname $(shell dirname $(shell readlink -f $(JAVA_BIN))))
JLINK     := $(JDK_HOME)/bin/jlink

# 同梱 JRE に入れるモジュール (editor_server が要る最小構成 = 元 app と同じ)。
JRE_MODULES := java.base,java.datatransfer,java.xml,java.prefs,java.desktop,java.logging,java.management,java.security.sasl,java.naming,java.net.http,jdk.httpserver

ROOT      := $(shell pwd)
# flix build-fatjar は「プロジェクトのディレクトリ名.jar」を吐く。studio では
# ディレクトリ名が server なので server/artifact/server.jar になる。
JAR       := $(ROOT)/server/artifact/server.jar
WEB_DIST  := $(ROOT)/web/dist
JRE_DIR   := $(ROOT)/app/runtime/jre
RESOURCES := $(ROOT)/app/src-tauri/resources
APP_BUNDLE := $(ROOT)/app/src-tauri/target/release/bundle/macos/Flix GE Studio.app

all: app

# --- server: Flix fatjar ---
# build-fatjar は artifact/server.jar を吐く (名前はディレクトリ名由来)。
# engine の 0.7.1 fpkg は server/lib/ に同梱済み (オフラインで解決できる)。
jar:
	@echo "==> [jar] editor_server を fatjar にビルド"
	cd server && GITHUB_TOKEN="$(GITHUB_TOKEN)" $(FLIX) build-fatjar
	@test -f $(JAR) && echo "==> [jar] OK: $(JAR)" || (echo "!! jar が出力されませんでした" && exit 1)

# --- swap-jar: 動いている .app に server 変更を反映（server だけ変えたとき）---
# .app は editor_server.jar を「自分の中に」同梱して起動する。だから make jar だけでは
# 動いている .app に効かない。ここで jar を焼き直し、インストール済みの全 .app（/Applications と
# ビルド先）の同梱 jar を差し替えて再署名する。web / UI も変えたなら make app（束ね直し）。
# 反映は Studio を Cmd+Q で完全終了 → 開き直し（ウィンドウを閉じるだけだと中の java が残る）。
.PHONY: swap-jar
swap-jar: jar
	@for app in \
	  "$(APP_BUNDLE)" \
	  "/Applications/Flix GE Studio.app" \
	  "$(HOME)/Applications/Flix GE Studio.app"; do \
	    if [ -d "$$app" ]; then \
	      cp "$(JAR)" "$$app/Contents/Resources/editor_server.jar" \
	        && codesign --force --deep -s - "$$app" >/dev/null 2>&1 \
	        && echo "==> [swap-jar] 差し替え+署名: $$app" \
	        || echo "!! [swap-jar] 失敗: $$app"; \
	    fi; \
	done
	@echo "==> [swap-jar] 完了。Studio を Cmd+Q → 開き直しで反映されます（server 変更のみ）。"

# --- swap-web: 動いている .app に web(UI)変更を反映（web だけ変えたとき）---
# web/dist も .app に同梱される。make web で dist を焼き直し、全 .app の同梱 dist を
# 差し替えて再署名する。server も変えたなら swap-jar と両方、あるいは make app（束ね直し）。
.PHONY: swap-web
swap-web: web
	@for app in \
	  "$(APP_BUNDLE)" \
	  "/Applications/Flix GE Studio.app" \
	  "$(HOME)/Applications/Flix GE Studio.app"; do \
	    if [ -d "$$app" ]; then \
	      rm -rf "$$app/Contents/Resources/dist" \
	        && cp -R "$(WEB_DIST)" "$$app/Contents/Resources/dist" \
	        && codesign --force --deep -s - "$$app" >/dev/null 2>&1 \
	        && echo "==> [swap-web] 差し替え+署名: $$app" \
	        || echo "!! [swap-web] 失敗: $$app"; \
	    fi; \
	done
	@echo "==> [swap-web] 完了。Studio を Cmd+Q → 開き直しで反映されます（web 変更のみ）。"

# --- web: Vite build ---
# devbox 経由 (nodejs@22 + elm)。npm 直呼びは環境が揃わないので使わない。
web:
	@echo "==> [web] resource editor を dist にビルド"
	cd web && devbox run -- npm install --no-audit --no-fund
	cd web && devbox run -- npm run build
	@test -d $(WEB_DIST) && echo "==> [web] OK: $(WEB_DIST)" || (echo "!! dist が出力されませんでした" && exit 1)

# --- jre: jlink 最小 JRE ---
# .app に同梱する自己完結 JRE。system Java 非依存で起動できるようにする。
jre:
	@echo "==> [jre] jlink で最小 JRE を作成"
	rm -rf $(JRE_DIR)
	mkdir -p $(ROOT)/app/runtime
	$(JLINK) --add-modules $(JRE_MODULES) \
	  --no-header-files --no-man-pages --strip-debug --compress=2 \
	  --output $(JRE_DIR)
	# jlink は legal/ を読み取り専用で吐き、後段の Tauri のリソースコピーが
	# Permission denied で死ぬ。書けるようにしてから渡す。
	chmod -R u+w $(JRE_DIR)
	@test -x $(JRE_DIR)/bin/java && echo "==> [jre] OK: $(JRE_DIR)" || (echo "!! JRE 作成に失敗" && exit 1)

# --- app: 全部同梱の .app ---
# jar+web+JRE を tauri の resources/ にステージしてから cargo tauri build。
# DEVELOPER_DIR/SDKROOT はこの Mac では nix パスが刺さっており cc/ld が壊れるので外す。
app: jar web jre
	@echo "==> [app] リソースをステージ (jar / dist / jre)"
	rm -rf $(RESOURCES)
	mkdir -p $(RESOURCES)
	cp $(JAR) $(RESOURCES)/editor_server.jar
	cp -R $(WEB_DIST) $(RESOURCES)/dist
	cp -R $(JRE_DIR) $(RESOURCES)/jre
	@echo "==> [app] cargo tauri build (bundle=app)"
	cd app/src-tauri && env -u DEVELOPER_DIR -u SDKROOT cargo tauri build --bundles app
	# Tauri の署名後にリソース (jre 等) が注入されて署名が壊れるので、
	# バンドル完成後にアドホックで丸ごと署名し直し、検証まで通す。
	@echo "==> [app] アドホック再署名 + 検証"
	codesign --force --deep -s - "$(APP_BUNDLE)"
	codesign --verify --deep --strict "$(APP_BUNDLE)"
	@echo "==> [app] 完了。生成 .app: $(APP_BUNDLE)"

# --- dev: 開発起動 ---
# server を web/dist 配信で起動 (ブラウザ / .app なしでの動作確認用)。
# プロジェクト未選択で上げ、画面で選ばせる。PORT/DIR で変えられる。
PORT ?= 8787
dev: web
	@echo "==> [dev] editor_server を :$(PORT) で起動 (web=$(WEB_DIST))"
	cd server && GITHUB_TOKEN="$(GITHUB_TOKEN)" \
	  EDITOR_PORT=$(PORT) EDITOR_WEB="$(WEB_DIST)" $(FLIX) run

clean:
	# server/lib は engine 0.7.1 fpkg の同梱先。今 engine リポは fetch できない
	# (404) ので消すと再取得できずビルドが止まる。よって lib は消さない。
	rm -rf server/build server/artifact
	rm -rf web/node_modules web/dist web/elm-stuff web/.devbox
	rm -rf app/runtime app/src-tauri/resources app/src-tauri/target app/src-tauri/gen

help:
	@echo "make jar    server → server/artifact/server.jar"
	@echo "make web    web    → web/dist"
	@echo "make jre    jlink  → app/runtime/jre"
	@echo "make app    jar+web+jre 同梱の .app をビルド"
	@echo "make dev    server を web/dist 配信で開発起動"
	@echo "make clean  生成物を消す"
