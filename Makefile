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
# FLIX / JLINK / FLIX_JAR は ?= — CI (devbox の無い GitHub Actions) が
# `make app FLIX="java -jar flix.jar" JLINK=$$JAVA_HOME/bin/jlink …` の形で差し替える。
ENGINE      ?= /Users/abab/Desktop/flix_game_engine
FLIX        ?= $(ENGINE)/bin/flix
GITHUB_TOKEN ?= $(shell gh auth token 2>/dev/null)

# jlink で最小 JRE を作るための JDK ホーム (devbox の zulu-21)。
JAVA_BIN  ?= $(ENGINE)/.devbox/nix/profile/default/bin/java
JDK_HOME  ?= $(shell dirname $(shell dirname $(shell readlink -f $(JAVA_BIN) 2>/dev/null)))
JLINK     ?= $(JDK_HOME)/bin/jlink

# 同梱 JRE に入れるモジュール。editor_server が要る分に加えて、この JRE は
# 同梱 engine がゲームを走らせる・作るのにも使う。後ろ 2 つがその分:
#   jdk.unsupported … lwjgl (GL/音) が掴む sun.misc.Unsafe の在り処。抜くとコンパイルは
#                     通るのに起動の瞬間 ClassNotFoundException で落ちる。
#   jdk.crypto.ec   … TLS の楕円曲線。抜くと cacerts があっても handshake_failure になり、
#                     新しいゲームの初回ビルド (Maven から lwjgl を取る) が通らない。
JRE_MODULES := java.base,java.datatransfer,java.xml,java.prefs,java.desktop,java.logging,java.management,jdk.management,java.security.sasl,java.naming,java.net.http,jdk.httpserver,jdk.unsupported,jdk.crypto.ec

# .app に同梱する Flix コンパイラ。engine の devbox profile が指す nix store の実体から
# 借りる。engine の bin/flix.jar を既定にしないのは、あれが手で置いた古い版のことが
# あり、ゲームの flix.toml が要求する版と食い違うため。FLIX_JAR= で差し替えられる。
FLIX_JAR ?= $(shell real=$$(readlink -f $(ENGINE)/.devbox/nix/profile/default/bin/flix 2>/dev/null); \
              [ -n "$$real" ] && echo "$$(dirname $$(dirname $$real))/share/java/flix/flix.jar")

ROOT      := $(shell pwd)
# flix build-fatjar は「プロジェクトのディレクトリ名.jar」を吐く。studio では
# ディレクトリ名が server なので server/artifact/server.jar になる。
JAR       := $(ROOT)/server/artifact/server.jar
WEB_DIST  := $(ROOT)/web/dist
JRE_DIR   := $(ROOT)/app/runtime/jre
RESOURCES := $(ROOT)/app/src-tauri/resources
# .app の中の engine 一式。ゲームを走らせる・焼く・新しく作るのに要る物だけを写す
# (engine のソースは持たない — ゲームは flix.toml で fpkg として引く)。
ENGINE_STAGE := $(RESOURCES)/engine
APP_BUNDLE := $(ROOT)/app/src-tauri/target/release/bundle/macos/Flix GE Studio.app

all: app

# --- server: Flix fatjar ---
# build-fatjar は artifact/server.jar を吐く (名前はディレクトリ名由来)。
# engine の fpkg は server/lib/ に同梱済み (オフラインで解決できる)。engine を上げたら
# server/flix.toml の version と一緒に、取れた新しい fpkg も git に入れ直す。
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

# --- swap-engine: 動いている .app に同梱 engine の変更を反映 ---
# ラッパ (app/engine/flix) やテンプレを直したときに使う。jar / web と同じ流儀。
.PHONY: swap-engine
swap-engine: stage-engine
	@for app in \
	  "$(APP_BUNDLE)" \
	  "/Applications/Flix GE Studio.app" \
	  "$(HOME)/Applications/Flix GE Studio.app"; do \
	    if [ -d "$$app" ]; then \
	      rm -rf "$$app/Contents/Resources/engine" \
	        && cp -R "$(ENGINE_STAGE)" "$$app/Contents/Resources/engine" \
	        && codesign --force --deep -s - "$$app" >/dev/null 2>&1 \
	        && echo "==> [swap-engine] 差し替え+署名: $$app" \
	        || echo "!! [swap-engine] 失敗: $$app"; \
	    fi; \
	done
	@echo "==> [swap-engine] 完了。Studio を Cmd+Q → 開き直しで反映されます。"

# --- swap-jre: 動いている .app に同梱 JRE の変更を反映 ---
# JRE_MODULES を足したときに使う (この JRE は server だけでなくゲームも走らせる)。
.PHONY: swap-jre
swap-jre: jre
	@for app in \
	  "$(APP_BUNDLE)" \
	  "/Applications/Flix GE Studio.app" \
	  "$(HOME)/Applications/Flix GE Studio.app"; do \
	    if [ -d "$$app" ]; then \
	      rm -rf "$$app/Contents/Resources/jre" \
	        && cp -R "$(JRE_DIR)" "$$app/Contents/Resources/jre" \
	        && codesign --force --deep -s - "$$app" >/dev/null 2>&1 \
	        && echo "==> [swap-jre] 差し替え+署名: $$app" \
	        || echo "!! [swap-jre] 失敗: $$app"; \
	    fi; \
	done
	@echo "==> [swap-jre] 完了。Studio を Cmd+Q → 開き直しで反映されます。"

# --- web: Vite build ---
# 手元は devbox 経由 (nodejs@22 + elm)。CI は素の node が居るので WEB_NPM=npm で差し替える。
WEB_NPM ?= devbox run -- npm
web:
	@echo "==> [web] resource editor を dist にビルド"
	cd web && $(WEB_NPM) install --no-audit --no-fund
	cd web && $(WEB_NPM) run build
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
	$(MAKE) stage-engine
	@echo "==> [app] cargo tauri build (bundle=app)"
	cd app/src-tauri && env -u DEVELOPER_DIR -u SDKROOT cargo tauri build --bundles app
	# Tauri の署名後にリソース (jre 等) が注入されて署名が壊れるので、
	# バンドル完成後にアドホックで丸ごと署名し直し、検証まで通す。
	@echo "==> [app] アドホック再署名 + 検証"
	codesign --force --deep -s - "$(APP_BUNDLE)"
	codesign --verify --deep --strict "$(APP_BUNDLE)"
	@echo "==> [app] 完了。生成 .app: $(APP_BUNDLE)"

# --- stage-engine: .app に入れる engine 一式を揃える ---
# ゲームの Makefile が $(ENGINE)/bin/flix を呼ぶ形はそのままに、その ENGINE の実体を
# .app の中へ移す。ここに要るのは「走らせる手」だけ:
#   bin/flix      … 同梱 JRE と隣の flix.jar だけを見るラッパ (app/engine/flix)
#   bin/flix.jar  … Flix コンパイラ本体
#   Makefile      … 新しいゲームを作る (new-game) のに使う
#   flix.toml     … その new-game が engine の版を読むのに要る
#   templates/    … その複製元。ジャンルカードのサムネ (reference/title.png) もここから読む
#   engine_full/  … 生まれたゲームの lib/ に置かれる engine の fpkg と toml
#   agents-pack/  … 生まれたゲームに配る AGENTS.md と skills の元
#   lib/          … lwjgl (Maven) の取り寄せ済みの種。new-game がゲームの lib/ へ写す
# .app に焼き固める engine_full.fpkg がソースより古くないか見る。
# WhyNot: これは engine 側の生成物なので「在れば良い」で通してしまいがちだが、.app は
# 配る物で、ここで固めた fpkg が新しいゲームの lib/ にそのまま座る。版名は bump で進む
# ので中身が古くても新しい版に見え、Flix は「版名が同じなら取り直さない」から、
# 使う人の側では engine にも Release にも在る def が「Undefined name」になって終わる。
# engine のソースが手元に無いとき (CI の一部) は judge できないので黙って通す。
ENGINE_FULL_FPKG := $(ENGINE)/engine_full/artifact/engine_full.fpkg
ENGINE_SRC_PKGS  := engine render_gl engine_world engine_tools
.PHONY: check-engine-full
check-engine-full:
	@test -f "$(ENGINE_FULL_FPKG)" \
	  || (echo "!! engine_full.fpkg がありません: $(ENGINE_FULL_FPKG) (engine で make sync-engine-full)" && exit 1)
	@dirs=""; for p in $(ENGINE_SRC_PKGS); do [ -d "$(ENGINE)/$$p/src" ] && dirs="$$dirs $(ENGINE)/$$p/src"; done; \
	 if [ -z "$$dirs" ]; then \
	   echo "==> [engine] engine のソースが無いので鮮度は見ません"; \
	 else \
	   newer=$$(find $$dirs -name '*.flix' -newer "$(ENGINE_FULL_FPKG)" 2>/dev/null | head -5); \
	   if [ -n "$$newer" ]; then \
	     echo "!! engine_full.fpkg が engine のソースより古いです。engine で make sync-engine-full してください"; \
	     echo "$$newer" | sed 's/^/  新しい: /'; exit 1; \
	   fi; \
	 fi

.PHONY: stage-engine
stage-engine:
	@echo "==> [engine] 同梱 engine をステージ"
	@test -n "$(FLIX_JAR)" -a -f "$(FLIX_JAR)" \
	  || (echo "!! flix.jar が見つかりません (FLIX_JAR= で場所を指定してください)" && exit 1)
	@$(MAKE) --no-print-directory check-engine-full
	rm -rf $(ENGINE_STAGE)
	mkdir -p $(ENGINE_STAGE)/bin $(ENGINE_STAGE)/engine_full/artifact
	cp $(FLIX_JAR) $(ENGINE_STAGE)/bin/flix.jar
	cp $(ROOT)/app/engine/flix $(ENGINE_STAGE)/bin/flix
	chmod +x $(ENGINE_STAGE)/bin/flix
	# 生まれたゲームへ配る道具 (new-game の sync-agents が bin/ と .claude/hooks/ から写す物。
	# 一覧は engine の Makefile sync-agents の cp と対 — あちらだけ増えると同梱 new-game が転ぶ)
	cp $(ENGINE)/bin/lint-*.py $(ENGINE)/bin/img-digest.py $(ENGINE)/bin/status.py $(ENGINE)/bin/checkd $(ENGINE_STAGE)/bin/
	cp $(ENGINE)/bin/gen-api-digest.py $(ENGINE)/bin/reference-update.sh \
	   $(ENGINE)/bin/reference-check.sh $(ENGINE)/bin/explain-error $(ENGINE_STAGE)/bin/
	cp $(ENGINE)/bin/precommit.py $(ENGINE)/bin/sync-agents.py $(ENGINE_STAGE)/bin/
	mkdir -p $(ENGINE_STAGE)/bin/githooks
	cp $(ENGINE)/bin/githooks/pre-commit $(ENGINE_STAGE)/bin/githooks/
	# ゲームの make api / status / スキルの参照先 (docs)。同梱漏れは末尾の check-refs が止める
	mkdir -p $(ENGINE_STAGE)/docs/api-digest
	cp $(ENGINE)/docs/api-digest.md $(ENGINE)/docs/module-index.md \
	   $(ENGINE)/docs/engine-module-index.md $(ENGINE)/docs/doc-conventions.md \
	   $(ENGINE)/docs/glossary.md $(ENGINE)/docs/shader-doc.md $(ENGINE)/docs/checkd.md \
	   $(ENGINE_STAGE)/docs/
	cp $(ENGINE)/docs/api-digest/*.md $(ENGINE_STAGE)/docs/api-digest/
	mkdir -p $(ENGINE_STAGE)/.claude/hooks
	cp $(ENGINE)/.claude/hooks/after-flix-edit.py $(ENGINE)/.claude/hooks/after-flix-work.py \
	   $(ENGINE)/.claude/hooks/after-flix-touch.py $(ENGINE)/.claude/hooks/session-diet.py \
	   $(ENGINE_STAGE)/.claude/hooks/
	cp $(ENGINE)/Makefile $(ENGINE_STAGE)/Makefile
	cp $(ENGINE)/flix.toml $(ENGINE_STAGE)/flix.toml
	cp $(ENGINE)/engine_full/flix.toml $(ENGINE_STAGE)/engine_full/flix.toml
	cp -R $(ENGINE)/agents-pack $(ENGINE_STAGE)/agents-pack
	cp -R $(ENGINE)/templates $(ENGINE_STAGE)/templates
	# テンプレが抱えている lib/ は複製時に new-game が捨てて engine_full から置き直すので
	# 運ばない。中身が fpkg だけで対の toml が無く、Tauri のリソース収集が転ぶのもある。
	# build/ も運ばない。コンパイル成果物なので複製先で作り直せるが、放っておくと
	# テンプレ 1 本で GB 級になり .app がその分ふくらむ。
	# .devbox/ も運ばない。中身が nix store を指す symlink で、そのマシンにしか無い先を
	# 指す。運ぶと codesign --verify が "invalid destination for symbolic link" で転ぶ
	# (Tauri のリソース収集は symlink を落とすので make app では出ず、swap-engine の
	# cp -R だけで出る — 気づきにくい)。
	find $(ENGINE_STAGE)/templates -type d \( -name lib -o -name build -o -name .devbox \) -exec rm -rf {} +
	cp $(ENGINE)/engine_full/artifact/engine_full.fpkg $(ENGINE_STAGE)/engine_full/artifact/
	# 種の出どころに $(ENGINE)/lib を使わないのは、engine リポの lib/ が生成物 (gitignore) で
	# 空のことがあるから。server 自身のビルドで必ず落ちる同じ lwjgl を使い回す。
	# これが無いと、生まれたゲームの初回ビルドが Maven へ取りに行き、回線が細い所や
	# 会社の proxy の内側では黙って何分も止まる (見張り番の打ち切りに当たる)。
	@if [ -d "$(ROOT)/server/lib/cache" -a -d "$(ROOT)/server/lib/external" ]; then \
	  mkdir -p $(ENGINE_STAGE)/lib; \
	  cp -R $(ROOT)/server/lib/cache $(ENGINE_STAGE)/lib/cache; \
	  cp -R $(ROOT)/server/lib/external $(ENGINE_STAGE)/lib/external; \
	  echo "==> [engine] Maven の種を同梱 ($$(du -sh $(ENGINE_STAGE)/lib | cut -f1))"; \
	else \
	  echo "!! [engine] server/lib が無いので Maven の種を同梱できません (make jar の後にどうぞ)"; \
	fi
	# nix store から写した flix.jar は読み取り専用で来る。後段の Tauri のリソース収集が
	# Permission denied で死ぬので、書けるようにしてから渡す (jre と同じ手当て)。
	chmod -R u+w $(ENGINE_STAGE)
	# 同梱の必須一覧 (BUNDLE_REQUIRED) と照合。どちらかの cp リストが痩せたらここで止まる
	python3 $(ENGINE)/bin/check-refs.py --bundle $(ENGINE_STAGE)
	@echo "==> [engine] OK: $(ENGINE_STAGE) ($$(du -sh $(ENGINE_STAGE) | cut -f1))"

# --- test: server のテスト + 配布の一致検査 ---
# 変更した部品だけ回す流儀のうち server ぶんの入口。web は web/ で elm-test / vitest。
.PHONY: test
test:
	@echo "==> [test] server (flix test)"
	cd server && GITHUB_TOKEN="$(GITHUB_TOKEN)" $(FLIX) test
	$(MAKE) --no-print-directory check-agents-parity

# --- check-agents-parity: AGENTS 配布の二重実装がずれていないかの機械検査 ---
# 配布物一覧は engine の agents-pack/manifest.json に一本化してあるが、AGENTS.md の
# 組み立てだけは engine (bin/sync-agents.py) と server (NewGame.flix) の二重実装が残る。
# 同じ engine から一時フォルダ A (make 版) / B (jar 版) へ配って diff -r で突き合わせる。
# AGENTS.local.md の有/無の 2 ケース (ゲーム固有部のつなぎ方が本文の形を変えるため)。
PARITY_JAVA := $(shell [ -x "$(JAVA_BIN)" ] && echo "$(JAVA_BIN)" || echo java)
.PHONY: check-agents-parity
check-agents-parity: jar
	@ver=$$(sed -n 's/^VERSION[[:space:]]*:=[[:space:]]*\([^[:space:]]*\).*/\1/p' "$(ENGINE)/Makefile" | head -1); \
	test -n "$$ver" || { echo "!! engine の VERSION が読めません: $(ENGINE)/Makefile"; exit 1; }; \
	tmp=$$(mktemp -d); status=0; \
	for c in plain local; do \
	  a="$$tmp/$$c/A"; b="$$tmp/$$c/B"; mkdir -p "$$a" "$$b"; \
	  if [ "$$c" = "local" ]; then \
	    printf '## この画面の画風\n\nparity 検査用のゲーム固有部。\n' > "$$a/AGENTS.local.md"; \
	    cp "$$a/AGENTS.local.md" "$$b/AGENTS.local.md"; \
	  fi; \
	  python3 "$(ENGINE)/bin/sync-agents.py" --game "$$a" --version "$$ver" >/dev/null; \
	  "$(PARITY_JAVA)" -jar "$(JAR)" --sync-agents "$(ENGINE)" "$$b" >/dev/null; \
	  if diff -r "$$a" "$$b" >/dev/null 2>&1; then \
	    echo "==> [parity] $$c: 一致"; \
	  else \
	    echo "!! [parity] $$c: make 版と Studio 版の配布結果がずれています"; \
	    diff -r "$$a" "$$b" | head -40; status=1; \
	  fi; \
	done; \
	rm -rf "$$tmp"; exit $$status

# --- dev: 開発起動 ---
# server を web/dist 配信で起動 (ブラウザ / .app なしでの動作確認用)。
# プロジェクト未選択で上げ、画面で選ばせる。PORT/DIR で変えられる。
PORT ?= 8787
dev: web
	@echo "==> [dev] editor_server を :$(PORT) で起動 (web=$(WEB_DIST))"
	cd server && GITHUB_TOKEN="$(GITHUB_TOKEN)" \
	  EDITOR_PORT=$(PORT) EDITOR_WEB="$(WEB_DIST)" $(FLIX) run

clean:
	# server/lib は engine fpkg (今は 0.24.1) の同梱先。消すとオフライン環境で
	# 再取得できずビルドが止まるので、lib は消さない。
	rm -rf server/build server/artifact
	rm -rf web/node_modules web/dist web/elm-stuff web/.devbox
	rm -rf app/runtime app/src-tauri/resources app/src-tauri/target app/src-tauri/gen

help:
	@echo "make jar    server → server/artifact/server.jar"
	@echo "make web    web    → web/dist"
	@echo "make jre    jlink  → app/runtime/jre"
	@echo "make app    jar+web+jre 同梱の .app をビルド"
	@echo "make dev    server を web/dist 配信で開発起動"
	@echo "make test   server の flix test + check-agents-parity"
	@echo "make check-agents-parity  AGENTS 配布の make 版 / jar 版の一致検査"
	@echo "make clean  生成物を消す"
