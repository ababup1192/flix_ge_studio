# 注釈チケットのパネル(旧・sketch roundtrip 案2改)

状態: 実装済み(2026-08-12)。旧・段階2/3(盤面グリッドの往復・差分再送)は見送り。

## なぜこの形になったか

当初案は「(a) 盤面 Doc をグリッドに戻して差分再送 + (b) 注釈チケットの一覧」の 2 本立て
だったが、壁打ちで (a) を見送った:

- 塗って直せる変更なら MapEditor で直接直せば終わり(即時・確実)。AI に投げる理由が無い。
- 塗って直せない変更(ルール・生成)は、マスの差分では意図が伝わらず結局言葉が要る。
  その「言葉 + 現場写真の運び屋」は (b) が既に担う。

残した (b) = 遊んでいて感じた「ここ変だ」を、スクショ + 場所 + 一言のまま AI に運ぶパネル。
直接編集では代替できず、他エンジンにも無い伝達手段。

## 使う engine 側の前提(実装済み・触らない)

ゲーム中に一時停止して矩形で囲うと、`debug/annotations/<日時_題名_フレーム>/` に
README.md(自動タイトル + 「## コメント」空欄)・highlighted.png・screenshot.png・
annotation.json・world.json が書き出される(`render_gl/src/LwjglLayer.flix` の
`saveAnnotation` / `readmeText`)。

## 作った物

- **server/src/Annotations.flix** — README の節の読み書き(純関数)+ HTTP の口 4 本。
  - `GET /annotations/list` … `{tickets:[{id,title,comment,hasShot}]}`(新しい順、archive/ は除く)
  - `GET /annotations/shot?id=…` … highlighted.png(無ければ screenshot.png)。id は URL デコード
  - `POST /annotations/comment {id,comment}` … README の「## コメント」欄を差し替え
  - `POST /annotations/archive {id}` … `debug/annotations/archive/` へ移動(消さない)
  - ルートは Editor.flix の dispatch(withProject 包み)。テストは server/test/TestAnnotations.flix
- **web/src/Tickets.elm** — ホーム(viewHome)の「🎫 注釈チケット」パネル。
  - コメント入力(プレースホルダ「ここに不具合の内容を記述」、blur で保存)
  - 「📋 注釈チケットを報告」= `buildTicketPrompt`(純関数)で依頼文を作りコピー。一言が空なら押せない
  - 「🗄 アーカイブ」。チケット 0 件ならパネルごと出さない。404 サーバは fail-open で畳む
  - 取得はホームを開いた足の 1 回(`gotoTab HomeTab` + 起動時)。ポーリング無し
  - テストは web/tests/TicketsTest.elm。realApi.ts に kind 3 つ追加

## 守った線引き

- Studio は world.json の中身を解釈しない(パスと言葉を運ぶだけ)。
- チケットは素の Markdown + PNG のまま(テキストエディタでも読める)。
- 依頼文は「直し先(コードか Doc か)は AI が選ぶ」と書き、意図の伝達だけを頼む。

## 後から足した: ラフのバージョン(2026-08-12)

上の「見送り」を覆した物ではない。機械が絵の差分を判定する仕組みは入れないまま、
**ラフにバージョンを残す** 配管だけを足した(人と合意済みのバージョン 3)。

- 保存先が `draft/sketch/<名前>.sketch.json` から
  **`draft/sketch/<名前>/v<番号>.json`** へ変わった。保存のたびに番号が 1 つ増え、
  前のラフは残る(上書きしない)。`draft/` 配下なので親ディレクトリはサーバが作る
  (`FileIndex.allowsParentCreation`)。
- **server/src/Sketches.flix** — `GET /sketch/list` …
  `{ok, sketches:[{name, versions:[3,2,1]}]}`。中身(塗り・凡例)は解釈せず置き場の名前だけ読む。
  純関数は `versionOf` / `nextVersion` / `isSafeName`。テストは server/test/TestSketches.flix
- SketchPad は書きっぱなし(ラフを読み戻す口が無い)なので、次の番号はこの一覧からしか取れない。
  一覧を配るのは **アトリエだけ** — 新しいゲームを産む画面のラフは、まだ無いプロジェクトへ
  書くので必ず v1 から。
- 依頼文に挟まる「原本: 」行が、その絵が収まったバージョンを指す(= 生成された物に由来が残る)。
  保存した直後は絵を触るまで同じ場所を指し続ける。

## 今後

- 案1(one-click genesis)が入ったら、「注釈チケットを報告」のコピーを
  「Studio に任せる」(claude ヘッドレス起動)に育てる。
- 生成された物の側から「どのラフのどのバージョンから来たか」を覚え書きに吐くのは engine 側の
  変更なので、着手前に人へ相談する(まだ入っていない)。
