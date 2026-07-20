#!/usr/bin/env node
// 固定スモーク: 実 DOM(実ブラウザ+実サーバ)でしか確かめられない項目だけを決め打ちで通す。
// 配線ロジックの検査は elm-test(FlowTest)の持ち場なので、ここでは数を増やさない。
//
// 前提: editor_server(8787・プロジェクト=flix_ge_shooting)と vite dev(5174)が起動済み。
// playwright は隣の flix_ge_editor の node_modules を借りる:
//   cd ../flix_ge_editor && devbox run -- node ../flix_ge_resource_editor/scripts/smoke.mjs
//
// 保存(PUT /file)は一切しない。docEdit はブラウザ内ローカル解決なのでファイルは汚れず、
// dirty になった編集は最後のリロードで捨てる。

import { createRequire } from "node:module";
import path from "node:path";

// 静的 import だとこのファイルの場所から解決されて playwright が見つからないので、
// 実行時 cwd(=flix_ge_editor)の node_modules から借りる
const require = createRequire(path.join(process.cwd(), "package.json"));
const { chromium } = require("playwright");

const URL = process.env.SMOKE_URL ?? "http://localhost:5174/";

const results = [];
let page;

async function item(name, run) {
  const t0 = Date.now();
  try {
    await run();
    results.push({ name, ok: true, ms: Date.now() - t0 });
    console.log(`PASS ${name} (${Date.now() - t0}ms)`);
  } catch (e) {
    results.push({ name, ok: false, ms: Date.now() - t0, error: String(e) });
    console.log(`FAIL ${name}: ${e}`);
  }
}

// 正本の textarea(中央エディタ)から JSON を読む。フォーム側にも multiline の
// textarea が居るので、エディタペインに限定する。docEdit の反映はローカル往復で
// 一瞬だが、ゼロではないため poll で待つ
async function docJson() {
  const text = await page.locator(".pane-editor textarea").inputValue();
  return JSON.parse(text);
}

async function waitFor(check, label, timeoutMs = 4000) {
  const t0 = Date.now();
  for (;;) {
    if (await check()) return;
    if (Date.now() - t0 > timeoutMs) throw new Error(`timeout: ${label}`);
    await page.waitForTimeout(100);
  }
}

async function main() {
  const browser = await chromium.launch();
  page = await browser.newPage({ viewport: { width: 1440, height: 900 } });
  const started = Date.now();
  await page.goto(URL);

  await item("グループ見出し(レベル)とファイル行が出る", async () => {
    await page.locator(".file-group", { hasText: "レベル" }).waitFor({ timeout: 8000 });
    await page.locator(".file-row", { hasText: "assets/level.json" }).waitFor();
  });

  await item("level.json を開くと盤面プレビューが出る", async () => {
    await page.locator(".file-row", { hasText: "assets/level.json" }).click();
    await page.locator(".preview-box img").waitFor({ timeout: 8000 });
    await page.locator(".number-box").first().waitFor();
  });

  await item("モード切替: 既定ビジュアルは textarea 無し・コードで出る・分割で両方", async () => {
    if ((await page.locator("textarea").count()) !== 0) throw new Error("ビジュアル既定で textarea が出ている");
    await page.locator(".mode-btn", { hasText: "コード" }).click();
    await page.locator("textarea").waitFor({ timeout: 4000 });
    // 以降の項目はテキスト(正本の見た目)とフォームの両方を使うので分割で進める
    await page.locator(".mode-btn", { hasText: "分割" }).click();
    await page.locator("textarea").waitFor();
    await page.locator(".number-box").first().waitFor();
  });

  await item("数値欄: フォーカスとタイプが保持され、Esc で戻る", async () => {
    const box = page.locator(".number-box").first();
    await box.click();
    await page.keyboard.press("End");
    await page.keyboard.type("7"); // "60" → "607"(未確定の打ちかけ)
    await page.waitForTimeout(400); // プレビュー往復が挟まっても消えないこと
    if ((await box.inputValue()) !== "607") throw new Error(`draft 消失: ${await box.inputValue()}`);
    const focused = await box.evaluate((el) => el === document.activeElement);
    if (!focused) throw new Error("フォーカスが外れた");
    await page.keyboard.press("Escape");
    if ((await box.inputValue()) !== "60") throw new Error("Esc で文書の値に戻らない");
  });

  await item("数値欄: ↑/Shift+↑ が docEdit として正本に入る", async () => {
    const before = (await docJson()).meta.scrollSpeed; // 60
    const box = page.locator(".number-box").first();
    await box.click();
    await page.keyboard.press("ArrowUp");
    await waitFor(async () => (await docJson()).meta.scrollSpeed > before, "↑ の反映");
    const mid = (await docJson()).meta.scrollSpeed;
    await page.keyboard.press("Shift+ArrowUp");
    await waitFor(async () => (await docJson()).meta.scrollSpeed > mid, "Shift+↑ の反映");
  });

  await item("sl-range 操作で docEdit が正本に入る", async () => {
    await page.evaluate(() => {
      const r = document.querySelector("sl-range");
      r.value = 100;
      r.dispatchEvent(new Event("sl-input"));
    });
    await waitFor(async () => (await docJson()).meta.scrollSpeed === 100, "スライダーの反映");
  });

  await item("エントリ追加→即削除(spawns・PUT はしない)", async () => {
    const before = (await docJson()).spawns.length;
    await page.locator(".form-tab", { hasText: "spawns" }).click();
    await page.locator(".crud-add").click();
    await waitFor(async () => (await docJson()).spawns.length === before + 1, "追加の反映");
    // 追加した行が選択されているので、そのまま削除(参照 0 件 → 確認なし)
    await page.locator(".crud-delete").click();
    await waitFor(async () => (await docJson()).spawns.length === before, "削除の反映");
  });

  await item("盤面の点を押すと行が選択される", async () => {
    const hotspot = page.locator(".preview-hotspot").first();
    await hotspot.hover();
    await page.mouse.down();
    await page.mouse.up();
    await page.locator(".preview-select").waitFor({ timeout: 4000 });
    await page.locator("tr.on").waitFor();
  });

  await item("盤面ドラッグで atX が変わる", async () => {
    const before = (await docJson()).spawns[0].atX;
    const hotspot = page.locator(".preview-hotspot").first();
    const boxRect = await hotspot.boundingBox();
    await page.mouse.move(boxRect.x + boxRect.width / 2, boxRect.y + boxRect.height / 2);
    await page.mouse.down();
    await page.mouse.move(boxRect.x + boxRect.width / 2 + 40, boxRect.y + boxRect.height / 2, { steps: 5 });
    await page.mouse.up();
    await waitFor(async () => (await docJson()).spawns[0].atX !== before, "ドラッグの反映");
  });

  await item("dirty のままファイル切替で確認ダイアログ(sl-dialog)が実際に開く", async () => {
    await page.locator(".file-row", { hasText: "assets/hitbox.json" }).click();
    await page.locator("sl-dialog.discard[open]").waitFor({ timeout: 4000 });
    await page.getByRole("button", { name: "やめる", exact: true }).click();
    await waitFor(
      async () => (await page.locator("sl-dialog.discard[open]").count()) === 0,
      "ダイアログが閉じる"
    );
    // 留まったこと(開いているのは level.json のまま)
    await page.locator(".topbar .path", { hasText: "assets/level.json" }).waitFor();
  });

  await item("ウィザードのステップ遷移(作成はしない)", async () => {
    await page.reload(); // ここまでの dirty を捨てる(保存はしない)
    await page.locator(".file-group", { hasText: "レベル" }).waitFor({ timeout: 8000 });
    await page.locator(".new-resource").click();
    await page.locator(".wizard-steps").waitFor();
    await page.getByRole("button", { name: "次へ →" }).click();
    await page.locator("text=フィールド定義").waitFor();
    await page.getByRole("button", { name: "やめて戻る" }).click();
    await page.locator(".pane-files").waitFor();
  });

  await item("weights: スライダー 1 本で他が連動し合計が total のまま", async () => {
    await page.locator(".file-row", { hasText: "assets/enemies.json" }).click();
    await page.locator(".entry-table").waitFor({ timeout: 8000 });
    await page.locator("td", { hasText: "slime" }).first().click();
    await page.locator(".control-weights").waitFor();
    // 正本(textarea)を読むため分割モードへ
    await page.locator(".mode-btn", { hasText: "分割" }).click();
    await page.locator(".pane-editor textarea").waitFor();
    const before = (await docJson()).enemies.slime.drops; // {claw:70, cannon:30}
    if (before.claw !== 70) throw new Error(`前提が崩れている: ${JSON.stringify(before)}`);
    await page.evaluate(() => {
      const r = document.querySelector(".control-weights .weight-row sl-range");
      r.value = 50;
      r.dispatchEvent(new Event("sl-input"));
    });
    await waitFor(async () => (await docJson()).enemies.slime.drops.claw === 50, "weights の反映");
    const drops = (await docJson()).enemies.slime.drops;
    const sum = Object.values(drops).reduce((a, b) => a + b, 0);
    if (sum !== 100) throw new Error(`合計が崩れた: ${JSON.stringify(drops)}`);
  });

  await item("肖像: slime のフォームにエンジン焼き(previewUi)のサムネが出る", async () => {
    // 直前の weights 項目で enemies.json を開いて slime を選択済み
    const box = page.locator(".entry-portrait .portrait-box").first();
    await box.waitFor({ timeout: 8000 });
    await waitFor(async () => {
      const bg = await box.evaluate((el) => el.style.backgroundImage || "");
      return bg.includes("data:image/png");
    }, "肖像の焼き上がり");
  });

  await browser.close();

  const failed = results.filter((r) => !r.ok);
  console.log(
    `\n${results.length - failed.length}/${results.length} PASS (${((Date.now() - started) / 1000).toFixed(1)}s)`
  );
  process.exit(failed.length === 0 ? 0 : 1);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
