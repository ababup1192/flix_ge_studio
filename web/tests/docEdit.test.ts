// docEdit は「触ったキーの周りだけ」を書き換える最小テキスト編集。
// キー順・インデント・"//" 注釈が保たれることを、結果テキスト全文で固定する。

import { describe, expect, test } from "vitest";
import { applyDocAppend, applyDocEdit, applyDocEdits, applyDocRemove } from "../src/js/docEdit";

const doc = [
  "{",
  '  "//": "手書きの注釈",',
  '  "meta": { "scrollSpeed": 60 },',
  '  "spawns": [',
  '    { "atX": 200, "kind": "popcorn" }',
  "  ]",
  "}",
  "",
].join("\n");

describe("applyDocEdit — 正本テキストへの最小編集", () => {
  test("float の書き換え: 値のトークンだけ変わり、キー順・注釈・整形が保たれる", () => {
    expect(applyDocEdit(doc, ["meta", "scrollSpeed"], 72.5, false)).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "meta": { "scrollSpeed": 72.5 },',
        '  "spawns": [',
        '    { "atX": 200, "kind": "popcorn" }',
        "  ]",
        "}",
        "",
      ].join("\n"),
    );
  });

  test("トップ直下のキー(kind value の書き戻し先)も値のトークンだけ変わる", () => {
    const light = ['{', '  "note": "洞窟のカンテラ",', '  "darkness": 0.85,', '  "rim": { "alpha": 0.2 }', "}", ""].join("\n");
    expect(applyDocEdit(light, ["darkness"], 0.7, false)).toBe(
      ['{', '  "note": "洞窟のカンテラ",', '  "darkness": 0.7,', '  "rim": { "alpha": 0.2 }', "}", ""].join("\n"),
    );
  });

  test("int フィールドは小数点なしで書く(350.0 → 350、端数は四捨五入)", () => {
    const expected = [
      "{",
      '  "//": "手書きの注釈",',
      '  "meta": { "scrollSpeed": 60 },',
      '  "spawns": [',
      '    { "atX": 350, "kind": "popcorn" }',
      "  ]",
      "}",
      "",
    ].join("\n");
    expect(applyDocEdit(doc, ["spawns", 0, "atX"], 350.0, true)).toBe(expected);
    expect(applyDocEdit(doc, ["spawns", 0, "atX"], 349.6, true)).toBe(expected);
  });

  test("無いキーへの書き込みはオブジェクト末尾に追加される", () => {
    expect(applyDocEdit(doc, ["spawns", 0, "y"], 120, true)).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "meta": { "scrollSpeed": 60 },',
        '  "spawns": [',
        "    {",
        '      "atX": 200,',
        '      "kind": "popcorn",',
        '      "y": 120',
        "    }",
        "  ]",
        "}",
        "",
      ].join("\n"),
    );
  });
});

// id 改名(キー改名+参照書き換え)が頼る挙動を固定する。要は
// 「キーのトークンだけが変わる」— 1 行書きエントリの整形・行位置・注釈は動かない。
describe("applyDocEdits — renameKey と複数編集のバッチ", () => {
  const level = [
    "{",
    '  "//": "手書きの注釈",',
    '  "routes": {',
    '    "slow":  { "type": "straight", "speed": 60 },',
    '    "wavy":  { "type": "sine", "speed": 85 }',
    "  },",
    '  "spawns": [',
    '    { "atX": 200, "route": "wavy" },',
    '    { "atX": 400, "route": "slow" }',
    "  ]",
    "}",
    "",
  ].join("\n");

  test("renameKey: ネストしたキーだけ変わり、1 行書きの整形・注釈・キー順が保たれる", () => {
    expect(applyDocEdits(level, [{ op: "renameKey", path: ["routes", "wavy"], newKey: "wave" }])).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "routes": {',
        '    "slow":  { "type": "straight", "speed": 60 },',
        '    "wave":  { "type": "sine", "speed": 85 }',
        "  },",
        '  "spawns": [',
        '    { "atX": 200, "route": "wavy" },',
        '    { "atX": 400, "route": "slow" }',
        "  ]",
        "}",
        "",
      ].join("\n"),
    );
  });

  test("バッチ: キー改名+参照の書き換えが 1 回の適用で全部入る", () => {
    expect(
      applyDocEdits(level, [
        { op: "renameKey", path: ["routes", "wavy"], newKey: "wave" },
        { op: "set", path: ["spawns", 0, "route"], value: "wave", intField: false },
      ]),
    ).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "routes": {',
        '    "slow":  { "type": "straight", "speed": 60 },',
        '    "wave":  { "type": "sine", "speed": 85 }',
        "  },",
        '  "spawns": [',
        '    { "atX": 200, "route": "wave" },',
        '    { "atX": 400, "route": "slow" }',
        "  ]",
        "}",
        "",
      ].join("\n"),
    );
  });

  test("renameKey: 無いパスは例外(中途半端な文書を返さない)", () => {
    expect(() => applyDocEdits(level, [{ op: "renameKey", path: ["routes", "ghost"], newKey: "x" }])).toThrow(
      "キーが見つかりません",
    );
  });
});

// ウィザードの project.json 追記が頼る挙動を固定する。要は
// 「追記なのに既存行が diff に出ない」— 1 行書きの直前エントリもそのまま。
describe("applyDocAppend — 配列末尾への追記", () => {
  test("editor.resources への追記: 1 行書きの既存エントリ・キー順・注釈が保たれる", () => {
    const project = [
      "{",
      '  "//": "手書きの注釈",',
      '  "title": "Flix Shooting",',
      '  "editor": {',
      '    "resources": [',
      '      { "id": "level", "pattern": "assets/level.json" }',
      "    ]",
      "  },",
      '  "fonts": []',
      "}",
      "",
    ].join("\n");
    expect(
      applyDocAppend(project, ["editor", "resources"], {
        id: "enemies",
        pattern: "assets/enemies.json",
        title: "敵図鑑",
      }),
    ).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "title": "Flix Shooting",',
        '  "editor": {',
        '    "resources": [',
        '      { "id": "level", "pattern": "assets/level.json" },',
        "      {",
        '        "id": "enemies",',
        '        "pattern": "assets/enemies.json",',
        '        "title": "敵図鑑"',
        "      }",
        "    ]",
        "  },",
        '  "fonts": []',
        "}",
        "",
      ].join("\n"),
    );
  });

  test("空配列へは 1 段深い字下げで最初のエントリが入る", () => {
    const project = ["{", '  "editor": {', '    "resources": []', "  }", "}", ""].join("\n");
    expect(applyDocAppend(project, ["editor", "resources"], { id: "enemies", pattern: "assets/enemies.json" })).toBe(
      [
        "{",
        '  "editor": {',
        '    "resources": [',
        "      {",
        '        "id": "enemies",',
        '        "pattern": "assets/enemies.json"',
        "      }",
        "    ]",
        "  }",
        "}",
        "",
      ].join("\n"),
    );
  });

  test('"editor" キーが無い project.json では editor.resources ごと作られる', () => {
    const project = ["{", '  "title": "Flix Shooting"', "}", ""].join("\n");
    expect(applyDocAppend(project, ["editor", "resources"], { id: "enemies", pattern: "assets/enemies.json" })).toBe(
      [
        "{",
        '  "title": "Flix Shooting",',
        '  "editor": {',
        '    "resources": [',
        "      {",
        '        "id": "enemies",',
        '        "pattern": "assets/enemies.json"',
        "      }",
        "    ]",
        "  }",
        "}",
        "",
      ].join("\n"),
    );
  });
});

describe("applyDocRemove — キー/要素の削除(エントリ削除の足)", () => {
  const catalog = [
    "{",
    '  "//": "手書きの注釈",',
    '  "routes": {',
    '    "slow": { "type": "straight", "speed": 60 },',
    '    "fast": { "type": "straight", "speed": 100 }',
    "  },",
    '  "spawns": [',
    '    { "atX": 200, "route": "slow" },',
    '    { "atX": 400, "route": "fast" }',
    "  ]",
    "}",
    "",
  ].join("\n");

  test("catalog のキー削除: その行だけ消え、他の行・注釈・整形は動かない", () => {
    expect(applyDocRemove(catalog, ["routes", "slow"])).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "routes": {',
        '    "fast": { "type": "straight", "speed": 100 }',
        "  },",
        '  "spawns": [',
        '    { "atX": 200, "route": "slow" },',
        '    { "atX": 400, "route": "fast" }',
        "  ]",
        "}",
        "",
      ].join("\n"),
    );
  });

  test("list の要素削除: 先頭を消すと後続が残る(区切りカンマも辻褄が合う)", () => {
    expect(applyDocRemove(catalog, ["spawns", 0])).toBe(
      [
        "{",
        '  "//": "手書きの注釈",',
        '  "routes": {',
        '    "slow": { "type": "straight", "speed": 60 },',
        '    "fast": { "type": "straight", "speed": 100 }',
        "  },",
        '  "spawns": [',
        '    { "atX": 400, "route": "fast" }',
        "  ]",
        "}",
        "",
      ].join("\n"),
    );
  });

  test("最後の 1 件を消すと空の入れ物が残る(キーごとは消えない)", () => {
    const single = ["{", '  "spawns": [', '    { "atX": 200 }', "  ]", "}", ""].join("\n");
    expect(applyDocRemove(single, ["spawns", 0])).toBe(["{", '  "spawns": []', "}", ""].join("\n"));
  });
});
