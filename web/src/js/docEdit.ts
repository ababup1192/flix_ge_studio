// 正本(中央 textarea のテキスト)への最小テキスト編集。
// jsonc-parser の modify/applyEdits は触ったキーの周りしか書き換えないので、
// キー順・インデント・"//" 注釈キーが保たれる(全体を stringify し直さない)。

import { applyEdits, findNodeAtLocation, getNodeValue, modify, parseTree } from "jsonc-parser";

export type PathSeg = string | number;

// int フィールドの整形規則はここ 1 箇所に集める:
// 小数点なしで書く(1 は 1 のまま、1.0 にしない)。スライダ由来の端数は四捨五入。
function normalizeInt(value: unknown): unknown {
  return typeof value === "number" ? Math.round(value) : value;
}

// 編集する前に、その場所に何が書いてあったかを読む(元に戻すの逆操作を組む材料)。
// found=false は「そこには何も無かった」— 旧値が null だった場合と区別する必要が
// あるので、値だけでなく在り無しも返す。
export function valueAt(text: string, path: PathSeg[]): { found: boolean; value: unknown } {
  const root = parseTree(text);
  const node = root === undefined ? undefined : findNodeAtLocation(root, path);
  if (node === undefined) {
    return { found: false, value: null };
  }
  return { found: true, value: getNodeValue(node) };
}

// path が指す場所の文字の範囲(キーごと)。フォームで触っている欄が、正本の
// どの行なのかを画面に示すために使う。見つからなければ null。
export function rangeAt(text: string, path: PathSeg[]): { from: number; to: number } | null {
  const root = parseTree(text);
  const node = root === undefined ? undefined : findNodeAtLocation(root, path);
  if (node === undefined) {
    return null;
  }
  // 値だけでなく "キー": 値 の一組を指す(行として読める範囲にする)
  const item = node.parent?.type === "property" ? node.parent : node;
  return { from: item.offset, to: item.offset + item.length };
}

export function applyDocEdit(
  text: string,
  path: PathSeg[],
  value: unknown,
  intField: boolean,
): string {
  const v = intField ? normalizeInt(value) : value;
  const edits = modify(text, path, v, {
    formattingOptions: { insertSpaces: true, tabSize: 2 },
  });
  return applyEdits(text, edits);
}

// path が指すキー(オブジェクト)/要素(配列)を消す。
// WhyNot: modify の undefined 削除に任せない — 削除後の再整形が 1 行書きの
// 隣接エントリまで複数行へ展開してしまい、消しただけなのに他の行が diff に
// 出るため。トークンの範囲(区切りカンマ込み)だけを自前で切り取る。
export function applyDocRemove(text: string, path: PathSeg[]): string {
  const root = parseTree(text);
  const valueNode = root === undefined ? undefined : findNodeAtLocation(root, path);
  if (valueNode === undefined) {
    // 消す対象が既に無い(二重クリック等)なら文書を触らない
    return text;
  }
  const item = valueNode.parent?.type === "property" ? valueNode.parent : valueNode;
  const container = item.parent;
  const siblings = container?.children;
  if (container === undefined || siblings === undefined) {
    throw new Error(`applyDocRemove: パス ${path.join("/")} は消せる場所ではありません`);
  }
  const cut = (from: number, to: number): string => text.slice(0, from) + text.slice(to);
  const i = siblings.indexOf(item);
  if (siblings.length === 1) {
    // 最後の 1 件: 中身だけ空にして入れ物({} / [])は残す
    return cut(container.offset + 1, container.offset + container.length - 1);
  }
  if (i === siblings.length - 1) {
    // 末尾: 直前の要素の終わりから消す(先行カンマごと)
    const prev = siblings[i - 1];
    return cut(prev.offset + prev.length, item.offset + item.length);
  }
  // 途中: 次の要素の頭まで消す(後続カンマと字下げは次の要素が引き継ぐ)
  return cut(item.offset, siblings[i + 1].offset);
}

// 1 バッチぶんの編集。set は値の書き換え(applyDocEdit と同じ)、renameKey は
// オブジェクトのキー自体の改名。
export type DocEditOp =
  | { op: "set"; path: PathSeg[]; value: unknown; intField: boolean }
  | { op: "renameKey"; path: PathSeg[]; newKey: string };

// 複数編集を上から順に 1 本のテキストへ当てる(呼び側は結果を 1 回だけ受け取る)。
// 途中で 1 件でも当たらなければ例外 = 全体が失敗 — 参照だけ書き換わった/キーだけ
// 変わった中途半端な文書を返さない。
export function applyDocEdits(text: string, edits: DocEditOp[]): string {
  return edits.reduce(
    (t, e) => (e.op === "set" ? applyDocEdit(t, e.path, e.value, e.intField) : renameKey(t, e.path, e.newKey)),
    text,
  );
}

// キーの改名。modify に任せない — modify はキーの付け替え(削除+追加)になり、
// エントリの行位置・1 行書きの整形が動いて diff が汚れるため。キー文字列の
// トークンだけを最小置換する。
function renameKey(text: string, path: PathSeg[], newKey: string): string {
  const root = parseTree(text);
  const valueNode = root === undefined ? undefined : findNodeAtLocation(root, path);
  const propNode = valueNode?.parent;
  const keyNode = propNode?.type === "property" ? propNode.children?.[0] : undefined;
  if (keyNode === undefined) {
    throw new Error(`renameKey: パス ${path.join("/")} のキーが見つかりません`);
  }
  return text.slice(0, keyNode.offset) + JSON.stringify(newKey) + text.slice(keyNode.offset + keyNode.length);
}

// path が指す配列の末尾へ 1 エントリ足す(project.json の "editor"."resources" 等)。
// WhyNot: modify の添字 -1 挿入に任せない — 挿入後の再整形(keepLines なし)が
// 1 行書きの直前エントリまで複数行へ展開してしまい、追記なのに既存行が diff に
// 出るため。挿入テキストは既存の字下げに合わせてここで自前に組む。
export function applyDocAppend(text: string, path: PathSeg[], value: unknown): string {
  const root = parseTree(text);
  const arrayNode = root === undefined ? undefined : findNodeAtLocation(root, path);
  if (arrayNode === undefined || arrayNode.type !== "array" || arrayNode.children === undefined) {
    // 配列(親キーごと)がまだ無いときは modify で丸ごと作る — 化けて困る既存エントリが無い
    const edits = modify(text, [...path, -1], value, {
      formattingOptions: { insertSpaces: true, tabSize: 2 },
    });
    return applyEdits(text, edits);
  }
  const indentOf = (offset: number): string => {
    const lineStart = text.lastIndexOf("\n", offset - 1) + 1;
    return /^[ \t]*/.exec(text.slice(lineStart, offset))?.[0] ?? "";
  };
  const splice = (offset: number, length: number, content: string): string =>
    text.slice(0, offset) + content + text.slice(offset + length);
  const last = arrayNode.children[arrayNode.children.length - 1];
  if (last === undefined) {
    // 空配列: [ と ] の間へ、配列の行より 1 段深い字下げで入れる
    const arrayIndent = indentOf(arrayNode.offset);
    const entryIndent = arrayIndent + "  ";
    const pretty = JSON.stringify(value, null, 2).split("\n").join("\n" + entryIndent);
    return splice(arrayNode.offset + 1, arrayNode.length - 2, `\n${entryIndent}${pretty}\n${arrayIndent}`);
  }
  const indent = indentOf(last.offset);
  const pretty = JSON.stringify(value, null, 2).split("\n").join("\n" + indent);
  return splice(last.offset + last.length, 0, `,\n${indent}${pretty}`);
}
