// soundPlayer — 音の再生(Web Audio)と、再生位置の線(プレイヘッド)。
//
// <audio> をやめて AudioBufferSourceNode にしたのは 2 つの理由から:
//   - 途中から鳴らす・範囲だけ鳴らす(start(when, offset, duration))が要る
//   - 「いま何秒目か」を AudioContext.currentTime から正確に割り出せる
//
// プレイヘッドは JS 側で動かす。Elm へ毎フレーム流すと、1 音 200ms の効果音でも
// 秒 60 回の更新が全画面の再描画になる。Elm が持つのは「始めた・止めた・
// ここから」の意図だけで、線の位置は持たない。

type PlayRequest = {
  // 鳴らす中身: URL(焼いてある WAV)か、その場で焼いた blob のどちらか
  url?: string;
  blob?: Blob;
  offset?: number;
  duration?: number;
  loop?: boolean;
  // 線を動かす照合キー(data-playhead)。同じ照合キーの線は全部動く —
  // 1 回の再生で波形とピアノロールの線が揃って走る。無ければ音だけ鳴る
  playheadId?: string;
};

let context: AudioContext | null = null;
let source: AudioBufferSourceNode | null = null;
let frame = 0;
let playing: { buffer: AudioBuffer; startedAt: number; offset: number; duration: number; loop: boolean; playheadId?: string } | null = null;

// 同じ音を鳴らし直すたびに取りに行かない(焼き直しは URL の t= で別物になる)
const buffers = new Map<string, AudioBuffer>();

function audioContext(): AudioContext {
  if (context === null) {
    context = new AudioContext();
  }
  return context;
}

async function bufferOf(req: PlayRequest): Promise<AudioBuffer | null> {
  const ctx = audioContext();
  if (req.blob !== undefined) {
    return ctx.decodeAudioData(await req.blob.arrayBuffer());
  }
  if (req.url === undefined) return null;
  const cached = buffers.get(req.url);
  if (cached !== undefined) return cached;
  const res = await fetch(req.url);
  if (!res.ok) return null;
  const decoded = await ctx.decodeAudioData(await res.arrayBuffer());
  buffers.set(req.url, decoded);
  return decoded;
}

function movePlayhead(key: string | undefined, ratio: number | null): void {
  if (key === undefined) return;
  const lines = document.querySelectorAll<HTMLElement>(`[data-playhead="${CSS.escape(key)}"]`);
  lines.forEach((el) => {
    if (ratio === null) {
      el.style.display = "none";
      return;
    }
    el.style.display = "block";
    el.style.left = `${Math.max(0, Math.min(1, ratio)) * 100}%`;
  });
}

function tick(): void {
  frame = 0;
  if (playing === null || context === null) return;
  const elapsed = context.currentTime - playing.startedAt;
  const span = playing.duration;
  const at = playing.loop && span > 0 ? playing.offset + (elapsed % span) : playing.offset + elapsed;
  if (!playing.loop && elapsed >= span) {
    // 鳴り終わり。線は最後まで行ってから消す(どこまで鳴ったかを見せる)
    movePlayhead(playing.playheadId, null);
    playing = null;
    return;
  }
  movePlayhead(playing.playheadId, at / playing.buffer.duration);
  frame = window.requestAnimationFrame(tick);
}

export function stop(): void {
  if (frame !== 0) {
    window.cancelAnimationFrame(frame);
    frame = 0;
  }
  if (source !== null) {
    try {
      source.stop();
    } catch {
      // 既に止まっている物を止めても困らない
    }
    source.disconnect();
    source = null;
  }
  if (playing !== null) {
    movePlayhead(playing.playheadId, null);
    playing = null;
  }
}

export async function play(req: PlayRequest): Promise<{ ok: boolean }> {
  const ctx = audioContext();
  // 自動再生の制限で眠っている事がある(最初のクリックで起こす)
  if (ctx.state === "suspended") await ctx.resume();
  const buffer = await bufferOf(req);
  if (buffer === null) return { ok: false };
  stop();
  const offset = Math.max(0, Math.min(req.offset ?? 0, buffer.duration));
  // duration 0 / 未指定は「最後まで」
  const rest = buffer.duration - offset;
  const duration = req.duration !== undefined && req.duration > 0 ? Math.min(req.duration, rest) : rest;
  const node = ctx.createBufferSource();
  node.buffer = buffer;
  node.loop = Boolean(req.loop);
  if (node.loop) {
    node.loopStart = offset;
    node.loopEnd = offset + duration;
  }
  node.connect(ctx.destination);
  node.start(0, offset, node.loop ? undefined : duration);
  source = node;
  playing = { buffer, startedAt: ctx.currentTime, offset, duration, loop: node.loop, playheadId: req.playheadId };
  if (frame !== 0) window.cancelAnimationFrame(frame);
  frame = window.requestAnimationFrame(tick);
  return { ok: true };
}
