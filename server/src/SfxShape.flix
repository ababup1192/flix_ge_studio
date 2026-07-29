// SfxShape — 焼き上がった効果音 WAV を「絵にできる形」へ measure して配る窓口。
// エディタ画面は波形と帯ごとの音量を描くだけなので、生の標本ではなく
// 間引いた包絡 (peaks) と帯×時刻の升目 (bands) を返す。
// 読めない・WAV でない・中身が空はどれも None (呼び手が 404 にする)。
mod SfxShape {
    import java.io.{File => JFile}
    import java.lang.Math
    import java.nio.file.Files

    use Util.Json.Json
    use Util.Json.Json.{JObject, JArray, JNumber}

    /// 音を配ってよいディレクトリの白名前簿。ゲームが焼いた物 (assets/sfx) と
    /// 検証用に吐いた物 (debug/sounds) だけ。
    pub def soundDirs(): List[String] = "assets/sfx" :: "debug/sounds" :: "golden/sfx" :: Nil

    /// 波形を間引く升目の数 (横)。
    def cols(): Int32 = 160

    /// 帯の数 (縦)。log で等間隔に並べる。
    def rows(): Int32 = 20

    /// 帯の下端・上端 (Hz)。エディタ画面の縦軸の目盛りと同じ。
    pub def bandLo(): Float64 = 180.0
    pub def bandHi(): Float64 = 9000.0

    /// 白名前簿を順に見て、最初に見つかったファイルのバイト列。
    /// 置き場を指しての取り出し(聴き比べ用)。許すのは soundDirs の中だけ。
    pub def findBytesIn(dir: String, subdir: String, name: String): Option[Array[Int8, Static]] \ IO =
        if (not Gallery.isSafeName(name) or not String.endsWith(suffix = ".wav", name)
                or not List.memberOf(subdir, soundDirs()))
            None
        else {
            let file = new JFile("${dir}/${subdir}/${name}");
            if (file.isFile()) Some(Files.readAllBytes(file.toPath())) else None
        }

    pub def findBytes(dir: String, name: String): Option[Array[Int8, Static]] \ IO =
        if (not Gallery.isSafeName(name) or not String.endsWith(suffix = ".wav", name)) None
        else soundDirs() |> List.findMap(sub -> {
            let file = new JFile("${dir}/${sub}/${name}");
            if (file.isFile()) Some(Files.readAllBytes(file.toPath())) else None
        })

    /// GET /sfx/shape の本文。
    /// ms = 鳴っている長さ、peaks = 波形の包絡 (0〜1 が cols 個)、
    /// bands = 帯ごとの音量 (0〜1 が rows×cols)、peak = 元の標本の最大値 (0〜1)。
    pub def shapeJson(dir: String, name: String): Option[Json] \ IO =
        forM (
            bytes <- findBytes(dir, name);
            wav   <- decode(bytes)
        ) yield {
            // 切り詰めない。無音を落として詰めると、音が短くなるたびに時間軸まで縮み、
            // 「同じ場所を掴んでいるのに別の時間になる」— エディタで ms を指すには、
            // 軸がつまみで動かないことのほうが大事。長さは speed でしか変わらない。
            let env = envelope(wav);
            JObject(Map#{
                "name"       => Json.JString(name),
                "sampleRate" => JNumber(JsonCodec.intToBd(wav#sampleRate)),
                "ms"         => num(Int32.toFloat64(wav#count) * 1000.0
                    / Int32.toFloat64(wav#sampleRate)),
                "peak"       => num(env#peak),
                "bandLo"     => num(bandLo()),
                "bandHi"     => num(bandHi()),
                "peaks"      => JArray(env#peaks |> List.map(num) |> List.toVector),
                "bands"      => JArray(bandRows(wav)
                    |> List.map(row -> JArray(row |> List.map(num) |> List.toVector))
                    |> List.toVector)
            })
        }

    def num(v: Float64): Json =
        JNumber(JsonCodec.floatToBd(round3(v)))

    /// 小数 3 桁で丸める。JSON が短くなるうえ、絵の見た目は変わらない。
    def round3(v: Float64): Float64 =
        Float64.round(v * 1000.0) / 1000.0

    // ── WAV を解く ────────────────────────────────────────

    /// 解いた音。samples は左チャンネル (単声に畳む) の -1.0〜1.0。
    type alias Wav = { sampleRate = Int32, count = Int32, samples = Array[Float64, Static] }

    def u8(bytes: Array[Int8, Static], i: Int32): Int32 \ IO =
        Int32.bitwiseAnd(Int8.toInt32(Array.get(i, bytes)), 0xFF)

    def u16(bytes: Array[Int8, Static], i: Int32): Int32 \ IO =
        u8(bytes, i) + u8(bytes, i + 1) * 256

    def u32(bytes: Array[Int8, Static], i: Int32): Int32 \ IO =
        u16(bytes, i) + u16(bytes, i + 2) * 65536

    def s16(bytes: Array[Int8, Static], i: Int32): Int32 \ IO =
        let v = u16(bytes, i);
        if (v >= 32768) v - 65536 else v

    // 塊の 4 文字の目印。文字を数へ直す関数が無いので、文字コードのまま置く。
    def riffTag(): List[Int32] = 0x52 :: 0x49 :: 0x46 :: 0x46 :: Nil   // "RIFF"
    def waveTag(): List[Int32] = 0x57 :: 0x41 :: 0x56 :: 0x45 :: Nil   // "WAVE"
    def fmtTag():  List[Int32] = 0x66 :: 0x6d :: 0x74 :: 0x20 :: Nil   // "fmt "
    def dataTag(): List[Int32] = 0x64 :: 0x61 :: 0x74 :: 0x61 :: Nil   // "data"

    /// 位置 i の 4 バイトが目印と一致するか。
    def tagIs(bytes: Array[Int8, Static], i: Int32, want: List[Int32]): Bool \ IO =
        want |> List.zipWithIndex |> List.forAll(pair -> {
            let (k, code) = pair;
            u8(bytes, i + k) == code
        })

    /// RIFF の塊を先頭から順に見て fmt と data を拾う。
    /// 16bit PCM 以外・data が無いは None (エディタは絵を出さないだけ)。
    def decode(bytes: Array[Int8, Static]): Option[Wav] \ IO =
        let len = Array.length(bytes);
        if (len < 44 or not tagIs(bytes, 0, riffTag()) or not tagIs(bytes, 8, waveTag())) None
        else {
            let found = walk(bytes, len, 12, { rate = 0, channels = 1, bits = 16, at = -1, size = 0 });
            if (found#at < 0 or found#rate <= 0 or found#bits != 16) None
            else {
                let ch = Int32.max(1, found#channels);
                let frames = found#size / (2 * ch);
                if (frames <= 0) None
                else {
                    let samples = Array.empty(Static, frames);
                    List.range(0, frames) |> List.forEach(i ->
                        Array.put(Int32.toFloat64(s16(bytes, found#at + i * 2 * ch)) / 32768.0, i, samples));
                    Some({ sampleRate = found#rate, count = frames, samples = samples })
                }
            }
        }

    def walk(bytes: Array[Int8, Static], len: Int32, p: Int32,
             acc: { rate = Int32, channels = Int32, bits = Int32, at = Int32, size = Int32 }):
            { rate = Int32, channels = Int32, bits = Int32, at = Int32, size = Int32 } \ IO =
        if (p + 8 > len) acc
        else {
            let size = u32(bytes, p + 4);
            let next =
                if (tagIs(bytes, p, fmtTag()) and p + 24 <= len)
                    { channels = u16(bytes, p + 10), rate = u32(bytes, p + 12),
                      bits = u16(bytes, p + 22) | acc }
                else if (tagIs(bytes, p, dataTag()))
                    { at = p + 8, size = Int32.min(size, len - (p + 8)) | acc }
                else acc;
            if (size <= 0) next else walk(bytes, len, p + 8 + size + Int32.remainder(size, 2), next)
        }

    // ── 絵の材料 ──────────────────────────────────────────

    /// 波形の包絡。升目ごとの最大値を、全体の最大値で割って 0〜1 にする。
    /// 最大値そのものも返す — 音量を下げた音は絵も低くなってほしいため。
    def envelope(wav: Wav): { peak = Float64, peaks = List[Float64] } \ IO =
        let n = wav#count;
        let peak = List.range(0, n)
            |> List.foldLeft((acc, i) -> Float64.max(acc, Float64.abs(Array.get(i, wav#samples))), 0.0);
        // 0〜1 に正規化して返す。実際の大きさは peak として別に返す
        // (絵の高さと「どれだけ大きい音か」を混ぜない)。
        let scale = if (peak <= 0.000001) 0.0 else 1.0 / peak;
        let peaks = List.range(0, cols()) |> List.map(c -> {
            let lo = c * n / cols();
            let hi = Int32.max(lo + 1, (c + 1) * n / cols());
            let m = List.range(lo, Int32.min(hi, n))
                |> List.foldLeft((acc, i) -> Float64.max(acc, Float64.abs(Array.get(i, wav#samples))), 0.0);
            m * scale
        });
        { peak = peak, peaks = peaks }

    /// 帯ごとの音量。log で等間隔に並べた帯を 1 本ずつ通して、升目ごとの実効値を取る。
    /// 並びは上が高い帯 (画面の縦軸と同じ向き)。
    def bandRows(wav: Wav): List[List[Float64]] \ IO =
        let sr = Int32.toFloat64(wav#sampleRate);
        let raw = List.range(0, rows()) |> List.map(r -> {
            let t = Int32.toFloat64(rows() - 1 - r) / Int32.toFloat64(rows() - 1);
            let f = bandLo() * Math.pow(bandHi() / bandLo(), t);
            bandEnergy(wav, sr, f)
        });
        let top = raw |> List.foldLeft((acc, row) ->
            row |> List.foldLeft((a, v) -> Float64.max(a, v), acc), 0.0);
        if (top <= 0.000001) raw
        else raw |> List.map(row -> row |> List.map(v -> v / top))

    /// 中心 f の帯だけ通して、升目ごとの実効値 (二乗平均平方根) を返す。
    /// 山の鋭さ (Q) は帯の数で決まる幅に合わせてある — 鋭すぎると隙間ができ、
    /// 鈍すぎるとどの帯も同じ絵になる。
    def bandEnergy(wav: Wav, sr: Float64, f: Float64): List[Float64] \ IO =
        let q = 3.0;
        let w0 = 2.0 * 3.141592653589793 * Float64.min(f, sr * 0.45) / sr;
        let al = Math.sin(w0) / (2.0 * q);
        let a0 = 1.0 + al;
        let b0 = al / a0;
        let b2 = 0.0 - b0;
        let a1 = (0.0 - 2.0 * Math.cos(w0)) / a0;
        let a2 = (1.0 - al) / a0;

        let n = wav#count;
        let acc = Array.empty(Static, cols());
        let cnt = Array.empty(Static, cols());
        let state = Array.empty(Static, 4);   // x1 x2 y1 y2

        List.range(0, n) |> List.forEach(i -> {
            let x0 = Array.get(i, wav#samples);
            let x1 = Array.get(0, state);
            let x2 = Array.get(1, state);
            let y1 = Array.get(2, state);
            let y2 = Array.get(3, state);
            let y0 = b0 * x0 + b2 * x2 - a1 * y1 - a2 * y2;
            Array.put(x0, 0, state); Array.put(x1, 1, state);
            Array.put(y0, 2, state); Array.put(y1, 3, state);
            let c = Int32.min(cols() - 1, i * cols() / Int32.max(1, n));
            Array.put(Array.get(c, acc) + y0 * y0, c, acc);
            Array.put(Array.get(c, cnt) + 1.0, c, cnt)
        });

        List.range(0, cols()) |> List.map(c -> {
            let k = Array.get(c, cnt);
            if (k <= 0.0) 0.0 else Math.sqrt(Array.get(c, acc) / k)
        })
}
