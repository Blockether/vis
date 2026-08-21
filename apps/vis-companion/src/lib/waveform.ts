/**
 * THE SHAPE OF A SPOKEN REPLY, read off the bytes that will be played.
 *
 * A drawn bar is a CLAIM about audio, so only real samples may draw one: this reads
 * the WAV the machine sent and nothing else. Device text-to-speech hands out no
 * samples at all, and `[]` is exactly that answer - "no shape is known" - never a
 * placeholder wiggle standing in for one. Pure and synchronous: no `AudioContext`,
 * no platform, so the transcript can test it.
 */
const chunkId = (view: DataView, at: number) =>
  String.fromCharCode(
    view.getUint8(at),
    view.getUint8(at + 1),
    view.getUint8(at + 2),
    view.getUint8(at + 3),
  );

/** At most this many samples are read per bar: a bar is an average, not a survey. */
const PER_BAR = 256;

export function wavePeaks(buffer: ArrayBuffer, buckets: number): number[] {
  if (buckets <= 0 || buffer.byteLength < 44) return [];
  const view = new DataView(buffer);
  if (chunkId(view, 0) !== "RIFF" || chunkId(view, 8) !== "WAVE") return [];

  let format = 0;
  let channels = 0;
  let bits = 0;
  let dataAt = 0;
  let dataBytes = 0;
  let at = 12;
  while (at + 8 <= view.byteLength) {
    const id = chunkId(view, at);
    const size = view.getUint32(at + 4, true);
    const body = at + 8;
    if (id === "fmt " && body + 16 <= view.byteLength) {
      format = view.getUint16(body, true);
      channels = view.getUint16(body + 2, true);
      bits = view.getUint16(body + 14, true);
    } else if (id === "data") {
      dataAt = body;
      dataBytes = Math.max(0, Math.min(size, view.byteLength - body));
    }
    at = body + size + (size % 2);
  }

  const width = Math.floor(bits / 8);
  if (channels <= 0 || width <= 0 || dataBytes <= 0) return [];
  const frames = Math.floor(dataBytes / (width * channels));
  if (frames <= 0) return [];

  const sample = (frame: number): number | null => {
    const offset = dataAt + frame * width * channels;
    if (offset + width > view.byteLength) return null;
    if (format === 3) {
      if (bits === 32) return view.getFloat32(offset, true);
      if (bits === 64) return view.getFloat64(offset, true);
      return null;
    }
    if (format !== 1) return null;
    if (bits === 8) return (view.getUint8(offset) - 128) / 128;
    if (bits === 16) return view.getInt16(offset, true) / 32768;
    if (bits === 24) {
      const raw =
        view.getUint8(offset) |
        (view.getUint8(offset + 1) << 8) |
        (view.getInt8(offset + 2) << 16);
      return raw / 8388608;
    }
    if (bits === 32) return view.getInt32(offset, true) / 2147483648;
    return null;
  };

  const bars: number[] = [];
  const span = frames / buckets;
  for (let bar = 0; bar < buckets; bar += 1) {
    const from = Math.floor(bar * span);
    const to = Math.max(from + 1, Math.floor((bar + 1) * span));
    const step = Math.max(1, Math.floor((to - from) / PER_BAR));
    let sum = 0;
    let taken = 0;
    for (let frame = from; frame < to; frame += step) {
      const value = sample(frame);
      if (value === null) return [];
      sum += value * value;
      taken += 1;
    }
    bars.push(taken ? Math.sqrt(sum / taken) : 0);
  }

  const loudest = Math.max(...bars);
  if (!(loudest > 0)) return [];
  return bars.map((bar) => Math.min(1, Math.round((bar / loudest) * 1000) / 1000));
}
