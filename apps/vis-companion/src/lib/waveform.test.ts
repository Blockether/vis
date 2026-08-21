// @vitest-environment node
import { describe, expect, it } from "vitest";

import { wavePeaks } from "./waveform";

/** A 16-bit mono WAV, exactly the `audio/wav` shape the gateway sends back. */
const wav = (samples: number[], rate = 8000) => {
  const buffer = new ArrayBuffer(44 + samples.length * 2);
  const view = new DataView(buffer);
  const ascii = (at: number, value: string) => {
    for (let index = 0; index < value.length; index += 1) {
      view.setUint8(at + index, value.charCodeAt(index));
    }
  };
  ascii(0, "RIFF");
  view.setUint32(4, 36 + samples.length * 2, true);
  ascii(8, "WAVE");
  ascii(12, "fmt ");
  view.setUint32(16, 16, true);
  view.setUint16(20, 1, true);
  view.setUint16(22, 1, true);
  view.setUint32(24, rate, true);
  view.setUint32(28, rate * 2, true);
  view.setUint16(32, 2, true);
  view.setUint16(34, 16, true);
  ascii(36, "data");
  view.setUint32(40, samples.length * 2, true);
  samples.forEach((sample, index) =>
    view.setInt16(44 + index * 2, Math.round(sample * 32767), true),
  );
  return buffer;
};

/** `count` samples alternating +/- `amplitude`: a tone with a known RMS. */
const tone = (count: number, amplitude: number) =>
  Array.from({ length: count }, (_, index) =>
    index % 2 === 0 ? amplitude : -amplitude,
  );

describe("wavePeaks", () => {
  it("reads one bar per bucket and normalises the loudest to 1", () => {
    const bars = wavePeaks(wav([...tone(400, 0.25), ...tone(400, 1)]), 4);

    expect(bars).toHaveLength(4);
    expect(Math.max(...bars)).toBe(1);
    expect(bars[0]).toBeCloseTo(0.25, 2);
    expect(bars[3]).toBe(1);
  });

  it("follows the loudness of the recording, quarter by quarter", () => {
    const bars = wavePeaks(
      wav([...tone(200, 0.2), ...tone(200, 0.4), ...tone(200, 0.8), ...tone(200, 1)]),
      4,
    );

    expect(bars[0]).toBeLessThan(bars[1]);
    expect(bars[1]).toBeLessThan(bars[2]);
    expect(bars[2]).toBeLessThan(bars[3]);
  });

  it("answers nothing rather than a shape when there is nothing to measure", () => {
    expect(wavePeaks(wav(tone(400, 0)), 8)).toEqual([]);
    expect(wavePeaks(wav(tone(400, 1)), 0)).toEqual([]);
    expect(wavePeaks(new ArrayBuffer(8), 8)).toEqual([]);
    expect(wavePeaks(new TextEncoder().encode("not audio at all").buffer, 8)).toEqual(
      [],
    );
  });
});
