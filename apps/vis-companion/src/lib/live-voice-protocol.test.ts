import { describe, expect, it } from 'vitest';
import {
  decodeVoiceFrame,
  encodeVoiceFrame,
  encodeLiveVoiceControl,
  parseLiveVoiceEvent,
  LIVE_VOICE_LIMITS as L,
} from './live-voice-protocol';
import { VoiceEndpoint, VoiceResampler } from './live-voice-dsp';

describe('live voice binary contract', () => {
  it('uses the shared little-endian header and signed PCM', () => {
    const frame = {
      kind: 'input' as const,
      generation: 7,
      streamId: 9,
      sequence: 3,
      sampleOffset: 1440,
      samples: Int16Array.from([-32768, -1, 0, 32767]),
    };
    const encoded = encodeVoiceFrame(frame);
    expect(new TextDecoder().decode(encoded.slice(0, 4))).toBe('VISV');
    expect(new DataView(encoded).getUint32(24, true)).toBe(24000);
    expect(decodeVoiceFrame(encoded)).toEqual(frame);
  });
  it('rejects reserved flags, wrong rates, truncated packets and oversized frames', () => {
    const packet = () =>
      encodeVoiceFrame({
        kind: 'output',
        generation: 1,
        streamId: 1,
        sequence: 0,
        sampleOffset: 0,
        samples: new Int16Array(480),
      });
    for (const [offset, value] of [
      [4, 2],
      [5, 9],
      [6, 1],
      [24, 0],
      [28, 0],
    ]) {
      const data = packet();
      new DataView(data).setUint8(offset, value);
      expect(() => decodeVoiceFrame(data)).toThrow();
    }
    expect(() => decodeVoiceFrame(packet().slice(0, 33))).toThrow();
    expect(() => decodeVoiceFrame(new ArrayBuffer(3))).toThrow();
    expect(() =>
      encodeVoiceFrame({
        kind: 'input',
        generation: 0,
        streamId: 0,
        sequence: 0,
        sampleOffset: 0,
        samples: new Int16Array(481),
      }),
    ).toThrow();
  });
  it('validates events and controls from the canonical schema', () => {
    expect(
      parseLiveVoiceEvent(
        JSON.stringify({
          type: 'transcript',
          utterance_id: 1,
          revision: 0,
          text: 'Hello',
          final: false,
        }),
      ).type,
    ).toBe('transcript');
    expect(() => parseLiveVoiceEvent('{"type":"task.cancel"}')).toThrow();
    expect(() =>
      parseLiveVoiceEvent(
        JSON.stringify({
          type: 'transcript',
          utterance_id: -1,
          revision: 0,
          text: 'Hello',
          final: false,
        }),
      ),
    ).toThrow();
    expect(() => encodeLiveVoiceControl({ type: 'heartbeat', token: 'extra' })).toThrow();
    expect(() =>
      encodeLiveVoiceControl(JSON.parse('{"type":"heartbeat","constructor":{}}')),
    ).toThrow();
    expect(() => parseLiveVoiceEvent(' '.repeat(L.max_control_bytes + 1))).toThrow();
  });
});

describe('continuous capture and endpointing', () => {
  it.each([44100, 48000, 96000])('has no per-block rounding drift at %i Hz', (rate) => {
    const resampler = new VoiceResampler(rate);
    let count = 0;
    const total = rate * 5;
    for (let offset = 0; offset < total; offset += 128) {
      const frames = resampler.push(new Float32Array(Math.min(128, total - offset)).fill(0.25));
      count += frames.reduce((sum, frame) => sum + frame.length, 0);
      for (const frame of frames) expect(frame[10]).toBe(8192);
    }
    expect(Math.abs(count - 24000 * 5)).toBeLessThanOrEqual(L.frame_samples);
  });
  it('keeps interpolation continuous across arbitrary block boundaries', () => {
    const samples = Float32Array.from({ length: 44100 }, (_, i) => Math.sin(i / 20));
    const whole = new VoiceResampler(44100).push(samples).flatMap((frame) => [...frame]);
    const split = new VoiceResampler(44100);
    const parts: number[] = [];
    for (let i = 0; i < samples.length; i += 113)
      parts.push(...split.push(samples.slice(i, i + 113)).flatMap((frame) => [...frame]));
    expect(parts).toEqual(whole);
  });
  it('keeps bounded preroll, ignores short impulses and ends after silence', () => {
    const vad = new VoiceEndpoint();
    const quiet = new Int16Array(480);
    const loud = new Int16Array(480).fill(3000);
    for (let i = 0; i < 100; i++) expect(vad.push(quiet)).toEqual([]);
    expect(vad.push(loud)).toEqual([]);
    expect(vad.push(quiet)).toEqual([]);
    const events = Array.from({ length: 6 }, () => vad.push(loud)).flat();
    expect(events[0]).toEqual({ type: 'start' });
    expect(events.filter((event) => event.type === 'frame')).toHaveLength(10);
    for (let i = 0; i < 54; i++)
      expect(vad.push(quiet).some((event) => event.type === 'end')).toBe(false);
    expect(vad.push(quiet).at(-1)).toEqual({ type: 'end' });
    expect(vad.push(quiet)).toEqual([]);
  });
  it('bounds long utterances and discards retained input on reset', () => {
    const vad = new VoiceEndpoint();
    const loud = new Int16Array(480).fill(3000);
    let ends = 0;
    for (let i = 0; i < L.max_utterance_ms / L.frame_ms; i++)
      ends += vad.push(loud).filter((event) => event.type === 'end').length;
    expect(ends).toBe(1);
    vad.push(loud);
    vad.reset();
    expect(vad.push(loud)).toEqual([]);
  });
});
