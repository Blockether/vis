import { afterEach, expect, it, vi } from 'vitest';
import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';

afterEach(() => {
  vi.unstubAllGlobals();
  vi.resetModules();
});
it('bounds the actual worklet message queue and never echoes captured samples', async () => {
  class Processor {
    port = {
      onmessage: null as ((event: { data: { type: string } }) => void) | null,
      postMessage: vi.fn(),
    };
    process(_inputs: Float32Array[][]): boolean {
      return true;
    }
  }
  let Worklet = Processor;
  vi.stubGlobal('sampleRate', 48000);
  vi.stubGlobal('AudioWorkletProcessor', Processor);
  vi.stubGlobal('registerProcessor', (_name: string, processor: typeof Processor) => {
    Worklet = processor;
  });
  await import('./live-voice-worklet');
  const node = new Worklet();
  const input = [[new Float32Array(960).fill(0.2)]];
  for (let i = 0; i < L.input_buffer_ms / L.frame_ms; i++) expect(node.process(input)).toBe(true);
  expect(node.process(input)).toBe(false);
  expect(node.port.postMessage).toHaveBeenLastCalledWith({ error: 'Voice capture queue is full.' });
  const live = new Worklet();
  for (let i = 0; i < 200; i++) {
    expect(live.process(input)).toBe(true);
    live.port.onmessage?.({ data: { type: 'ack' } });
  }
  live.port.onmessage?.({ data: { type: 'stop' } });
  expect(live.process(input)).toBe(false);
});
