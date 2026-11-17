// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { NativeLiveVoiceAudio } from './live-voice-native-audio';
import type { NativeLiveVoicePlugin, NativeVoiceCapabilities } from './live-voice-native-bridge';
import { LIVE_VOICE_LIMITS as L } from './live-voice-protocol';

const output = vi.hoisted(() => ({ release: vi.fn(), reserve: vi.fn() }));
vi.mock('./speech', () => ({
  speechOutput: { reserveLiveOutput: () => { output.reserve(); return output.release; } },
}));
vi.mock('@capacitor/core', () => ({ registerPlugin: vi.fn(() => ({})) }));

const capabilities: NativeVoiceCapabilities = {
  platform: 'ios', available: true, echoCancellation: true, requiresVerification: true,
};
const adapters: NativeLiveVoiceAudio[] = [];
function harness(allowed = true) {
  const listeners = new Map<string, (event: unknown) => void>();
  const removers: ReturnType<typeof vi.fn>[] = [];
  const plugin = {
    getCapabilities: vi.fn(async () => capabilities),
    prepare: vi.fn(async (_options: { sessionId: string }) => capabilities),
    startCapture: vi.fn(async (_options: { captureId: number }): Promise<void> => undefined),
    stopCapture: vi.fn(async () => undefined),
    acknowledgeCapture: vi.fn(async () => undefined),
    play: vi.fn(async () => undefined),
    stopOutput: vi.fn(async () => undefined),
    close: vi.fn(async () => undefined),
    addListener: vi.fn(async (name: string, callback: (event: unknown) => void) => {
      listeners.set(name, callback);
      const remove = vi.fn(async () => { listeners.delete(name); });
      removers.push(remove);
      return { remove };
    }),
  };
  const audio = new NativeLiveVoiceAudio(allowed, plugin as unknown as NativeLiveVoicePlugin);
  adapters.push(audio);
  const fail = vi.fn();
  const sessionId = () => plugin.prepare.mock.calls[0][0].sessionId;
  const emit = (name: string, payload: object) => listeners.get(name)?.({ sessionId: sessionId(), ...payload });
  const captureId = () => plugin.startCapture.mock.calls.at(-1)![0].captureId;
  return { audio, plugin, fail, sessionId, emit, captureId, removers };
}
function deferCapture(h: ReturnType<typeof harness>) {
  let resolve!: () => void;
  let reject!: (error: Error) => void;
  h.plugin.startCapture.mockImplementationOnce(() => new Promise<void>((done, fail) => {
    resolve = done;
    reject = fail;
  }));
  return { resolve: () => resolve(), reject: (error: Error) => reject(error) };
}
function pcm(first = 0) {
  const bytes = new Uint8Array(L.frame_samples * 2);
  new DataView(bytes.buffer).setInt16(0, first, true);
  return btoa(String.fromCharCode(...bytes));
}
beforeEach(() => {
  output.release.mockClear();
  output.reserve.mockClear();
  Object.defineProperty(document, 'hidden', { configurable: true, value: false });
});
afterEach(() => { for (const audio of adapters.splice(0)) audio.close(); });

describe('native live voice bridge', () => {
  // Regression: native capture stopped while the panel still showed Listening.
  it('reports a capture stall instead of remaining silently connected', async () => {
    vi.useFakeTimers();
    const h = harness();
    try {
      await h.audio.prepare(h.fail);
      await h.audio.capture(vi.fn());
      await vi.advanceTimersByTimeAsync(3500);
      expect(h.fail).toHaveBeenCalledWith('Microphone stopped delivering audio. Start voice again.');
      expect(h.plugin.close).toHaveBeenCalledTimes(1);
    } finally { h.audio.close(); vi.useRealTimers(); }
  });
  it('keeps capture alive while frames arrive and detects a later stall', async () => {
    vi.useFakeTimers();
    const h = harness();
    try {
      await h.audio.prepare(h.fail);
      await h.audio.capture(vi.fn());
      for (let sequence = 0; sequence < 3; sequence++) {
        await vi.advanceTimersByTimeAsync(2000);
        h.emit('capture', { captureId: h.captureId(), sequence, pcm: pcm() });
        expect(h.fail).not.toHaveBeenCalled();
      }
      await vi.advanceTimersByTimeAsync(3000);
      expect(h.fail).toHaveBeenCalledTimes(1);
    } finally { h.audio.close(); vi.useRealTimers(); }
  });
  it('does not report a capture stall after mute', async () => {
    vi.useFakeTimers();
    const h = harness();
    try {
      await h.audio.prepare(h.fail);
      await h.audio.capture(vi.fn());
      h.audio.stopCapture();
      await vi.advanceTimersByTimeAsync(3500);
      expect(h.fail).not.toHaveBeenCalled();
    } finally { h.audio.close(); vi.useRealTimers(); }
  });
  it('requires explicit mobile test opt-in before capability or device access', async () => {
    const h = harness(false);
    await expect(h.audio.prepare(h.fail)).rejects.toThrow('Enable mobile audio testing');
    expect(h.plugin.getCapabilities).not.toHaveBeenCalled();
    expect(h.plugin.prepare).not.toHaveBeenCalled();
  });
  it.each(['available', 'echoCancellation'] as const)('fails closed without %s', async (field) => {
    const h = harness();
    h.plugin.getCapabilities.mockResolvedValue({ ...capabilities, [field]: false });
    await expect(h.audio.prepare(h.fail)).rejects.toThrow('echo cancellation');
    expect(h.plugin.startCapture).not.toHaveBeenCalled();
    expect(h.plugin.prepare).not.toHaveBeenCalled();
  });
  it('rechecks echo cancellation after native preparation and releases ownership on failure', async () => {
    const h = harness();
    h.plugin.prepare.mockResolvedValue({ ...capabilities, echoCancellation: false });
    await expect(h.audio.prepare(h.fail)).rejects.toThrow('echo cancellation');
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
    expect(output.release).toHaveBeenCalledTimes(1);
    expect(h.plugin.startCapture).not.toHaveBeenCalled();
  });

  it('removes other listeners when one listener cannot be installed', async () => {
    const h = harness();
    h.plugin.addListener.mockRejectedValueOnce(new Error('Listener unavailable.'));
    await expect(h.audio.prepare(h.fail)).rejects.toThrow('Listener unavailable.');
    expect(h.plugin.prepare).not.toHaveBeenCalled();
    expect(h.removers).toHaveLength(2);
    expect(h.removers.every((remove) => remove.mock.calls.length === 1)).toBe(true);
    expect(output.release).toHaveBeenCalledTimes(1);
  });

  it('bounds pending bridge commands without delaying stop behind them', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    h.plugin.stopOutput.mockReturnValue(new Promise<undefined>(() => {}));
    const bound = L.input_buffer_ms / L.frame_ms + L.output_credit_ms / L.frame_ms + 8;
    for (let index = 0; index <= bound; index++) h.audio.stopOutput();
    expect(h.plugin.stopOutput).toHaveBeenCalledTimes(bound);
    expect(h.fail).toHaveBeenCalledWith('The native audio bridge exceeded its buffer limit.');
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
  });

  it('ignores failures from invalidated output but closes on a current failure', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    h.plugin.play.mockRejectedValueOnce(new Error('Old output failed.'));
    h.audio.play(new Int16Array(480), vi.fn());
    h.audio.stopOutput();
    await Promise.resolve();
    expect(h.fail).not.toHaveBeenCalled();
    h.plugin.play.mockRejectedValueOnce(new Error('Current output failed.'));
    h.audio.play(new Int16Array(480), vi.fn());
    await vi.waitFor(() => expect(h.fail).toHaveBeenCalledWith('Current output failed.'));
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
  });

  it('uses canonical limits, reserves output, and does not capture during preparation', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    expect(h.plugin.prepare).toHaveBeenCalledWith({
      sessionId: expect.any(String), sampleRate: L.sample_rate, frameSamples: L.frame_samples,
      inputBufferFrames: L.input_buffer_ms / L.frame_ms,
      outputBufferSamples: L.sample_rate * L.output_credit_ms / 1000,
    });
    expect(h.plugin.startCapture).not.toHaveBeenCalled();
    expect(h.audio.captureRestartsOutput).toBe(true);
    h.audio.close();
    h.audio.close();
    expect(output.release).toHaveBeenCalledTimes(1);
    expect(h.removers.every((remove) => remove.mock.calls.length === 1)).toBe(true);
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
  });
  it('decodes PCM and acknowledges ordered capture with ownership and generation', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const frame = vi.fn();
    await h.audio.capture(frame);
    h.emit('capture', { captureId: h.captureId(), sequence: 0, pcm: pcm(-32768) });
    expect(frame).toHaveBeenCalledTimes(1);
    expect(frame.mock.calls[0][0][0]).toBe(-32768);
    expect(h.plugin.acknowledgeCapture).toHaveBeenCalledWith({ sessionId: h.sessionId(), captureId: h.captureId(), sequence: 0 });
    h.emit('capture', { sessionId: 'other', captureId: h.captureId(), sequence: 1, pcm: pcm() });
    h.audio.stopCapture();
    h.emit('capture', { captureId: h.captureId(), sequence: 1, pcm: pcm() });
    expect(frame).toHaveBeenCalledTimes(1);
    expect(h.plugin.stopCapture).toHaveBeenCalledWith({ sessionId: h.sessionId(), captureId: h.captureId() + 1 });
  });
  it.each([{ sequence: 1, pcm: pcm() }, { sequence: 0, pcm: 'bad' }])('closes on invalid native capture', async (invalid) => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const frame = vi.fn();
    await h.audio.capture(frame);
    h.emit('capture', { captureId: h.captureId(), ...invalid });
    expect(h.fail).toHaveBeenCalledTimes(1);
    expect(frame).not.toHaveBeenCalled();
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
  });
  it('sends stop while a microphone permission request is still pending', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    let finish!: () => void;
    h.plugin.startCapture.mockImplementation(() => new Promise<void>((resolve) => { finish = resolve; }));
    const frame = vi.fn();
    const opening = h.audio.capture(frame);
    h.audio.stopCapture();
    expect(h.plugin.stopCapture).toHaveBeenCalledTimes(1);
    finish();
    await opening;
    h.emit('capture', { captureId: h.captureId(), sequence: 0, pcm: pcm() });
    expect(frame).not.toHaveBeenCalled();
  });
  // Regression: iOS startup retries can complete buffers that were not played.
  it('waits for capture startup before submitting output or trusting playback receipts', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const pending = deferCapture(h);
    const captured = vi.fn();
    const opening = h.audio.capture(captured);
    const played = vi.fn();
    h.audio.play(new Int16Array([32767, -32768]), played);
    h.audio.play(new Int16Array([1234]), played);
    expect(h.plugin.play).not.toHaveBeenCalled();
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 2 });
    expect(played).not.toHaveBeenCalled();
    h.emit('capture', { captureId: h.captureId(), sequence: 0, pcm: pcm() });
    expect(captured).toHaveBeenCalledTimes(1);
    expect(h.plugin.play).not.toHaveBeenCalled();
    pending.resolve();
    await opening;
    expect(h.plugin.play.mock.calls).toEqual([
      [{ sessionId: h.sessionId(), outputId: 0, frameId: 0, pcm: '/38AgA==' }],
      [{ sessionId: h.sessionId(), outputId: 0, frameId: 1, pcm: '0gQ=' }],
    ]);
    expect(played).not.toHaveBeenCalled();
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 2 });
    expect(played).toHaveBeenCalledExactlyOnceWith(2);
    expect(h.fail).not.toHaveBeenCalled();
  });
  it('keeps startup output within the existing credit limit', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const pending = deferCapture(h);
    const opening = h.audio.capture(vi.fn());
    const frames = L.output_credit_ms / L.frame_ms;
    for (let index = 0; index < frames; index++) h.audio.play(new Int16Array(480), vi.fn());
    expect(() => h.audio.play(new Int16Array(480), vi.fn())).toThrow('exceeded its credit');
    expect(h.plugin.play).not.toHaveBeenCalled();
    pending.resolve();
    await opening;
    expect(h.plugin.play).toHaveBeenCalledTimes(frames);
    expect(() => h.audio.play(new Int16Array(480), vi.fn())).toThrow('exceeded its credit');
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 480 });
    expect(() => h.audio.play(new Int16Array(480), vi.fn())).not.toThrow();
  });
  it.each(['stopCapture', 'stopOutput', 'close'] as const)(
    'discards startup output without playback credit after %s', async (stop) => {
      const h = harness();
      await h.audio.prepare(h.fail);
      const pending = deferCapture(h);
      const opening = h.audio.capture(vi.fn());
      const played = vi.fn();
      h.audio.play(new Int16Array(480), played);
      h.audio[stop]();
      pending.resolve();
      await opening;
      expect(h.plugin.play).not.toHaveBeenCalled();
      expect(played).not.toHaveBeenCalled();
      expect(h.fail).not.toHaveBeenCalled();
    },
  );
  it('does not flush output when an older capture startup finishes', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const first = deferCapture(h);
    const oldOpening = h.audio.capture(vi.fn());
    const stale = vi.fn();
    h.audio.play(new Int16Array(480), stale);
    const second = deferCapture(h);
    const opening = h.audio.capture(vi.fn());
    const played = vi.fn();
    h.audio.play(new Int16Array(480), played);
    first.resolve();
    await oldOpening;
    expect(h.plugin.play).not.toHaveBeenCalled();
    second.resolve();
    await opening;
    expect(h.plugin.play).toHaveBeenCalledTimes(1);
    expect(h.plugin.play).toHaveBeenCalledWith(expect.objectContaining({ frameId: 1 }));
    expect(stale).not.toHaveBeenCalled();
    expect(played).not.toHaveBeenCalled();
  });
  it('submits only current output after interruption during capture startup', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const pending = deferCapture(h);
    const opening = h.audio.capture(vi.fn());
    const stale = vi.fn();
    h.audio.play(new Int16Array(480), stale);
    h.audio.stopOutput();
    h.audio.play(new Int16Array(480), vi.fn());
    pending.resolve();
    await opening;
    expect(h.plugin.play).toHaveBeenCalledTimes(1);
    expect(h.plugin.play).toHaveBeenCalledWith(expect.objectContaining({ outputId: 1, frameId: 1 }));
    expect(stale).not.toHaveBeenCalled();
  });
  it('discards output when capture startup fails', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const pending = deferCapture(h);
    const opening = h.audio.capture(vi.fn());
    const played = vi.fn();
    h.audio.play(new Int16Array(480), played);
    pending.reject(new Error('Capture startup failed.'));
    await opening;
    expect(h.plugin.play).not.toHaveBeenCalled();
    expect(played).not.toHaveBeenCalled();
    expect(h.fail).toHaveBeenCalledWith('Capture startup failed.');
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
  });
  it('reports native playback progress, never enqueue completion', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const played = vi.fn();
    h.audio.play(new Int16Array([32767, -32768]), played);
    await Promise.resolve();
    expect(h.plugin.play).toHaveBeenCalledWith({ sessionId: h.sessionId(), outputId: 0, frameId: 0, pcm: '/38AgA==' });
    expect(played).not.toHaveBeenCalled();
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 1 });
    expect(played).toHaveBeenLastCalledWith(1);
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 2 });
    expect(played).toHaveBeenLastCalledWith(2);
    expect(played).toHaveBeenCalledTimes(2);
  });
  it('invalidates old output receipts immediately without stopping capture', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    const frame = vi.fn();
    await h.audio.capture(frame);
    const played = vi.fn();
    h.audio.play(new Int16Array(480), played);
    h.audio.stopOutput();
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 480 });
    expect(played).not.toHaveBeenCalled();
    expect(h.plugin.stopOutput).toHaveBeenCalledWith({ sessionId: h.sessionId(), outputId: 1 });
    h.emit('capture', { captureId: h.captureId(), sequence: 0, pcm: pcm() });
    expect(frame).toHaveBeenCalledTimes(1);
  });
  it('bounds queued output and rejects impossible playback credit', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    for (let index = 0; index < L.output_credit_ms / L.frame_ms; index++) h.audio.play(new Int16Array(480), vi.fn());
    expect(() => h.audio.play(new Int16Array(480), vi.fn())).toThrow('exceeded its credit');
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 481 });
    expect(h.fail).toHaveBeenCalledWith('Native playback reported an invalid position.');
  });
  it('cleans up a preparation that completes after close', async () => {
    const h = harness();
    let finish!: (value: NativeVoiceCapabilities) => void;
    h.plugin.prepare.mockImplementation(() => new Promise((resolve) => { finish = resolve; }));
    const preparing = h.audio.prepare(h.fail);
    await vi.waitFor(() => expect(h.plugin.prepare).toHaveBeenCalled());
    h.audio.close();
    finish(capabilities);
    await expect(preparing).rejects.toThrow('cancelled');
    expect(h.plugin.close).toHaveBeenCalledTimes(2);
    expect(output.release).toHaveBeenCalledTimes(1);
  });
  it('ignores failures from another owner', async () => {
    const h = harness();
    await h.audio.prepare(h.fail);
    h.emit('failure', { sessionId: 'other', message: 'Old session failed.' });
    expect(h.fail).not.toHaveBeenCalled();
    h.emit('failure', { message: 'Live audio stopped.' });
    expect(h.fail).toHaveBeenCalledWith('Live audio stopped.');
    expect(h.plugin.close).toHaveBeenCalledTimes(1);
  });
  // A locked screen hides the page. Native audio continues, so the session must continue.
  it('keeps capture and playback when the screen locks and unlocks', async () => {
    const h = harness();
    const frames = vi.fn();
    const played = vi.fn();
    await h.audio.prepare(h.fail);
    await h.audio.capture(frames);
    Object.defineProperty(document, 'hidden', { configurable: true, value: true });
    document.dispatchEvent(new Event('visibilitychange'));
    h.emit('capture', { captureId: h.captureId(), sequence: 0, pcm: pcm(7) });
    h.audio.play(new Int16Array(480), played);
    h.emit('played', { outputId: 0, frameId: 0, playedSamples: 480 });
    Object.defineProperty(document, 'hidden', { configurable: true, value: false });
    document.dispatchEvent(new Event('visibilitychange'));
    h.emit('capture', { captureId: h.captureId(), sequence: 1, pcm: pcm(8) });
    expect(frames).toHaveBeenCalledTimes(2);
    expect(played).toHaveBeenCalledWith(480);
    expect(h.fail).not.toHaveBeenCalled();
    expect(h.plugin.close).not.toHaveBeenCalled();
  });
  // A resumed iOS web view can continue to report a hidden document.
  it('starts when the document reports hidden and the native bridge accepts', async () => {
    const h = harness();
    Object.defineProperty(document, 'hidden', { configurable: true, value: true });
    await h.audio.prepare(h.fail);
    await h.audio.capture(vi.fn());
    expect(h.plugin.startCapture).toHaveBeenCalledTimes(1);
    expect(h.fail).not.toHaveBeenCalled();
  });
});
