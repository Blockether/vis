// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { BrowserLiveVoiceAudio, createLiveVoiceAudio, liveVoiceDeviceReason, liveVoiceNeedsVerification } from './live-voice-audio';
import { NativeLiveVoiceAudio } from './live-voice-native-audio';

const native = vi.hoisted(() => ({ value: false, available: true, platform: 'ios' }));
const outputLease = vi.hoisted(() => ({ release: vi.fn() }));
vi.mock('./speech', () => ({
  speechOutput: { reserveLiveOutput: () => outputLease.release },
}));
vi.mock('@capacitor/core', () => ({
  Capacitor: {
    isNativePlatform: () => native.value,
    getPlatform: () => native.platform,
    isPluginAvailable: () => native.available,
  },
  registerPlugin: vi.fn(() => ({})),
}));
class FakeNode {
  connect = vi.fn();
  disconnect = vi.fn();
  start = vi.fn();
  stop = vi.fn();
  buffer: unknown;
}
class FakeContext extends EventTarget {
  static last: FakeContext;
  state = 'suspended';
  currentTime = 0;
  baseLatency = 0.02;
  outputLatency = 0.03;
  destination = {};
  audioWorklet = { addModule: vi.fn(async () => undefined) };
  sources: FakeNode[] = [];
  constructor() {
    super();
    FakeContext.last = this;
  }
  resume = vi.fn(async () => {
    this.state = 'running';
  });
  close = vi.fn(async () => {
    this.state = 'closed';
  });
  createMediaStreamSource = vi.fn(() => new FakeNode());
  createBuffer = vi.fn((_channels: number, count: number) => ({
    getChannelData: () => new Float32Array(count),
  }));
  createBufferSource = vi.fn(() => {
    const source = new FakeNode();
    this.sources.push(source);
    return source;
  });
}
class FakeWorklet extends FakeNode {
  static last: FakeWorklet;
  onprocessorerror: ((event: Event) => void) | null = null;
  port = {
    onmessage: null as ((event: { data: unknown }) => void) | null,
    postMessage: vi.fn(),
    close: vi.fn(),
  };
  constructor() {
    super();
    FakeWorklet.last = this;
  }
}
let tracks: (EventTarget & {
  stop: ReturnType<typeof vi.fn>;
  getSettings?: () => MediaTrackSettings;
})[];
let devices: EventTarget & { getUserMedia: ReturnType<typeof vi.fn> };
const audios: BrowserLiveVoiceAudio[] = [];
function audio() {
  const value = new BrowserLiveVoiceAudio();
  audios.push(value);
  return value;
}
beforeEach(() => {
  vi.useFakeTimers();
  native.value = false;
  native.available = true;
  native.platform = 'ios';
  outputLease.release.mockClear();
  tracks = [
    Object.assign(new EventTarget(), {
      stop: vi.fn(),
      getSettings: () => ({ echoCancellation: true }),
    }),
  ];
  devices = Object.assign(new EventTarget(), {
    getUserMedia: vi.fn(async () => ({ getTracks: () => tracks })),
  });
  vi.stubGlobal('isSecureContext', true);
  vi.stubGlobal('AudioContext', FakeContext);
  vi.stubGlobal('AudioWorkletNode', FakeWorklet);
  Object.defineProperty(navigator, 'mediaDevices', {
    configurable: true,
    value: devices,
  });
});
afterEach(() => {
  for (const value of audios.splice(0)) value.close();
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe('browser live audio lifecycle', () => {
  it.each([false, undefined])(
    'refuses unverified AEC (%s) before connecting capture',
    async (echoCancellation) => {
      const value = audio();
      await value.prepare(vi.fn());
      tracks[0].getSettings = () => ({ echoCancellation });
      await expect(value.capture(vi.fn())).rejects.toThrow('Use dictation');
      expect(tracks[0].stop).toHaveBeenCalledTimes(1);
      expect(FakeContext.last.createMediaStreamSource).not.toHaveBeenCalled();
    },
  );
  it('refuses tracks without settings and stops every acquired track', async () => {
    const value = audio();
    await value.prepare(vi.fn());
    delete tracks[0].getSettings;
    tracks.push(Object.assign(new EventTarget(), { stop: vi.fn() }));
    await expect(value.capture(vi.fn())).rejects.toThrow('Use dictation');
    for (const track of tracks) expect(track.stop).toHaveBeenCalledTimes(1);
  });
  it('requests echo controls and streams PCM without a microphone-to-speaker connection', async () => {
    const value = audio();
    await value.prepare(vi.fn());
    const frame = vi.fn();
    await value.capture(frame);
    expect(devices.getUserMedia).toHaveBeenCalledWith({
      audio: {
        channelCount: 1,
        echoCancellation: true,
        noiseSuppression: true,
        autoGainControl: true,
      },
    });
    FakeWorklet.last.port.onmessage?.({ data: new Int16Array(480) });
    expect(frame).toHaveBeenCalledTimes(1);
    expect(FakeWorklet.last.port.postMessage).toHaveBeenCalledWith({
      type: 'ack',
    });
    value.stopCapture();
    expect(tracks[0].stop).toHaveBeenCalledTimes(1);
    expect(FakeWorklet.last.port.close).toHaveBeenCalledTimes(1);
  });
  it.each(['mute', 'replace', 'close'] as const)(
    'ignores queued processor errors after %s',
    async (action) => {
      const value = audio();
      const failure = vi.fn();
      await value.prepare(failure);
      await value.capture(vi.fn());
      const previous = FakeWorklet.last;
      const queuedError = previous.onprocessorerror;
      if (action === 'replace') await value.capture(vi.fn());
      else if (action === 'close') value.close();
      else value.stopCapture();
      queuedError?.(new Event('processorerror'));
      expect(failure).not.toHaveBeenCalled();
      expect(previous.onprocessorerror).toBeNull();
    },
  );
  it('reports a processor failure from the current capture', async () => {
    const value = audio();
    const failure = vi.fn();
    await value.prepare(failure);
    await value.capture(vi.fn());
    FakeWorklet.last.onprocessorerror?.(new Event('processorerror'));
    expect(failure).toHaveBeenCalledExactlyOnceWith('The microphone processor stopped.');
  });
  it('stops tracks from a permission request that resolves after mute', async () => {
    const value = audio();
    await value.prepare(vi.fn());
    let grant!: (stream: { getTracks: () => typeof tracks }) => void;
    devices.getUserMedia.mockReturnValue(
      new Promise((resolve) => {
        grant = resolve;
      }),
    );
    const pending = value.capture(vi.fn());
    value.stopCapture();
    grant({ getTracks: () => tracks });
    await pending;
    expect(tracks[0].stop).toHaveBeenCalledTimes(1);
    expect(FakeContext.last.createMediaStreamSource).not.toHaveBeenCalled();
  });
  it('aligns output references with scheduled audio and device latency without granting credit', async () => {
    const value = audio();
    await value.prepare(vi.fn());
    const played = vi.fn();
    const start = value.outputReferenceStartMs!;
    expect(start - performance.now()).toBeCloseTo(170);
    value.play(new Int16Array(480), played);
    expect(value.outputReferenceStartMs! - start).toBeCloseTo(20);
    expect(played).not.toHaveBeenCalled();
    FakeContext.last.currentTime = 0.5;
    expect(value.outputReferenceStartMs! - start).toBeCloseTo(500);
  });
  it('reports only conservatively played samples and bounds output credit', async () => {
    const value = audio();
    await value.prepare(vi.fn());
    const played = vi.fn();
    value.play(new Int16Array(480), played);
    expect(played).not.toHaveBeenCalled();
    FakeContext.last.currentTime = 0.2;
    await vi.advanceTimersByTimeAsync(40);
    expect(played).not.toHaveBeenCalled();
    FakeContext.last.currentTime = 0.22;
    await vi.advanceTimersByTimeAsync(40);
    expect(played.mock.calls.at(-1)?.[0]).toBeGreaterThanOrEqual(239);
    expect(played.mock.calls.at(-1)?.[0]).toBeLessThan(480);
    value.stopOutput();
    expect(FakeContext.last.sources[0].stop).toHaveBeenCalled();
    for (let i = 0; i < 50; i++) value.play(new Int16Array(480), vi.fn());
    expect(() => value.play(new Int16Array(480), vi.fn())).toThrow('credit');
  });
  it('fails on a lost track, focus, device, or suspended context and removes listeners', async () => {
    const value = audio();
    const failure = vi.fn();
    await value.prepare(failure);
    await value.capture(vi.fn());
    tracks[0].dispatchEvent(new Event('ended'));
    devices.dispatchEvent(new Event('devicechange'));
    window.dispatchEvent(new Event('blur'));
    FakeContext.last.state = 'suspended';
    FakeContext.last.dispatchEvent(new Event('statechange'));
    expect(failure).toHaveBeenCalledTimes(4);
    value.close();
    expect(outputLease.release).toHaveBeenCalledTimes(1);
    devices.dispatchEvent(new Event('devicechange'));
    window.dispatchEvent(new Event('blur'));
    expect(failure).toHaveBeenCalledTimes(4);
  });
  it('selects gated native audio only when its app plugin is installed', () => {
    native.value = true;
    expect(liveVoiceNeedsVerification()).toBe(true);
    expect(liveVoiceDeviceReason()).toBeNull();
    expect(createLiveVoiceAudio()).toBeInstanceOf(NativeLiveVoiceAudio);
    native.available = false;
    expect(liveVoiceDeviceReason()).toMatch(/Install an app build/);
    native.value = false;
    vi.stubGlobal('isSecureContext', false);
    expect(liveVoiceDeviceReason()).toMatch(/secure desktop browser/);
    expect(createLiveVoiceAudio()).toBeInstanceOf(BrowserLiveVoiceAudio);
    expect(liveVoiceNeedsVerification()).toBe(false);
  });
});
