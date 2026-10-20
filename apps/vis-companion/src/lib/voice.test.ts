// @vitest-environment jsdom
// The desktop app is signed with the hardened runtime: a build without the microphone entitlement
// is refused by macOS, and WebKit reports it as "The request is not allowed by the user agent".
// A refused microphone has to say where the permission can be changed.
import { afterEach, beforeEach, expect, it, vi } from 'vitest';

import { insecurePageMessage, startWavRecording } from './voice';

const getUserMedia = vi.fn();
const desktopHost = window as unknown as { __TAURI__?: unknown };

beforeEach(() => {
  getUserMedia.mockReset();
  Object.defineProperty(navigator, 'mediaDevices', {
    configurable: true,
    value: { getUserMedia },
  });
  delete desktopHost.__TAURI__;
  Object.defineProperty(window, 'isSecureContext', { configurable: true, value: true });
});

afterEach(() => {
  vi.unstubAllGlobals();
  delete (navigator as Navigator & { audioSession?: unknown }).audioSession;
});

const denied = () =>
  new DOMException('The request is not allowed by the user agent.', 'NotAllowedError');

it('sends a denied desktop microphone to the macOS privacy settings', async () => {
  desktopHost.__TAURI__ = { core: { invoke: vi.fn() } };
  getUserMedia.mockRejectedValue(denied());

  await expect(startWavRecording()).rejects.toThrow(
    'Microphone permission denied. Allow Vis in System Settings, under Privacy & Security.',
  );
});

it('asks for site permission in a browser instead', async () => {
  getUserMedia.mockRejectedValue(denied());

  await expect(startWavRecording()).rejects.toThrow(
    'Microphone permission denied. Allow microphone access for this site and try again.',
  );
});

it('names a missing microphone and keeps every other failure as it is', async () => {
  getUserMedia.mockRejectedValue(new DOMException('No device', 'NotFoundError'));
  await expect(startWavRecording()).rejects.toThrow('No microphone is available on this device');

  getUserMedia.mockRejectedValue(new Error('capture stopped'));
  await expect(startWavRecording()).rejects.toThrow('capture stopped');
});

it('refuses a device that cannot record at all', async () => {
  Object.defineProperty(navigator, 'mediaDevices', { configurable: true, value: undefined });

  await expect(startWavRecording()).rejects.toThrow(
    'Microphone recording is unavailable on this device',
  );
});

// A browser on Windows that opens the WSL address of the gateway (http://172.x.x.x) gets an
// insecure page: Chrome then hides navigator.mediaDevices. Listing the origin in the Chrome flag,
// or HTTPS, makes the page secure.
it('names the Chrome flag for the page origin when the page is not a secure context', async () => {
  Object.defineProperty(navigator, 'mediaDevices', { configurable: true, value: undefined });
  Object.defineProperty(window, 'isSecureContext', { configurable: true, value: false });

  await expect(startWavRecording()).rejects.toThrow('Microphone recording needs a secure page');
  expect(insecurePageMessage(new URL('http://172.20.1.2:7890/sessions/a'))).toBe(
    'Microphone recording needs a secure page (HTTPS). In Chrome, add http://172.20.1.2:7890 in ' +
      'chrome://flags/#unsafely-treat-insecure-origin-as-secure, then relaunch Chrome.',
  );
});

// Regression, user report: after a dictation ended on iOS, the app kept the microphone indicator
// whenever it left the foreground. WebKit deactivates the audio session only when capture stops,
// and it skips that while the page still declares `play-and-record`
// (MediaSessionManagerInterface::maybeDeactivateAudioSession). A live AudioContext also keeps the
// session. The session type must return to `auto` and the context must close BEFORE the track stops.
function fakeAudio({ resumes = true } = {}) {
  const events: string[] = [];
  const audioSession = {
    current: 'auto',
    get type() {
      return this.current;
    },
    set type(value: string) {
      this.current = value;
      events.push(`session:${value}`);
    },
  };
  Object.defineProperty(navigator, 'audioSession', { configurable: true, value: audioSession });
  const track = {
    addEventListener: vi.fn(),
    stop: () => events.push('track:stop'),
  };
  getUserMedia.mockResolvedValue({ getTracks: () => [track] });
  const node = () => ({ connect: vi.fn(), disconnect: vi.fn() });
  const processor = { ...node(), onaudioprocess: null as ((event: unknown) => void) | null };
  class FakeAudioContext {
    sampleRate = 16000;
    state = 'suspended';
    destination = {};
    onstatechange: (() => void) | null = null;
    createMediaStreamSource = node;
    createScriptProcessor = () => processor;
    createGain = () => ({ ...node(), gain: { value: 1 } });
    async resume() {
      if (resumes) this.state = 'running';
    }
    async close() {
      this.state = 'closed';
      events.push('context:close');
    }
  }
  vi.stubGlobal('AudioContext', FakeAudioContext);
  const speak = () =>
    processor.onaudioprocess?.({ inputBuffer: { getChannelData: () => new Float32Array(64).fill(0.5) } });
  return { events, speak };
}

it('releases the iOS audio session before the microphone stops when a dictation ends', async () => {
  const { events, speak } = fakeAudio();
  const recording = await startWavRecording();
  speak();
  events.length = 0;

  await recording.stop();

  expect(events).toEqual(['session:auto', 'context:close', 'track:stop']);
});

it('releases the iOS audio session the same way when a dictation is cancelled', async () => {
  const { events } = fakeAudio();
  const recording = await startWavRecording();
  events.length = 0;

  await recording.cancel();

  expect(events).toEqual(['session:auto', 'context:close', 'track:stop']);
});

it('releases the iOS audio session the same way when the microphone cannot start', async () => {
  const { events } = fakeAudio({ resumes: false });

  await expect(startWavRecording()).rejects.toThrow('Microphone could not start');

  expect(events).toEqual(['session:play-and-record', 'session:auto', 'context:close', 'track:stop']);
});
