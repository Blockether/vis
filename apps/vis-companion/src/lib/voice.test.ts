// @vitest-environment jsdom
// The desktop app is signed with the hardened runtime: a build without the microphone entitlement
// is refused by macOS, and WebKit reports it as "The request is not allowed by the user agent".
// A refused microphone has to say where the permission can be changed.
import { beforeEach, expect, it, vi } from 'vitest';

import { startWavRecording } from './voice';

const getUserMedia = vi.fn();
const desktopHost = window as unknown as { __TAURI__?: unknown };

beforeEach(() => {
  getUserMedia.mockReset();
  Object.defineProperty(navigator, 'mediaDevices', {
    configurable: true,
    value: { getUserMedia },
  });
  delete desktopHost.__TAURI__;
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
