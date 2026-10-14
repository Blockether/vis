// @vitest-environment jsdom
// Regression for #381: the web app drew its shell like a desktop window. A browser tab must
// know that it is not the desktop app, so window styling never reaches it.
import { afterEach, describe, expect, it, vi } from 'vitest';

const native = vi.hoisted(() => ({ isNative: false }));

vi.mock('@capacitor/core', () => ({
  Capacitor: {
    getPlatform: () => (native.isNative ? 'ios' : 'web'),
    isNativePlatform: () => native.isNative,
    isPluginAvailable: () => false,
  },
  registerPlugin: () => ({}),
}));

type DesktopWindow = Window & { __TAURI__?: { core: { invoke: () => Promise<unknown> } } };

describe('the shell host', () => {
  afterEach(() => {
    delete (window as DesktopWindow).__TAURI__;
    delete document.documentElement.dataset.host;
    native.isNative = false;
  });

  it('marks a browser tab as the web', async () => {
    const { stampShellHost } = await import('./host');
    expect(stampShellHost()).toBe('web');
    expect(document.documentElement.dataset.host).toBe('web');
  });

  it('marks the Pake window as the desktop app', async () => {
    (window as DesktopWindow).__TAURI__ = { core: { invoke: () => Promise.resolve() } };
    const { stampShellHost } = await import('./host');
    expect(stampShellHost()).toBe('desktop');
    expect(document.documentElement.dataset.host).toBe('desktop');
  });

  it('marks a Capacitor app as native', async () => {
    native.isNative = true;
    const { stampShellHost } = await import('./host');
    expect(stampShellHost()).toBe('native');
  });
});
