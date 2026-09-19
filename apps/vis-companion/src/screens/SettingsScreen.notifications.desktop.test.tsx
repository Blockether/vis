// @vitest-environment jsdom
// The desktop app is this bundle inside a Pake (Tauri) window, where Web Push does not exist:
// the browser panel could only tell the reader that this browser does not support it, which is
// a dead end on a machine that CAN alert. The desktop panel offers the local switch instead.
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this file exist.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock('@capacitor/preferences', () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({ value: native.store.get(key) ?? null }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

vi.mock('@capacitor/core', () => ({
  Capacitor: {
    getPlatform: () => 'web',
    isNativePlatform: () => false,
    isPluginAvailable: () => false,
  },
  // The settings dialog reaches the device's speech engine, which registers a plugin at import.
  registerPlugin: () => ({
    speak: () => Promise.resolve(),
    stop: () => Promise.resolve(),
    getVoices: () => Promise.resolve({ voices: [] }),
  }),
}));
vi.mock('@capacitor/push-notifications', () => ({ PushNotifications: {} }));

import { NotificationsPanel } from './settings/NotificationSettings';
import { getGatewayNotify } from '../lib/storage';
import type { GatewayClient } from '../lib/gateway';

type DesktopWindow = Window & { __TAURI__?: { core: { invoke: ReturnType<typeof vi.fn> } } };

const MACHINE = 'http://10.0.0.5:7890';

/** The Pake desktop window, which answers every permission question with yes. */
function stubDesktopHost() {
  const invoke = vi.fn(async (command: string) =>
    command === 'plugin:notification|is_permission_granted' ? true : 'granted',
  );
  (window as DesktopWindow).__TAURI__ = { core: { invoke } };
  return invoke;
}

const panel = () => {
  stubDesktopHost();
  render(
    <NotificationsPanel
      client={{} as GatewayClient}
      gateway={{ url: MACHINE, label: 'buildbox' }}
    />,
  );
};

beforeEach(() => {
  native.store.clear();
  localStorage.clear();
});

afterEach(() => {
  delete (window as DesktopWindow).__TAURI__;
  document.body.innerHTML = '';
  vi.restoreAllMocks();
});

it('connects this machine from the desktop window', async () => {
  panel();
  const verb = await screen.findByRole('switch', { name: 'Notifications from buildbox: off' });

  fireEvent.click(verb);

  await waitFor(async () => expect(await getGatewayNotify(MACHINE)).toBe(true));
});

it('offers the switch instead of the browser push dead end', async () => {
  panel();

  expect(await screen.findByRole('switch', { name: /Notifications from buildbox/ })).toBeVisible();
  expect(screen.queryByText(/background Web Push/i)).toBeNull();
});

// The panel used to explain, under a connected switch, that alerts arrive only while the app is
// open. Reported as noise on every visit: the switch already answers the question it was under.
it('states the connection without explaining itself', async () => {
  panel();

  fireEvent.click(await screen.findByRole('switch', { name: 'Notifications from buildbox: off' }));

  expect(await screen.findByRole('switch', { name: 'Notifications from buildbox: on' })).toBeVisible();
  expect(screen.queryByText(/Alerts arrive while the desktop app is open/)).toBeNull();
});
