// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

/** What the OS would answer, and what it was asked to remove. */
const native = vi.hoisted(() => ({
  isNative: true,
  delivered: [] as { id: string; tag?: string; data?: Record<string, unknown> }[],
  removed: [] as { id: string; tag?: string }[][],
}));

vi.mock('@capacitor/core', () => ({
  Capacitor: {
    isNativePlatform: () => native.isNative,
    isPluginAvailable: (name: string) => native.isNative && name === 'PushNotifications',
    getPlatform: () => (native.isNative ? 'android' : 'web'),
  },
}));

vi.mock('@capacitor/push-notifications', () => ({
  PushNotifications: {
    getDeliveredNotifications: async () => ({ notifications: native.delivered }),
    removeDeliveredNotifications: async (arg: { notifications: { id: string; tag?: string }[] }) => {
      native.removed.push(arg.notifications);
    },
  },
}));

const fresh = async () => {
  vi.resetModules();
  return await import('./push');
};

beforeEach(() => {
  native.isNative = true;
  native.delivered = [];
  native.removed = [];
});

describe('dropDeliveredPushes', () => {
  // Android never sees the payload again: Firebase builds the tray entry itself
  // and copies only its own `android.*` keys into it, so a delivered alert there
  // knows its session ONLY by tag. Reading `data.session_id` alone matched
  // nothing, left every Android alert in the tray for good — and the launcher
  // badges its icon from exactly that tray.
  it('matches an Android alert by its tag', async () => {
    native.delivered = [
      { id: '1', tag: 's-read', data: { 'android.title': 'Vis' } },
      { id: '2', tag: 's-unread', data: {} },
    ];
    const { dropDeliveredPushes } = await fresh();

    const waiting = await dropDeliveredPushes((sessionId) => sessionId === 's-read');

    expect(waiting).toBe(1);
    // The whole notification goes back, because Android cancels by tag AND id.
    expect(native.removed).toEqual([[{ id: '1', tag: 's-read', data: { 'android.title': 'Vis' } }]]);
  });

  it('matches an iOS alert by the session id in its payload', async () => {
    native.delivered = [
      { id: 'a', data: { session_id: 's-read' } },
      { id: 'b', data: { session_id: 's-unread' } },
    ];
    const { dropDeliveredPushes } = await fresh();

    const waiting = await dropDeliveredPushes((sessionId) => sessionId === 's-read');

    expect(waiting).toBe(1);
    expect(native.removed).toEqual([[{ id: 'a', data: { session_id: 's-read' } }]]);
  });

  it('leaves an alert of a session this device knows nothing about', async () => {
    native.delivered = [{ id: '1', tag: 's-elsewhere' }];
    const { dropDeliveredPushes } = await fresh();

    expect(await dropDeliveredPushes((sessionId) => sessionId === 's-read')).toBe(1);
    expect(native.removed).toEqual([]);
  });

  it('touches nothing where there is no push plugin', async () => {
    native.isNative = false;
    native.delivered = [{ id: '1', tag: 's-read' }];
    const { dropDeliveredPushes } = await fresh();

    expect(await dropDeliveredPushes(() => true)).toBe(0);
    expect(native.removed).toEqual([]);
  });
});
