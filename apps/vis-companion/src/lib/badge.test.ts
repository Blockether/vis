// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from 'vitest';

import type { FleetMachine } from './fleet';

const native = vi.hoisted(() => ({
  platform: 'ios',
  available: true,
  fails: false,
  waiting: 0,
  writes: [] as number[],
  dropped: [] as ((sessionId: string | undefined) => boolean)[],
}));

vi.mock('@capacitor/core', () => ({
  Capacitor: {
    getPlatform: () => native.platform,
    isPluginAvailable: (name: string) => native.available && name === 'VisBadge',
  },
  registerPlugin: () => ({
    set: async ({ count }: { count: number }) => {
      if (native.fails) throw new Error('not implemented');
      native.writes.push(count);
    },
  }),
}));

vi.mock('./push', () => ({
  dropDeliveredPushes: async (isDone: (sessionId: string | undefined) => boolean) => {
    native.dropped.push(isDone);
    return native.waiting;
  },
}));

// The module holds what it last told the OS, so every test gets its own copy.
const fresh = async () => {
  vi.resetModules();
  const [badge, unread] = await Promise.all([import('./badge'), import('./unread')]);
  return { ...badge, ...unread };
};

const row = (id: string, turns: number) => ({ id, turn_count: turns, status: 'idle' });
const machine = (sessions: ReturnType<typeof row>[], error?: string) =>
  ({ sessions, error } as unknown as FleetMachine);

beforeEach(() => {
  native.platform = 'ios';
  native.available = true;
  native.fails = false;
  native.waiting = 0;
  native.writes.length = 0;
  native.dropped.length = 0;
  localStorage.clear();
});

describe('the icon badge', () => {
  // Called on every fleet change — which on a busy machine is every poll — so a
  // number already showing must cost no bridge hop.
  it('writes a number once', async () => {
    const { setBadge } = await fresh();
    await setBadge(3);
    await setBadge(3);
    await setBadge(5);
    expect(native.writes).toEqual([3, 5]);
  });

  it('never asks iOS to paint a number it cannot', async () => {
    const { setBadge } = await fresh();
    await setBadge(2.7);
    await setBadge(-1);
    await setBadge(Number.NaN);
    expect(native.writes).toEqual([2, 0]);
  });

  // While the app was away, VisNotify moved the badge without telling anyone
  // here: the cached value is a lie the moment the app resumes.
  it('says it again on the way back', async () => {
    const { setBadge, reassertBadge } = await fresh();
    await setBadge(4);
    await reassertBadge();
    expect(native.writes).toEqual([4, 4]);
  });

  // An Android launcher badges itself from the notification FCM delivers, so a
  // number written here would be a second, competing source.
  it('leaves the count alone where the OS owns it', async () => {
    const { setBadge } = await fresh();
    native.platform = 'android';
    await setBadge(3);
    expect(native.writes).toEqual([]);
  });

  // A build without the VisBadge target answers "not implemented"; the badge is
  // never worth an unhandled rejection, and the next app that has it must start
  // from the truth rather than from a cached success.
  it('survives a host that has no badge plugin', async () => {
    const { setBadge } = await fresh();
    native.fails = true;
    await expect(setBadge(2)).resolves.toBeUndefined();
    native.fails = false;
    await setBadge(2);
    expect(native.writes).toEqual([2]);
  });
});

describe('syncBadge', () => {
  // The badge means "notifications you have not dealt with", so it is the tray
  // that decides the number — the same set VisNotify counts inside an arriving
  // alert. A fleet tally would be a second, disagreeing source.
  it('badges what is still waiting in the tray, not what the fleet holds', async () => {
    const { markSessionRead, syncBadge } = await fresh();
    native.waiting = 3;
    markSessionRead('unread', 1);
    await syncBadge([machine([row('unread', 40)])]);
    expect(native.writes).toEqual([3]);
  });

  // The tray must hold exactly the answers still owed — but an alert for a
  // session outside the loaded window, one whose durable read mark has not loaded,
  // or one belonging to a machine that is not answering is not ours to throw away.
  it('drops the delivered alerts of sessions it knows are read, and only those', async () => {
    const { markSessionRead, syncBadge } = await fresh();
    markSessionRead('read', 4);
    markSessionRead('unread', 3);
    markSessionRead('down', 1);
    await syncBadge([
      machine([row('read', 4), row('unread', 5), row('unmarked', 5)]),
      machine([row('down', 9)], 'connection refused'),
    ]);
    const isDone = native.dropped[0];
    expect(isDone('read')).toBe(true);
    expect(isDone('down')).toBe(false);
    expect(isDone('unread')).toBe(false);
    expect(isDone('unmarked')).toBe(false);
    expect(isDone('elsewhere')).toBe(false);
    expect(isDone(undefined)).toBe(false);
  });
});
