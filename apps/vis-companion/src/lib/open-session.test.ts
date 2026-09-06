// The transcript a cold start may reopen is ONE pointer per device, written when a
// transcript is entered and cleared when the user leaves it — never inferred from the
// per-gateway subscription list, which every visited session joins and none leaves.
// These pin the durable half: the pointer survives a relaunch, follows its machine
// across an address change, dies with the machine, and a broken record reads as none.

import { beforeEach, describe, expect, it, vi } from 'vitest';

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

import {
  forgetOpenSession,
  loadOpenSession,
  rememberOpenSession,
  removeConnection,
  saveConnections,
  switchConnectionUrl,
} from './storage';
import type { GatewayConn } from './types';

const makeLocalStorage = () => {
  const map = new Map<string, string>();
  return {
    getItem: (key: string) => map.get(key) ?? null,
    setItem: (key: string, value: string) => {
      map.set(key, value);
    },
    removeItem: (key: string) => {
      map.delete(key);
    },
    clear: () => {
      map.clear();
    },
    key: (index: number) => [...map.keys()][index] ?? null,
    get length() {
      return map.size;
    },
  } as unknown as Storage;
};

const LAPTOP = 'http://10.0.0.5:7890';
const BUILDBOX = 'http://10.0.0.6:7890';
const paired: GatewayConn[] = [
  { url: LAPTOP, token: 'a' },
  { url: BUILDBOX, token: 'b' },
];

beforeEach(() => {
  native.store.clear();
  globalThis.localStorage = makeLocalStorage();
});

describe('the transcript a cold start may reopen', () => {
  it('is nothing until a transcript is entered, and the last one entered after', async () => {
    expect(await loadOpenSession()).toBeNull();
    const before = Date.now();
    await rememberOpenSession(LAPTOP, 's1');
    await rememberOpenSession(BUILDBOX, 's2');
    const open = await loadOpenSession();
    expect(open).toMatchObject({ url: BUILDBOX, sid: 's2' });
    expect(open!.at).toBeGreaterThanOrEqual(before);
  });

  it('is gone once the user leaves the transcript', async () => {
    await rememberOpenSession(LAPTOP, 's1');
    await forgetOpenSession();
    expect(await loadOpenSession()).toBeNull();
  });

  it('follows its machine to another address', async () => {
    await saveConnections(paired);
    await rememberOpenSession(BUILDBOX, 's2');
    await switchConnectionUrl(BUILDBOX, 'http://buildbox.tail:7890');
    expect(await loadOpenSession()).toMatchObject({ url: 'http://buildbox.tail:7890', sid: 's2' });
  });

  it('dies with its machine', async () => {
    await saveConnections(paired);
    await rememberOpenSession(BUILDBOX, 's2');
    await removeConnection(BUILDBOX);
    expect(await loadOpenSession()).toBeNull();
  });

  it('reads a broken record as none', async () => {
    native.store.set('vis.openSession', '{"url":"x"}');
    expect(await loadOpenSession()).toBeNull();
    native.store.set('vis.openSession', 'not json');
    expect(await loadOpenSession()).toBeNull();
  });
});
