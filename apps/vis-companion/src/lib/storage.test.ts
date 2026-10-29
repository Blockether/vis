// Regression: refreshing an open session on the web app showed only
// "missing or invalid bearer token" and a Reconnecting badge. Background upkeep
// (the advertised-address refresh, the address picker and the id backfill)
// wrote partial records through an insert-or-replace. When the saved list no
// longer held that URL (another tab moved or removed the machine), the write
// ADDED a new machine without a token, and every request to it was refused.
// A patch must change a saved machine only, never pair a new one.

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

import { loadConnections, saveConnections, updateConnection } from './storage';

const LOOPBACK = 'http://127.0.0.1:7890';
const LAN = 'http://10.0.0.5:7890';

beforeEach(() => {
  native.store.clear();
  localStorage.clear();
});

describe('updateConnection', () => {
  it('patches the saved machine and keeps its token', async () => {
    await saveConnections([{ url: LOOPBACK, token: 'secret', label: 'mac' }]);

    const conns = await updateConnection(LOOPBACK, { alts: [LOOPBACK, LAN], id: 'gw-1' });

    expect(conns).toEqual([
      { url: LOOPBACK, token: 'secret', label: 'mac', alts: [LOOPBACK, LAN], id: 'gw-1' },
    ]);
    expect(await loadConnections()).toEqual(conns);
  });

  it('never adds a machine that is no longer saved', async () => {
    await saveConnections([{ url: LAN, token: 'secret', label: 'mac' }]);

    const conns = await updateConnection(LOOPBACK, { alts: [LOOPBACK, LAN] });

    expect(conns).toEqual([{ url: LAN, token: 'secret', label: 'mac' }]);
    expect((await loadConnections()).some((conn) => !conn.token)).toBe(false);
  });
});
