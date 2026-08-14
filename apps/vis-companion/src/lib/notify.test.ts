// Notifications are a PER-GATEWAY decision, and the thing that made them feel
// broken was that the decision did not survive: the app re-registered this
// device's push token with whichever gateway it had open, so a machine you
// silenced started buzzing again after the next launch, and a machine you did
// want stayed quiet because it was never the open one.
//
// These pin the durable half of that contract: nothing alerts this device until
// that machine's own Connect is pressed, each paired machine carries its own
// answer, the answer follows the machine across an address change, and one
// unreachable machine never decides for the rest.

import { beforeEach, describe, expect, it, vi } from 'vitest';

// `vi.mock` factories run at import time, before module-scope `const`s of this
// file exist — the shared state has to be hoisted with them.
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

import { applyGatewayNotify, syncPushRegistrations, type PushRegistrar } from './notify';
import {
  getGatewayNotify,
  removeConnection,
  saveConnections,
  setGatewayNotify,
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

/** A registrar that records every call, and can fail for chosen gateways. */
const recorder = (broken: string[] = []) => {
  const calls = { registered: [] as string[], unregistered: [] as string[] };
  const guard = (conn: GatewayConn) => {
    if (broken.includes(conn.url)) throw new Error('machine unreachable');
  };
  const registrar: PushRegistrar = {
    register: async (conn) => {
      guard(conn);
      calls.registered.push(conn.url);
    },
    unregister: async (conn) => {
      guard(conn);
      calls.unregistered.push(conn.url);
    },
  };
  return { calls, registrar };
};

beforeEach(() => {
  native.store.clear();
  globalThis.localStorage = makeLocalStorage();
});

// Regression, user report ("I didn't have to click connect after I paired with
// another machine, which is wrong — notifications should be per machine"): a
// machine with no answer of its own counted as YES, so pairing a second machine
// handed it this device's push token on the very next sweep and its panel opened
// already Connected. Silence is the only honest default — a machine you paired is
// not yet a machine you asked to be buzzed by — and it is also what makes the
// other half of that report impossible: disconnecting the machine you HAD
// connected can no longer leave a never-connected one still alerting.
describe('per-gateway notification switch', () => {
  it('says no for a machine whose Connect was never pressed', async () => {
    expect(await getGatewayNotify(LAPTOP)).toBe(false);
  });

  it('connects one machine without connecting the fleet', async () => {
    await setGatewayNotify(LAPTOP, true);
    expect(await getGatewayNotify(LAPTOP)).toBe(true);
    expect(await getGatewayNotify(BUILDBOX)).toBe(false);
  });

  it('remembers an explicit off for that machine alone', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, false);
    expect(await getGatewayNotify(BUILDBOX)).toBe(false);
    expect(await getGatewayNotify(LAPTOP)).toBe(true);
  });

  it('follows a machine that moves to another address', async () => {
    await saveConnections(paired);
    await setGatewayNotify(BUILDBOX, true);
    await switchConnectionUrl(BUILDBOX, 'http://buildbox.tail:7890');
    expect(await getGatewayNotify('http://buildbox.tail:7890')).toBe(true);
    expect(await getGatewayNotify(BUILDBOX)).toBe(false);
  });

  it('is forgotten with the machine, so re-pairing starts fresh', async () => {
    await saveConnections(paired);
    await setGatewayNotify(BUILDBOX, true);
    await removeConnection(BUILDBOX);
    expect(await getGatewayNotify(BUILDBOX)).toBe(false);
  });
});

describe('syncPushRegistrations', () => {
  it('registers nothing for a machine that was only paired', async () => {
    const { calls, registrar } = recorder();
    const result = await syncPushRegistrations(paired, 'tok', registrar);
    expect(calls.registered).toEqual([]);
    expect(result.registered).toEqual([]);
    expect(calls.unregistered).toEqual([LAPTOP, BUILDBOX]);
  });

  it('registers the machines this device connected, and only those', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { calls, registrar } = recorder();
    await syncPushRegistrations(paired, 'tok', registrar);
    expect(calls.registered).toEqual([LAPTOP]);
    expect(calls.unregistered).toEqual([BUILDBOX]);
  });

  it('keeps a silenced machine silenced across relaunches', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, false);
    for (const _ of [1, 2]) {
      const { calls, registrar } = recorder();
      await syncPushRegistrations(paired, 'tok', registrar);
      expect(calls.registered).toEqual([LAPTOP]);
      expect(calls.unregistered).toEqual([BUILDBOX]);
    }
  });

  it('registers a machine the app does not currently have open', async () => {
    await setGatewayNotify(BUILDBOX, true);
    const { calls, registrar } = recorder();
    await syncPushRegistrations([paired[1]], 'tok', registrar);
    expect(calls.registered).toEqual([BUILDBOX]);
  });

  it('lets one unreachable machine fail without silencing the rest', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    const { calls, registrar } = recorder([LAPTOP]);
    const result = await syncPushRegistrations(paired, 'tok', registrar);
    expect(result.failed).toEqual([LAPTOP]);
    expect(calls.registered).toEqual([BUILDBOX]);
  });

  it('sweeps each machine once and stops when the app tears the sweep down', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    const { calls, registrar } = recorder();
    await syncPushRegistrations([...paired, { url: LAPTOP }], 'tok', registrar);
    expect(calls.registered).toEqual([LAPTOP, BUILDBOX]);

    const second = recorder();
    await syncPushRegistrations(paired, 'tok', second.registrar, () => true);
    expect(second.calls.registered).toEqual([]);
  });
});

// The answer is the durable half: a machine that is unreachable exactly when you
// silence it must still end up silenced.
describe('applyGatewayNotify', () => {
  const unreachable = () => Promise.reject(new Error('machine unreachable'));

  it('stores the stop even when that machine cannot be reached', async () => {
    await setGatewayNotify(LAPTOP, true);
    await expect(applyGatewayNotify(BUILDBOX, false, unreachable)).rejects.toThrow(
      'machine unreachable',
    );
    expect(await getGatewayNotify(BUILDBOX)).toBe(false);

    // ...and the next sweep is what finally lands it on that machine.
    const { calls, registrar } = recorder();
    await syncPushRegistrations(paired, 'tok', registrar);
    expect(calls.unregistered).toEqual([BUILDBOX]);
    expect(calls.registered).toEqual([LAPTOP]);
  });

  it('stores a start that failed, so the sweep retries it', async () => {
    await setGatewayNotify(LAPTOP, false);
    await expect(applyGatewayNotify(LAPTOP, true, unreachable)).rejects.toThrow(
      'machine unreachable',
    );
    expect(await getGatewayNotify(LAPTOP)).toBe(true);
  });

  it('stores the answer before asking the gateway, never after', async () => {
    const seen: boolean[] = [];
    await applyGatewayNotify(BUILDBOX, false, async () => {
      seen.push(await getGatewayNotify(BUILDBOX));
    });
    expect(seen).toEqual([false]);
  });
});
