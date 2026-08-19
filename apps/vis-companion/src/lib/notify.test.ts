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

import {
  applyGatewayNotify,
  drainPushRevocations,
  syncFleetPush,
  type FleetPush,
} from './notify';
import {
  getGatewayNotify,
  loadConnections,
  pendingRevocations,
  removeConnection,
  saveConnections,
  setGatewayNotify,
  switchConnectionUrl,
  upsertConnection,
} from './storage';
import {
  cachedNotifyVerdict,
  rememberNotifyVerdict,
} from './notify-verdict';
import type { GatewayConn, PushDevice } from './types';

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

/**
 * A fleet whose machines answer with the device rows named, recording every
 * call — so a test can pin not only WHAT the sweep asserted but how many
 * requests that took the fleet.
 */
const fleetOf = (
  holding: Record<string, string[]> = {},
  broken: string[] = [],
  refusing: string[] = [],
) => {
  const calls = {
    read: [] as string[],
    registered: [] as string[],
    unregistered: [] as string[],
  };
  const guard = (conn: GatewayConn) => {
    if (broken.includes(conn.url)) throw new Error('machine unreachable');
  };
  const refuse = (conn: GatewayConn) => {
    if (refusing.includes(conn.url)) throw new Error('machine refused');
  };
  const fleet: FleetPush = {
    read: async (conn) => {
      guard(conn);
      calls.read.push(conn.url);
      return { devices: (holding[conn.url] ?? []).map(device) };
    },
    register: async (conn) => {
      guard(conn);
      calls.registered.push(conn.url);
      refuse(conn);
    },
    unregister: async (conn) => {
      guard(conn);
      calls.unregistered.push(conn.url);
      refuse(conn);
    },
  };
  return { calls, fleet };
};

const device = (preview: string): PushDevice =>
  ({ token_preview: preview, platform: 'ios', is_relayed: false }) as PushDevice;

/** Everything the sweep asked of the fleet, counted. */
const requests = (calls: {
  read: string[];
  registered: string[];
  unregistered: string[];
}) => calls.read.length + calls.registered.length + calls.unregistered.length;

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

describe('syncFleetPush', () => {
  it('registers nothing for a machine that was only paired', async () => {
    const { calls, fleet } = fleetOf();
    const result = await syncFleetPush(paired, fleet, ['mine'], false);
    expect(calls.registered).toEqual([]);
    // ...and takes nothing off a machine that is not holding this device: a
    // DELETE per never-connected machine, per launch, asserted nothing at all.
    expect(calls.unregistered).toEqual([]);
    expect(result.unchanged).toEqual([LAPTOP, BUILDBOX]);
  });

  // Regression, user report (paraphrased: "it cannot be four or five requests
  // per machine — it has to be one"): every launch asked each paired machine
  // for its push status, posted a registration it was already holding, asked
  // for its device list again to warm the row, and the panel asked a fourth
  // time the moment it was opened.
  it('costs a fleet that already agrees ONE request per machine', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    const { calls, fleet } = fleetOf({
      [LAPTOP]: ['mine'],
      [BUILDBOX]: ['mine'],
    });

    const result = await syncFleetPush(paired, fleet, ['mine'], false);

    expect(requests(calls)).toBe(paired.length);
    expect(calls.read).toEqual([LAPTOP, BUILDBOX]);
    expect(result.unchanged).toEqual([LAPTOP, BUILDBOX]);
  });

  it('registers the machine this device connected, and only that one', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { calls, fleet } = fleetOf();
    await syncFleetPush(paired, fleet, ['mine'], false);
    expect(calls.registered).toEqual([LAPTOP]);
    expect(calls.unregistered).toEqual([]);
  });

  it('keeps a silenced machine silenced, and stops asking after once', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, false);
    const first = fleetOf({ [LAPTOP]: ['mine'], [BUILDBOX]: ['mine'] });
    await syncFleetPush(paired, first.fleet, ['mine'], false);
    expect(first.calls.unregistered).toEqual([BUILDBOX]);

    // The next launch finds that machine no longer holding this device, so it
    // is left alone instead of being sent the same DELETE for good.
    const next = fleetOf({ [LAPTOP]: ['mine'] });
    await syncFleetPush(paired, next.fleet, ['mine'], false);
    expect(next.calls.unregistered).toEqual([]);
    expect(requests(next.calls)).toBe(paired.length);
  });

  // A grant carries its own expiry, so agreement is not enough: the machine
  // holding a lapsed one would go quiet on a device that never changed its mind.
  it('re-registers a machine whose relay grant is about to lapse', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { calls, fleet } = fleetOf({ [LAPTOP]: ['mine'] });
    await syncFleetPush(
      [paired[0]],
      { ...fleet, isRenewalDue: async () => true },
      ['mine'],
      false,
    );
    expect(calls.registered).toEqual([LAPTOP]);
  });

  it('registers a machine the app does not currently have open', async () => {
    await setGatewayNotify(BUILDBOX, true);
    const { calls, fleet } = fleetOf();
    await syncFleetPush([paired[1]], fleet, ['mine'], false);
    expect(calls.registered).toEqual([BUILDBOX]);
  });

  it('lets one unreachable machine fail without silencing the rest', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    const { calls, fleet } = fleetOf({}, [LAPTOP]);
    const result = await syncFleetPush(paired, fleet, ['mine'], false);
    expect(result.failed).toEqual([LAPTOP]);
    expect(calls.registered).toEqual([BUILDBOX]);
  });

  it('sweeps each machine once and stops when the app tears the sweep down', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    const { calls, fleet } = fleetOf();
    await syncFleetPush([...paired, { url: LAPTOP }], fleet, ['mine'], false);
    expect(calls.registered).toEqual([LAPTOP, BUILDBOX]);

    const second = fleetOf();
    await syncFleetPush(paired, second.fleet, ['mine'], false, () => true);
    expect(requests(second.calls)).toBe(0);
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
    const { calls, fleet } = fleetOf({ [BUILDBOX]: ['mine'] });
    await syncFleetPush(paired, fleet, ['mine'], false);
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

// Regression, user report ("I removed the binding from the iOS phone but it is
// still notifying me"): forgetting a machine deleted the pairing and nothing
// else. A device row lives on the MACHINE, so that machine went on pushing —
// and the forget was the very thing that made it permanent, because the sweep
// only walks machines that are still paired and nothing was left that could
// name the one that had been dropped.
describe('forgetting a machine', () => {
  it('takes this device off the machine that was forgotten', async () => {
    await saveConnections(paired);
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    await removeConnection(BUILDBOX);
    const { calls, fleet } = fleetOf();
    // A relaunch: the sweep over what is still paired, then the revocations
    // this device still owes.
    await syncFleetPush(await loadConnections(), fleet, ['mine'], false);
    await drainPushRevocations('tok', fleet.unregister);
    expect(calls.registered).toEqual([LAPTOP]);
    expect(calls.unregistered).toEqual([BUILDBOX]);
    expect(await pendingRevocations()).toEqual([]);
  });

  it('revokes with the credential the machine was paired with', async () => {
    await saveConnections(paired);
    await removeConnection(BUILDBOX);
    const seen: GatewayConn[] = [];
    await drainPushRevocations('tok', async (conn) => {
      seen.push(conn);
    });
    expect(seen).toEqual([{ url: BUILDBOX, token: 'b' }]);
  });

  it('keeps owing the revocation until that machine accepts it', async () => {
    await saveConnections(paired);
    await removeConnection(BUILDBOX);
    const unreachable = fleetOf({}, [BUILDBOX]);
    await drainPushRevocations('tok', unreachable.fleet.unregister);
    expect((await pendingRevocations()).map((c) => c.url)).toEqual([BUILDBOX]);
    const back = fleetOf();
    await drainPushRevocations('tok', back.fleet.unregister);
    expect(back.calls.unregistered).toEqual([BUILDBOX]);
    expect(await pendingRevocations()).toEqual([]);
  });

  it('owes nothing for a machine that was paired again', async () => {
    await saveConnections(paired);
    await removeConnection(BUILDBOX);
    await upsertConnection({ url: BUILDBOX, token: 'b' });
    const { calls, fleet } = fleetOf();
    await drainPushRevocations('tok', fleet.unregister);
    expect(calls.unregistered).toEqual([]);
    expect(await pendingRevocations()).toEqual([]);
  });
});

// Regression, user report ("the notification thing flickers every time, on every
// machine I am connected to — can it not fetch the settings for all of them
// before I tap one?"): the verdict was only remembered once a machine's own
// Notifications panel had settled, so the FIRST open of every paired machine —
// and every open after a device that had never been there — still painted a
// pulsing amber `Connect` labelled `Checking…` before settling.
describe('the verdict a row opens on', () => {
  it('answers a machine before its panel is ever opened', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { calls, fleet } = fleetOf({ [LAPTOP]: ['mine'] });

    await syncFleetPush([{ url: LAPTOP, token: 'a' }], fleet, ['mine'], false);

    expect(cachedNotifyVerdict(LAPTOP)).toBe(true);
    expect(requests(calls)).toBe(1);
  });

  it('says no for a machine that would not take this device', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { fleet } = fleetOf({ [LAPTOP]: ['someone-else'] }, [], [LAPTOP]);

    const result = await syncFleetPush(
      [{ url: LAPTOP, token: 'a' }],
      fleet,
      ['mine'],
      false,
    );

    expect(result.failed).toEqual([LAPTOP]);
    expect(cachedNotifyVerdict(LAPTOP)).toBe(false);
  });

  it('answers each paired machine from its own switch', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { calls, fleet } = fleetOf({ [LAPTOP]: ['mine'] });

    await syncFleetPush(paired, fleet, ['mine'], false);

    expect(cachedNotifyVerdict(LAPTOP)).toBe(true);
    expect(cachedNotifyVerdict(BUILDBOX)).toBe(false);
    expect(requests(calls)).toBe(paired.length);
  });

  it('leaves an unreachable machine with the verdict it settled on', async () => {
    rememberNotifyVerdict(LAPTOP, true);
    await setGatewayNotify(LAPTOP, true);
    const { fleet } = fleetOf({}, [LAPTOP]);

    const result = await syncFleetPush(
      [{ url: LAPTOP, token: 'a' }],
      fleet,
      ['mine'],
      false,
    );

    expect(result.failed).toEqual([LAPTOP]);
    expect(cachedNotifyVerdict(LAPTOP)).toBe(true);
  });

  it('needs no round trip at all once the OS has silenced this app', async () => {
    await setGatewayNotify(LAPTOP, true);
    const { calls, fleet } = fleetOf({
      [LAPTOP]: ['mine'],
      [BUILDBOX]: ['mine'],
    });

    await syncFleetPush(paired, fleet, ['mine'], true);

    expect(requests(calls)).toBe(0);
    expect(cachedNotifyVerdict(LAPTOP)).toBe(false);
    expect(cachedNotifyVerdict(BUILDBOX)).toBe(false);
  });

  it('stops where the effect was torn down', async () => {
    await setGatewayNotify(LAPTOP, true);
    await setGatewayNotify(BUILDBOX, true);
    const { calls, fleet } = fleetOf({ [LAPTOP]: ['mine'] });

    await syncFleetPush(paired, fleet, ['mine'], false, () => true);

    expect(requests(calls)).toBe(0);
    expect(cachedNotifyVerdict(LAPTOP)).toBeNull();
  });
});
