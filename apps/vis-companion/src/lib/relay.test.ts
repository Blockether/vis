// Most machines a phone is paired with hold no Apple or Firebase signing key,
// and until the relay existed those machines simply could not buzz it. The relay
// changes that, but only under two rules that are easy to break silently and
// impossible to notice from the UI:
//
//   * a machine that CAN sign its own pushes must never be handed a grant — the
//     relay would see alerts that never had to leave that machine;
//   * a machine that cannot sign must be handed the grant INSTEAD of the token,
//     because a token it cannot use is a device row that never delivers.
//
// These pin both, plus the caching that keeps a launch sweep from being throttled
// by the relay's own mint limit, and the revocation that has to name whichever id
// the machine is actually holding.

import { beforeEach, describe, expect, it, vi } from "vitest";

// `vi.mock` factories run at import time, before module-scope `const`s of this
// file exist — the shared state has to be hoisted with them.
const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock("@capacitor/preferences", () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({
      value: native.store.get(key) ?? null,
    }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
    remove: async ({ key }: { key: string }) => {
      native.store.delete(key);
    },
  },
}));

import {
  PUBLISHER_RELAY_URL,
  grantFor,
  registerForPush,
  registeredIds,
  refusedRelayUrl,
  relayHost,
  relayUrlFor,
  unregisterFromPush,
  type PushGateway,
} from "./relay";
import { getRelayGrant } from "./storage";
import type { PushDeviceInput, PushStatus } from "./types";

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

const RELAY = "https://relay.example.com";
const TOKEN = "a".repeat(64);

const status = (over: Partial<PushStatus> = {}): PushStatus => ({
  is_available: false,
  provider: "relay",
  devices: 1,
  apns: { is_available: false },
  fcm: { is_available: false },
  relay: { is_available: true, url: RELAY, source: "env" },
  ...over,
});

const device = (token = TOKEN): PushDeviceInput => ({
  token,
  platform: "ios",
  environment: "production",
  client: "vis-companion",
});

/** A relay that hands out numbered grants and counts how often it was asked. */
const minter = (expiresAt?: number) => {
  const calls: PushDeviceInput[] = [];
  return {
    calls,
    mint: async (_url: string, input: PushDeviceInput) => {
      calls.push(input);
      return {
        token: String(input.token ?? ""),
        grant: `vg1.grant-${calls.length}`,
        expires_at: expiresAt,
      };
    },
  };
};

/** One gateway, recording exactly what it was asked to file and to drop. */
const gatewayOf = (push: PushStatus) => {
  const registered: PushDeviceInput[] = [];
  const unregistered: string[] = [];
  const gateway: PushGateway = {
    status: async () => push,
    register: async (input) => registered.push(input),
    unregister: async (id) => unregistered.push(id),
  };
  return { gateway, registered, unregistered };
};

beforeEach(() => {
  native.store.clear();
  vi.stubGlobal("localStorage", makeLocalStorage());
});

describe("which machines need a relay", () => {
  it("never routes a machine that can sign its own pushes through one", () => {
    const signing = status({ apns: { is_available: true }, provider: "apns" });
    expect(relayUrlFor(signing, "ios")).toBeNull();
  });

  it("routes a machine with no key of its own to the relay it advertises", () => {
    expect(relayUrlFor(status(), "ios")).toBe(RELAY);
  });

  it("answers per platform, because push has two disjoint halves", () => {
    // Firebase credentials sign for a Pixel and prove nothing for an iPhone.
    const androidOnly = status({
      fcm: { is_available: true },
      provider: "fcm",
    });
    expect(relayUrlFor(androidOnly, "android")).toBeNull();
    expect(relayUrlFor(androidOnly, "ios")).toBe(RELAY);
  });

  it("never routes browser push through the native relay", () => {
    expect(
      relayUrlFor(status({ relay: { is_available: true, url: RELAY } }), "web"),
    ).toBeNull();
  });

  it("falls back to the relay this BUILD was published with", () => {
    // Nobody should have to know an address to be notified. WHICH relay can
    // sign for this app is a property of the app — its publisher owns the topic
    // and the signing key — so a machine that names none is not a machine
    // without push; it is the ordinary case.
    expect(
      relayUrlFor(status({ relay: { is_available: false, url: null } }), "ios"),
    ).toBe(PUBLISHER_RELAY_URL);
    expect(PUBLISHER_RELAY_URL.startsWith("https://")).toBe(true);
    // A gateway we have not heard from yet is not a gateway with no relay.
    expect(relayUrlFor(undefined, "ios")).toBeNull();
  });

  it("refuses a relay offered over plain http, whoever offered it", () => {
    // The gateway names the address this device's push token is handed to.
    const plaintext = status({
      relay: { is_available: true, url: "http://10.0.0.5:8787" },
    });
    expect(relayUrlFor(plaintext, "ios")).toBeNull();
  });

  it("names the http relay a machine offered, instead of blaming its keys", () => {
    // The relay address is configuration on the MACHINE — anyone may run their
    // own — so the one failure this device cannot fix must be reported as an
    // address, not as "missing push credentials".
    const plaintext = status({
      relay: {
        is_available: false,
        url: "http://10.0.0.5:8787",
        is_insecure: true,
      },
    });
    expect(refusedRelayUrl(plaintext, "ios")).toBe("http://10.0.0.5:8787");
    // A machine that can sign for this platform itself is not misconfigured,
    // whatever its relay says, and a relay we accept is not a complaint.
    expect(
      refusedRelayUrl({ ...plaintext, apns: { is_available: true } }, "ios"),
    ).toBeNull();
    expect(refusedRelayUrl(status(), "ios")).toBeNull();
    expect(refusedRelayUrl(undefined, "ios")).toBeNull();
  });

  it("shows a relay by host, so a self-hosted one is recognisable", () => {
    expect(relayHost(RELAY)).toBe("relay.example.com");
    expect(relayHost("https://push.example.com:8443/v1")).toBe(
      "push.example.com:8443",
    );
    expect(relayHost(null)).toBeNull();
  });
});

describe("the grant this device holds", () => {
  it("is minted once and reused by every machine that needs it", async () => {
    // The relay rate-limits minting on purpose; a sweep over several paired
    // machines that minted one grant each would be refused by it.
    const relay = minter();
    const first = await grantFor(RELAY, device(), relay.mint);
    const second = await grantFor(RELAY, device(), relay.mint);
    expect([first, second]).toEqual(["vg1.grant-1", "vg1.grant-1"]);
    expect(relay.calls).toHaveLength(1);
    expect(await getRelayGrant(RELAY)).toMatchObject({
      token: TOKEN,
      grant: "vg1.grant-1",
    });
  });

  it("is re-minted when the OS rotates the push token", async () => {
    const relay = minter();
    await grantFor(RELAY, device(), relay.mint);
    const rotated = await grantFor(RELAY, device("b".repeat(64)), relay.mint);
    expect(rotated).toBe("vg1.grant-2");
    expect(relay.calls[1]?.token).toBe("b".repeat(64));
  });

  it("is renewed a week early, so a rarely opened app never arrives with a dead one", async () => {
    const now = 1_000_000_000_000;
    const nearlyDead = minter(now + 2 * 24 * 60 * 60 * 1000);
    await grantFor(RELAY, device(), nearlyDead.mint, now);
    await grantFor(RELAY, device(), nearlyDead.mint, now);
    expect(nearlyDead.calls).toHaveLength(2);

    native.store.clear();
    const healthy = minter(now + 90 * 24 * 60 * 60 * 1000);
    await grantFor(RELAY, device(), healthy.mint, now);
    await grantFor(RELAY, device(), healthy.mint, now);
    expect(healthy.calls).toHaveLength(1);
  });
});

describe("registering this device with one gateway", () => {
  it("hands a signing machine the token and nothing else", async () => {
    const relay = minter();
    const target = gatewayOf(
      status({ apns: { is_available: true }, provider: "apns" }),
    );
    const identity = await registerForPush(
      device(),
      target.gateway,
      relay.mint,
    );
    expect(identity).toEqual({ kind: "token", value: TOKEN });
    expect(target.registered[0]).toMatchObject({ token: TOKEN });
    expect(target.registered[0]?.grant).toBeUndefined();
    expect(relay.calls).toHaveLength(0);
  });

  it("hands a keyless machine the grant INSTEAD of the token", async () => {
    const relay = minter();
    const target = gatewayOf(status());
    const identity = await registerForPush(
      device(),
      target.gateway,
      relay.mint,
    );
    expect(identity).toEqual({ kind: "grant", value: "vg1.grant-1" });
    expect(target.registered[0]?.grant).toBe("vg1.grant-1");
    // The raw token would be dead weight on a machine with no key to sign it.
    expect(target.registered[0]?.token).toBeUndefined();
    expect(target.registered[0]).toMatchObject({
      platform: "ios",
      environment: "production",
      relay_url: RELAY,
    });
  });

  it("tells the gateway WHICH relay sealed the grant", async () => {
    // A grant is gibberish to every relay but the one that sealed it, so the
    // address belongs to the grant — not to a global setting on a machine that
    // never chose it and cannot know which build the phone is running.
    const relay = minter();
    const target = gatewayOf(
      status({ relay: { is_available: false, url: null } }),
    );
    await registerForPush(device(), target.gateway, relay.mint);
    expect(target.registered[0]).toMatchObject({
      grant: "vg1.grant-1",
      relay_url: PUBLISHER_RELAY_URL,
    });
  });

  it("fails instead of registering a token the machine could never use", async () => {
    const target = gatewayOf(status());
    const refuse = async () => {
      throw new Error("The relay refused a grant for this device (429).");
    };
    await expect(
      registerForPush(device(), target.gateway, refuse),
    ).rejects.toThrow("429");
    expect(target.registered).toHaveLength(0);
  });
});

describe("taking this device off a gateway", () => {
  it("drops it under every name it may be filed under", async () => {
    const relay = minter();
    const keyless = gatewayOf(status());
    await registerForPush(device(), keyless.gateway, relay.mint);

    expect(await registeredIds(TOKEN)).toEqual([TOKEN, "vg1.grant-1"]);

    // The switch is flipped off in a DIFFERENT gateway's settings than the one
    // that got the grant, and neither app nor user knows which name it holds.
    const other = gatewayOf(
      status({ apns: { is_available: true }, provider: "apns" }),
    );
    await unregisterFromPush(TOKEN, other.gateway);
    expect(other.unregistered).toEqual([TOKEN, "vg1.grant-1"]);
  });

  // Regression, user report (one machine kept alerting after Disconnect): the
  // ids were revoked in a loop that stopped at the first refusal, so a machine
  // that answered the token DELETE with an error was left holding the GRANT it
  // was actually pushing with. Every name is tried; the failure is still raised.
  it("drops every other name when one revocation is refused", async () => {
    const relay = minter();
    const keyless = gatewayOf(status());
    await registerForPush(device(), keyless.gateway, relay.mint);

    const dropped: string[] = [];
    const flaky = {
      unregister: async (id: string) => {
        if (id === TOKEN) throw new Error("machine unreachable");
        dropped.push(id);
      },
    };

    await expect(unregisterFromPush(TOKEN, flaky)).rejects.toThrow(
      "machine unreachable",
    );
    expect(dropped).toEqual(["vg1.grant-1"]);
  });
});
