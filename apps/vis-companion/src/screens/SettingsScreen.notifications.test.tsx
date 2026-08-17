// @vitest-environment jsdom
// The Notifications panel commits ONE verb to ONE machine, and Disconnect is the
// half that has to survive everything: it is pressed exactly when a machine is
// buzzing, and this device is not always holding in memory the name that machine
// filed it under. A machine with no signing key of its own knows this device by
// the relay GRANT it minted on some earlier launch — never by the OS push token
// this run may not have been given.
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

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

vi.mock("@capacitor/core", () => ({
  Capacitor: {
    getPlatform: () => "ios",
    isNativePlatform: () => true,
    isPluginAvailable: () => true,
  },
  // The settings dialog now reaches the device's speech engine, which registers a
  // Capacitor plugin at import time.
  registerPlugin: () => ({
    speak: () => Promise.resolve(),
    stop: () => Promise.resolve(),
    getVoices: () => Promise.resolve({ voices: [] }),
  }),
}));
vi.mock("@capacitor/push-notifications", () => ({ PushNotifications: {} }));

// The OS handed this run no token: the launch sweep asked before this panel was
// opened, or it timed out, or the permission was granted after it ran.
vi.mock("../lib/push", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../lib/push")>()),
  cachedPushToken: () => null,
  pushPermission: async () => "granted" as const,
}));

import { NativeNotificationsPanel } from "./SettingsScreen";
import { maskToken } from "../lib/push";
import { getGatewayNotify, setGatewayNotify, setRelayGrant } from "../lib/storage";
import type { GatewayClient } from "../lib/gateway";
import type { PushStatus } from "../lib/types";

const MACHINE = "http://10.0.0.5:7890";
const RELAY = "https://relay.example.com";
const GRANT = "vg1.this-devices-own-grant";

/** A machine with no signing key: it wakes this device through the relay. */
const relayed: PushStatus = {
  is_available: false,
  provider: "relay",
  devices: 1,
  apns: { is_available: false },
  fcm: { is_available: false },
  relay: { is_available: true, url: RELAY, source: "env" },
};

/** That machine, reduced to the two calls this panel makes of it. */
const machine = () => {
  const unregistered: string[] = [];
  const client = {
    cachedDevices: () => null,
    devices: async () => ({
      devices: [
        { token_preview: maskToken(GRANT), platform: "ios", is_relayed: true },
      ],
      push: relayed,
    }),
    pushTarget: () => ({
      status: async () => relayed,
      register: async () => undefined,
      unregister: async (id: string) => {
        unregistered.push(id);
      },
    }),
  } as unknown as GatewayClient;
  return { client, unregistered };
};

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

/** This device connected to that machine earlier, and it is registered there. */
const connected = async () => {
  await setRelayGrant(RELAY, { token: "", grant: GRANT });
  await setGatewayNotify(MACHINE, true);
  const { client, unregistered } = machine();
  render(
    <NativeNotificationsPanel
      client={client}
      gateway={{ url: MACHINE, label: "buildbox" }}
    />,
  );
  const verb = await screen.findByRole("button", {
    name: "Disconnect notifications from buildbox",
  });
  return { unregistered, verb };
};

beforeEach(() => {
  native.store.clear();
  globalThis.localStorage = makeLocalStorage();
});

afterEach(() => {
  document.body.innerHTML = "";
});

// Regression, user report ("I clicked unsubscribe and still I got notifications
// from one of the machines"): Disconnect gave up the moment this device held no
// cached push token, so on the machine that had filed it under a relay grant the
// press did nothing at all -- nothing revoked, and no stored answer for the next
// sweep to land, leaving that one machine alerting forever.
describe("disconnecting a machine that knows this device by its relay grant", () => {
  it("revokes the name that machine is actually holding", async () => {
    const { unregistered, verb } = await connected();

    fireEvent.click(verb);

    await waitFor(() => expect(unregistered).toEqual([GRANT]));
  });

  it("stores this device's answer, so the next sweep keeps it silent", async () => {
    const { verb } = await connected();

    fireEvent.click(verb);

    await waitFor(async () =>
      expect(await getGatewayNotify(MACHINE)).toBe(false),
    );
  });
});
