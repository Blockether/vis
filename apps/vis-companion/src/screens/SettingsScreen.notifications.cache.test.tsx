// @vitest-environment jsdom
// Regression, user report ("when I open settings they always flicker"): the
// Notifications row assembles its verdict from four asynchronous answers, so
// every single open of the settings dialog painted a pulsing amber `Connect`
// labelled `Checking…` first and settled into a quiet `Disconnect` a moment
// later — on a machine this device had been connected to for days.
import { render, screen } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

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
  registerPlugin: () => ({
    speak: () => Promise.resolve(),
    stop: () => Promise.resolve(),
    getVoices: () => Promise.resolve({ voices: [] }),
  }),
}));
vi.mock("@capacitor/push-notifications", () => ({ PushNotifications: {} }));

vi.mock("../lib/push", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../lib/push")>()),
  cachedPushToken: () => "device-token",
  pushPermission: async () => "granted" as const,
}));

import { NativeNotificationsPanel } from "./SettingsScreen";
import { maskToken } from "../lib/push";
import {
  cachedNotifyVerdict,
  rememberNotifyVerdict,
} from "../lib/notify-verdict";
import type { GatewayClient } from "../lib/gateway";
import type { PushStatus } from "../lib/types";

const MACHINE = "http://10.0.0.5:7890";

const signing: PushStatus = {
  is_available: true,
  provider: "apns",
  devices: 1,
  apns: { is_available: true },
  fcm: { is_available: false },
};

const held = {
  devices: [
    {
      token_preview: maskToken("device-token"),
      platform: "ios",
      is_relayed: false,
    },
  ],
  push: signing,
};

/** A machine that answers, but never within the frame under test. */
const slowMachine = (cached: typeof held | null) =>
  ({
    cachedDevices: () => cached,
    devices: () => new Promise(() => undefined),
    pushTarget: () => ({
      status: async () => signing,
      register: async () => undefined,
      unregister: async () => undefined,
    }),
  }) as unknown as GatewayClient;

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

const open = (cached: typeof held | null) =>
  render(
    <NativeNotificationsPanel
      client={slowMachine(cached)}
      gateway={{ url: MACHINE, label: "buildbox" }}
    />,
  );

beforeEach(() => {
  native.store.clear();
  globalThis.localStorage = makeLocalStorage();
});

afterEach(() => {
  document.body.innerHTML = "";
});

describe("reopening the notifications panel", () => {
  it("paints the verdict it settled on last time, with no Checking frame", () => {
    rememberNotifyVerdict(MACHINE, true);

    open(held);

    expect(
      screen.getByRole("button", {
        name: "Disconnect notifications from buildbox",
      }),
    ).toBeTruthy();
    expect(screen.queryByText("Checking…")).toBeNull();
  });

  it("still asks where this device has never been told", () => {
    open(null);

    expect(screen.getByText("Checking…")).toBeTruthy();
  });

  it("remembers a machine this device is NOT connected to", async () => {
    rememberNotifyVerdict(MACHINE, true);
    expect(cachedNotifyVerdict(MACHINE)).toBe(true);

    rememberNotifyVerdict(MACHINE, false);

    expect(cachedNotifyVerdict(MACHINE)).toBe(false);
  });
});
