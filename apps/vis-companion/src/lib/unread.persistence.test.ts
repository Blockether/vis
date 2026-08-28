// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";

import type { Session } from "./types";

const native = vi.hoisted(() => ({ store: new Map<string, string>() }));

vi.mock("@capacitor/preferences", () => ({
  Preferences: {
    get: async ({ key }: { key: string }) => ({ value: native.store.get(key) ?? null }),
    set: async ({ key, value }: { key: string; value: string }) => {
      native.store.set(key, value);
    },
  },
}));

const fresh = async () => {
  vi.resetModules();
  return import("./unread");
};

const session = {
  id: "unread-after-restart",
  status: "idle",
  turn_count: 3,
} satisfies Session;

const settle = () => new Promise((done) => setTimeout(done, 0));

beforeEach(() => {
  native.store.clear();
  localStorage.clear();
});

// Regression, issue #3e2b5725-dbf9-4254-8a10-ec4bb74c936a: restarting the
// native app lost its read marks, so answers not yet opened stopped showing as NEW.
describe("durable session read marks", () => {
  it("keeps unread answers unread after the webview store is reset", async () => {
    const firstLaunch = await fresh();
    firstLaunch.markSessionRead(session.id, 1);
    await settle();
    expect(firstLaunch.unreadTurnCount(session)).toBe(2);

    localStorage.clear();
    const restarted = await fresh();
    await restarted.seedReadMarks([session]);

    expect(restarted.unreadTurnCount(session)).toBe(2);
  });
});
