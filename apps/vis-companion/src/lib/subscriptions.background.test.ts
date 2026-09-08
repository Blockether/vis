// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import type { GatewayClient } from "./gateway";
import type { SseEvent } from "./types";

const native = vi.hoisted(
  () => new Map<string, (state?: { isActive: boolean }) => void>(),
);

vi.mock("@capacitor/app", () => ({
  App: {
    addListener: (
      event: string,
      listener: (state?: { isActive: boolean }) => void,
    ) => {
      native.set(event, listener);
      return Promise.resolve({ remove: () => void native.delete(event) });
    },
  },
}));

function fakeClient() {
  const state = {
    sessionOpened: 0,
    sessionStopped: 0,
    fleetOpened: 0,
    fleetStopped: 0,
  };
  const stoppable = (kind: "session" | "fleet") => {
    let stopped = false;
    return () => {
      if (stopped) return;
      stopped = true;
      state[`${kind}Stopped`] += 1;
    };
  };
  const client = {
    streamSessionEvents(
      _cursors: Map<string, number>,
      _onEvent: (event: SseEvent) => void,
      opts: { onOpen?: () => void } = {},
    ) {
      state.sessionOpened += 1;
      opts.onOpen?.();
      return stoppable("session");
    },
    streamFleetStatus(
      _onEvent: (event: SseEvent) => void,
      opts: { onOpen?: () => void } = {},
    ) {
      state.fleetOpened += 1;
      opts.onOpen?.();
      return stoppable("fleet");
    },
  };
  return { state, client: client as unknown as GatewayClient };
}

beforeEach(() => {
  vi.useFakeTimers();
  native.clear();
});

afterEach(() => {
  vi.useRealTimers();
  vi.resetModules();
});

describe("multiplexed session subscriptions", () => {
  it("shares the stream across views and keeps replay when a view remounts", async () => {
    const { SessionSubscriptionHub } = await import("./subscriptions");
    const { state, client } = fakeClient();
    const stream = vi.spyOn(client, "streamSessionEvents");
    const hub = new SessionSubscriptionHub(client);
    try {
      hub.watchSessions(["session-1", "session-2"]);
      const first = vi.fn();
      const second = vi.fn();
      const stopFirst = hub.subscribeSession("session-1", first);
      const stopSecond = hub.subscribeSession("session-2", second);
      expect(stream).toHaveBeenCalledTimes(1);
      const [cursors, emit] = stream.mock.calls[0]!;
      expect([...cursors]).toEqual([
        ["session-1", -1],
        ["session-2", -1],
      ]);
      const event: SseEvent = {
        type: "turn.started",
        session_id: "session-1",
        seq: 10,
      };
      emit(event);
      expect(first).toHaveBeenCalledExactlyOnceWith(event);
      expect(second).not.toHaveBeenCalled();
      stopFirst();
      const remounted = vi.fn();
      const stopRemounted = hub.subscribeSession("session-1", remounted);
      expect(remounted).toHaveBeenCalledExactlyOnceWith(event);
      stopRemounted();
      stopSecond();
      expect(state.sessionStopped).toBe(0);
      expect(stream).toHaveBeenCalledTimes(1);
    } finally {
      hub.dispose();
    }
    expect(state.sessionStopped).toBe(1);
    await vi.advanceTimersByTimeAsync(30_000);
    expect(stream).toHaveBeenCalledTimes(1);
  });
});

// Regression, Vis session 1bd4284d-861b-48e6-8639-ef8eafb22f0a: killing the gateway
// while the app was backgrounded left WebKit holding both fetch streams; after resume,
// opening or creating another session waited behind those parked sockets until restart.
describe("gateway streams across native backgrounding", () => {
  it("retires them before suspension and opens fresh streams after resume", async () => {
    const { SessionSubscriptionHub } = await import("./subscriptions");
    const { state, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    hub.watchSessions(["session-1"]);
    hub.subscribeFleet(() => {});
    expect(state).toEqual({
      sessionOpened: 1,
      sessionStopped: 0,
      fleetOpened: 1,
      fleetStopped: 0,
    });

    native.get("appStateChange")?.({ isActive: false });
    expect(state.sessionStopped).toBe(1);
    expect(state.fleetStopped).toBe(1);

    // The supervisor must not undo the retirement while the app is still away.
    await vi.advanceTimersByTimeAsync(30_000);
    expect(state.sessionOpened).toBe(1);
    expect(state.fleetOpened).toBe(1);

    native.get("appStateChange")?.({ isActive: true });
    await vi.advanceTimersByTimeAsync(250);
    expect(state.sessionOpened).toBe(2);
    expect(state.fleetOpened).toBe(2);

    hub.dispose();
  });
});
