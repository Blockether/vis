// @vitest-environment jsdom
import { describe, expect, it } from "vitest";

import { SessionSubscriptionHub } from "./subscriptions";
import type { GatewayClient } from "./gateway";
import type { SseEvent } from "./types";

/** A client that is nothing but the two streams the hub can open. */
function fakeClient() {
  const state = {
    opened: 0,
    stopped: 0,
    deliver: null as ((event: SseEvent) => void) | null,
  };
  const client = {
    streamFleetStatus(
      onEvent: (event: SseEvent) => void,
      opts: { onOpen?: () => void } = {},
    ) {
      state.opened += 1;
      state.deliver = onEvent;
      opts.onOpen?.();
      return () => {
        state.stopped += 1;
        state.deliver = null;
      };
    },
    streamSessionEvents() {
      return () => {};
    },
  };
  return { state, client: client as unknown as GatewayClient };
}

// Regression, user report in this Vis session (paraphrased: "the companion is slow and
// keeps refreshing"): the list had no push channel of its own, so it re-read its whole
// window on a timer to notice that a run had started or ended. The fleet stream is that
// channel — and a machine must not stream to a list nobody is looking at.
describe("the hub's fleet stream", () => {
  it("runs only while somebody is listening", () => {
    const { state, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    expect(state.opened).toBe(0);

    const stopFirst = hub.subscribeFleet(() => {});
    expect(state.opened).toBe(1);

    // A second listener rides the connection that is already open.
    const stopSecond = hub.subscribeFleet(() => {});
    expect(state.opened).toBe(1);

    stopFirst();
    expect(state.stopped).toBe(0);
    stopSecond();
    expect(state.stopped).toBe(1);

    // ...and the next listener opens it again.
    hub.subscribeFleet(() => {});
    expect(state.opened).toBe(2);
    hub.dispose();
    expect(state.stopped).toBe(2);
  });

  it("hands every frame to its listeners and says whether it is delivering", () => {
    const { state, client } = fakeClient();
    const hub = new SessionSubscriptionHub(client);
    const streaming: boolean[] = [];
    hub.subscribeFleetState((live) => streaming.push(live));
    // Nothing is listening for frames yet, so there is no stream to report.
    expect(streaming).toEqual([false]);

    const seen: SseEvent[] = [];
    const stop = hub.subscribeFleet((event) => seen.push(event));
    expect(streaming).toEqual([false, true]);

    state.deliver?.({ type: "session.status", session_id: "s1", is_live: true });
    expect(seen).toEqual([{ type: "session.status", session_id: "s1", is_live: true }]);

    stop();
    expect(streaming).toEqual([false, true, false]);
    hub.dispose();
  });
});
