// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { screen } from "@testing-library/react";

import { renderSessionScreen } from "./session-screen-harness";

// Reported from a phone: the message was sent and the session title updated, but
// the answer rail stayed a bare "Vis" — no phase, no clock, no trace — for the
// whole turn. A running turn was adopted ONLY from the session read, so that one
// failed request left `running` false and the live bubble null: the freshly
// persisted `running` row was then painted as a finished one, and every delta
// that arrived had no bubble to land in.
describe("a running turn the session read cannot confirm", () => {
  const runningRow = {
    id: "t1",
    user_request: "check the logs",
    status: "running",
    created_at: Date.now(),
    iterations: [],
  };

  it("reports the work the transcript says is under way", async () => {
    renderSessionScreen({
      client: {
        // The registry read is the request that fails on a phone's link; the
        // transcript is the witness that does not go through the registry.
        session: () => Promise.reject(new Error("network down")),
        transcript: () => Promise.resolve([runningRow]),
      },
      subscriptions: {
        subscribeConnection: (on: (live: boolean) => void) => {
          on(true);
          return () => {};
        },
      },
    });

    expect(await screen.findByText("check the logs")).toBeInTheDocument();
    expect(
      (await screen.findAllByText(/Vis is waiting for an update/)).length,
    ).toBeGreaterThan(0);
  });
});
