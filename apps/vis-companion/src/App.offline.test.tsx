// @vitest-environment jsdom
// Regression, TestFlight feedback (build 2823, 2026-07-31): "Screen keeps
// blinking when cannot connect to gateway, no way to manage connections".
// A dead gateway made every failed request report "unreachable" again, which
// kicked another recovery sweep, which pinged every address again — ~300
// fetches per second that pinned the WebKit process and left the shell
// flickering on a session list that never arrived, with no screen to pair or
// repair a machine from.
import { screen, waitFor } from "@testing-library/react";
import { describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";

const DEAD = "http://192.168.0.241:7890";

describe("the app against a gateway that answers nothing", () => {
  it("offers the machines screen instead of storming the dead address", async () => {
    const view = renderApp({
      machines: [{ label: "tower", url: DEAD }],
      unreachable: [DEAD],
    });
    const inner = globalThis.fetch;
    let calls = 0;
    globalThis.fetch = ((...args: Parameters<typeof fetch>) => {
      calls += 1;
      return inner(...args);
    }) as typeof fetch;
    try {
      // There IS a way to manage connections: the pairing screen, with the
      // machine on it and the way to add another.
      await waitFor(() => expect(screen.getByText("Add a machine")).toBeTruthy());
      expect(screen.getAllByText("tower").length).toBeGreaterThan(0);

      // And the recovery loop is bounded: a second of a dead gateway costs a
      // handful of requests, not hundreds.
      calls = 0;
      await new Promise((resolve) => setTimeout(resolve, 1_000));
      expect(calls).toBeLessThan(40);
    } finally {
      globalThis.fetch = inner;
      view.unmount();
      view.restore();
    }
  });
});
