// @vitest-environment jsdom
// Regression, TestFlight feedback (build 2823, 2026-07-31): "Screen keeps
// blinking when cannot connect to gateway, no way to manage connections".
// A gateway that answered nothing left the shell flickering on a session list
// that never arrived, with no screen to pair or repair a machine from.
import { screen, waitFor } from "@testing-library/react";
import { describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";

const DEAD = "http://192.168.0.241:7890";

describe("the app against a gateway that answers nothing", () => {
  it("parks on the machines screen, where connections can still be managed", async () => {
    const view = renderApp({
      machines: [{ label: "tower", url: DEAD }],
      unreachable: [DEAD],
    });
    try {
      // The pairing screen IS the way out: the dead machine is listed (its
      // settings reachable) and another one can be added.
      await waitFor(() =>
        expect(screen.getByText("Add a machine")).toBeTruthy(),
      );
      expect(screen.getAllByText("tower").length).toBeGreaterThan(0);
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
