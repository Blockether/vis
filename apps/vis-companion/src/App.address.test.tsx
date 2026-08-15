// @vitest-environment jsdom
// Regression, user report ("recently there is problem with the connections with
// the Tailscale"): a machine bound BY NAME to its LAN address kept that pin when
// the app had to move ITSELF off it. Away from that Wi-Fi the pinned address
// answered nothing, the app failed over to the tailnet address the gateway also
// serves — and carried the pin onto it, so the row went on saying `Pinned` about
// an address nobody had picked, and the pin (which outranks the durability order)
// froze the app there against every later address.
import { waitFor } from "@testing-library/react";
import { describe, expect, it } from "vitest";

import { renderApp } from "./app-harness";
import type { GatewayConn } from "./lib/types";

const LAN = "http://192.168.0.241:7890";
const TAILSCALE = "http://100.64.0.10:7890";

/** The pairing as this device has it saved, after the app has had its way. */
const saved = (): GatewayConn[] =>
  JSON.parse(localStorage.getItem("vis.connections") ?? "[]");

describe("the address the app moves itself onto", () => {
  it("releases a pin it could not honour", async () => {
    const view = renderApp({
      machines: [{ label: "tower", url: LAN, alts: [TAILSCALE], pinned: true }],
      unreachable: [LAN],
    });
    try {
      await waitFor(() => {
        // The machine travels whole — same token, same name, both addresses
        // still known — and arrives on the tailnet address unpinned.
        expect(saved()).toEqual([
          expect.objectContaining({
            url: TAILSCALE,
            label: "tower",
            token: "t",
            alts: [TAILSCALE, LAN],
            pinned: false,
          }),
        ]);
      });
    } finally {
      view.unmount();
      view.restore();
    }
  });
});
