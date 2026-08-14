// @vitest-environment jsdom
// Regression, user report ("Addresses should be also simplified and in slide"):
// the ADDRESS panel answered one question — which route does this device take to
// this machine — with four blocks of prose (a 110-character band description, a
// hint sentence under the row in use, a paragraph under the list and an
// `Automatic` button beside it) and painted the word USE on every row. Each row
// was a button, so the address the app talks to was one stray tap from changing,
// and the row in use was that same button DISABLED, greyed to half ink.
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

// Nothing here is about the network: every address answers, so the ROWS are the
// whole question.
vi.mock("../lib/gateway", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../lib/gateway")>()),
  GatewayClient: class {
    ping() {
      return Promise.resolve(true);
    }
  },
}));

import { AddressPanel } from "./SettingsScreen";
import type { GatewayConn } from "../lib/types";

const TAILSCALE = "http://100.64.0.10:7890";
const LAN = "http://192.168.0.5:7890";
const LOOPBACK = "http://127.0.0.1:7890";

afterEach(() => {
  document.body.innerHTML = "";
});

/** The panel on a machine that answers on all three of its addresses. */
const panel = async (conn: Partial<GatewayConn> = {}) => {
  const chosen: Array<[string, boolean]> = [];
  const gateway: GatewayConn = {
    url: LAN,
    token: "t",
    alts: [TAILSCALE, LOOPBACK],
    ...conn,
  };
  const { container } = render(
    <AddressPanel
      gateway={gateway}
      onSelect={(url, pinned) => {
        chosen.push([url, pinned]);
      }}
    />,
  );
  await waitFor(() =>
    expect(container.querySelectorAll("[data-swipe-track]")).toHaveLength(3),
  );
  return { chosen, container };
};

/** The captions of one row's slide, addressed by the machine-facing name. */
const verbs = (container: HTMLElement, host: string) =>
  [
    ...(container
      .querySelector(`[role="group"][aria-label="${host} actions"]`)
      ?.querySelectorAll("button") ?? []),
  ].map((b) => ({ caption: b.textContent ?? "", name: b.getAttribute("aria-label") ?? "" }));

describe("the address list", () => {
  it("keeps every verb in the row's own slide and none in the list", async () => {
    const { container } = await panel();

    // Each address is a row that slides, and every control on this panel stands
    // in one of those slides — the list itself holds no button at all.
    for (const button of container.querySelectorAll("button")) {
      expect(button.closest('[role="group"]')).not.toBeNull();
    }
    // And no sentence anywhere: not on the band, not under the row in use, not
    // under the list.
    expect(container.querySelectorAll("p")).toHaveLength(0);
    expect(container.textContent).not.toContain("Automatic: this device prefers");
    expect(container.textContent).not.toContain("Works from anywhere");
  });

  it("gives an address this device is not on the one verb it can be given", async () => {
    const { chosen, container } = await panel();

    expect(verbs(container, "100.64.0.10:7890")).toEqual([
      { caption: "Use", name: "Use 100.64.0.10:7890" },
    ]);

    fireEvent.click(screen.getByLabelText("Use 100.64.0.10:7890"));
    expect(chosen).toEqual([[TAILSCALE, true]]);
  });

  it("asks the address in use the only question left about it", async () => {
    // Automatic: this device follows the durability order, so the verb is the
    // rank that freezes it here.
    const automatic = await panel();
    expect(verbs(automatic.container, "192.168.0.5:7890")).toEqual([
      { caption: "Pin", name: "Always use 192.168.0.5:7890" },
    ]);
    fireEvent.click(screen.getByLabelText("Always use 192.168.0.5:7890"));
    expect(automatic.chosen).toEqual([[LAN, true]]);
    document.body.innerHTML = "";

    // Pinned: the rank is already held, so the verb is the way back to letting
    // the app move itself onto the most durable address that answers.
    const pinned = await panel({ pinned: true });
    expect(verbs(pinned.container, "192.168.0.5:7890")).toEqual([
      { caption: "Auto", name: "Let this device pick the address" },
    ]);
    fireEvent.click(screen.getByLabelText("Let this device pick the address"));
    expect(pinned.chosen).toEqual([[TAILSCALE, false]]);
  });

  it("says which address is in use in a row that is not a control", async () => {
    const { container } = await panel();

    const mark = screen.getByText("in use");
    expect(mark.closest("button")).toBeNull();
    // One row wears it, and the reach that makes each address durable is the
    // only other word on any of them.
    expect(container.textContent?.match(/in use/g)).toHaveLength(1);
    expect(screen.getByText("Tailscale")).toBeTruthy();
    expect(screen.getByText("Local network")).toBeTruthy();
    expect(screen.getByText("This machine")).toBeTruthy();
  });

  it("keeps each slide caption one word wide, because the cell is 72px", async () => {
    const { container } = await panel();

    for (const button of container.querySelectorAll('[role="group"] button')) {
      expect(button.textContent?.trim()).toMatch(/^\S+$/);
    }
  });
});
