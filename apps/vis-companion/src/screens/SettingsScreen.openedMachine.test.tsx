// @vitest-environment jsdom
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { SettingsDialog } from "./SettingsScreen";
import type { GatewayConn } from "../lib/types";

const URL_A = "http://10.0.0.5:7890";
const URL_B = "http://10.0.0.6:7890";

/** A machine that answers every read with an empty body, so the bands can paint. */
const quiet = () =>
  Promise.resolve(
    new Response(JSON.stringify({}), {
      status: 200,
      headers: { "Content-Type": "application/json" },
    }),
  );

/** A reachable gateway whose protocol floor excludes this app build. */
const incompatibleSettings = (input: RequestInfo | URL) => {
  const path = new URL(String(input), URL_A).pathname;
  if (path !== "/v1/settings") return quiet();
  return Promise.resolve(
    new Response(
      JSON.stringify({
        error: {
          type: "incompatible_protocol",
          title: "Update this client",
          message:
            "The gateway speaks protocol 4 and no longer serves clients below protocol 4.",
        },
      }),
      { status: 426, headers: { "Content-Type": "application/json" } },
    ),
  );
};

let previousFetch: typeof fetch;

beforeEach(() => {
  previousFetch = globalThis.fetch;
  globalThis.fetch = vi.fn(quiet) as unknown as typeof fetch;
});

afterEach(() => {
  globalThis.fetch = previousFetch;
  globalThis.localStorage?.clear();
  document.body.innerHTML = "";
  vi.restoreAllMocks();
});

const open = (gateways: GatewayConn[]) =>
  render(
    <SettingsDialog
      gateways={gateways}
      onAddMachine={async () => {}}
      onClose={() => {}}
    />,
  );

describe("machine settings disclosures", () => {
  it("starts every machine closed and opens one only after its row is pressed", async () => {
    const view = open([
      { url: URL_A, token: "t", id: "be2c15686eaef0f4" },
      { url: URL_B, token: "t", id: "cad6247b600f9bbc" },
    ]);

    const [first, second] = screen
      .getAllByRole("button")
      .filter((button) => button.hasAttribute("aria-expanded"));
    expect(first).toHaveAttribute("aria-expanded", "false");
    expect(second).toHaveAttribute("aria-expanded", "false");
    expect(screen.queryByText("MCP servers")).toBeNull();

    fireEvent.click(first);

    expect(first).toHaveAttribute("aria-expanded", "true");
    expect(second).toHaveAttribute("aria-expanded", "false");
    await waitFor(() => expect(screen.getByText("MCP servers")).toBeTruthy());
    view.unmount();
  });

  // Regression, issue #ea166d2d-d22f-4a89-b117-d058641b7422: a protocol refusal
  // proves the machine answered, so no unreachable-machine panel may follow it.
  it("does not call a protocol-incompatible machine unreachable", async () => {
    globalThis.fetch = vi.fn(incompatibleSettings) as unknown as typeof fetch;
    const view = open([{ url: URL_A, token: "t", id: "be2c15686eaef0f4" }]);

    const row = screen
      .getAllByRole("button")
      .find((button) => button.hasAttribute("aria-expanded"));
    if (!row) throw new Error("Machine disclosure not found");
    fireEvent.click(row);

    await waitFor(() =>
      expect(screen.getAllByText(/gateway speaks protocol 4/i).length).toBeGreaterThan(0),
    );
    expect(screen.queryByText("Machine unreachable")).toBeNull();
    expect(screen.queryByRole("button", { name: "Retry" })).toBeNull();
    view.unmount();
  });
});
