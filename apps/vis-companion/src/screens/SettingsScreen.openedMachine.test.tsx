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
});
