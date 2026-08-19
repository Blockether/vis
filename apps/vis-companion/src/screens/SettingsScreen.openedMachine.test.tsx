// @vitest-environment jsdom
import { render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { SettingsDialog } from "./SettingsScreen";
import type { GatewayConn } from "../lib/types";

const URL_A = "http://10.0.0.5:7890";

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

const open = (gateway: GatewayConn, gateways: GatewayConn[]) =>
  render(
    <SettingsDialog
      gateways={gateways}
      gateway={gateway}
      onAddMachine={async () => {}}
      onClose={() => {}}
    />,
  );

// Regression, user report from the simulator: opening Settings after a session showed
// that machine's own bands missing, while opening it straight off the list showed
// them. The dialog was handed a connection snapshotted before the machine's id was
// backfilled, and it opened the row named by THAT id -- one no paired machine has.
describe("the dialog opened on a snapshot of a machine", () => {
  it("opens the row of the machine it was opened on when the snapshot has no id", async () => {
    const view = open({ url: URL_A, token: "t" }, [
      { url: URL_A, token: "t", id: "be2c15686eaef0f4" },
    ]);

    await waitFor(() => expect(screen.getByText("MCP servers")).toBeTruthy());
    view.unmount();
  });

  it("still opens that row when the machine has since moved address", async () => {
    // The snapshot names the address the app reached it on; the fleet has since
    // rebound the same machine to another one.
    const view = open({ url: URL_A, token: "t", id: "be2c15686eaef0f4" }, [
      { url: "http://192.168.0.241:7890", token: "t", id: "be2c15686eaef0f4" },
    ]);

    await waitFor(() => expect(screen.getByText("MCP servers")).toBeTruthy());
    view.unmount();
  });

  it("opens no machine when the dialog was opened on none", () => {
    const view = render(
      <SettingsDialog
        gateways={[{ url: URL_A, token: "t", id: "be2c15686eaef0f4" }]}
        gateway={null}
        onAddMachine={async () => {}}
        onClose={() => {}}
      />,
    );

    expect(screen.queryByText("MCP servers")).toBeNull();
    view.unmount();
  });
});
