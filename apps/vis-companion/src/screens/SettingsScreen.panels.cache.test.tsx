// @vitest-environment jsdom
// Machine panels are warmed before their disclosure opens. Once they have answered,
// reopening that machine must paint the cached answer immediately instead of flickering.
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { SettingsDialog } from "./SettingsScreen";
import type { GatewayConn } from "../lib/types";

const MACHINE: GatewayConn = {
  url: "http://10.0.0.5:7890",
  token: "t",
  id: "be2c15686eaef0f4",
};

const SERVERS = {
  servers: [
    {
      name: "files",
      transport: "stdio",
      enabled: true,
      is_connected: true,
      is_managed: true,
      tools: 3,
      is_killed: false,
      command: "mcp-files",
    },
  ],
};

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json" },
  });

/** A machine that answers everything the dialog asks it. */
const machine = () =>
  vi.fn((input: RequestInfo | URL) => {
    const path = new URL(String(input), MACHINE.url).pathname;
    if (path === "/v1/mcp/servers") return Promise.resolve(json(SERVERS));
    return Promise.resolve(json({}));
  });

/** A machine that has gone quiet: nothing this open asks for will ever land. */
const silent = () => vi.fn(() => new Promise<Response>(() => {}));

let previousFetch: typeof fetch;

beforeEach(() => {
  previousFetch = globalThis.fetch;
});

afterEach(() => {
  globalThis.fetch = previousFetch;
  globalThis.localStorage?.clear();
  document.body.innerHTML = "";
  vi.restoreAllMocks();
});

const open = () =>
  render(
    <SettingsDialog
      gateways={[MACHINE]}
      onAddMachine={async () => {}}
      onClose={() => {}}
    />,
  );

const openMachine = () => {
  const row = screen
    .getAllByRole("button")
    .find((button) => button.hasAttribute("aria-expanded"));
  if (!row) throw new Error("Machine disclosure not found");
  fireEvent.click(row);
};

describe("opening settings for a machine that already answered", () => {
  it("paints the MCP servers and providers in the first open frame", async () => {
    globalThis.fetch = machine() as unknown as typeof fetch;
    const first = open();
    openMachine();
    await waitFor(() => expect(screen.getByText("files")).toBeTruthy());
    first.unmount();

    // Same machine, now unreachable: its disclosure paints what the cache holds.
    globalThis.fetch = silent() as unknown as typeof fetch;
    const second = open();
    openMachine();
    expect(screen.getByText("files")).toBeTruthy();
    expect(screen.queryByText("Checking provider sign-in…")).toBeNull();
    second.unmount();
  });

  // The device route's own refusal is remembered the same way, and is proved
  // beside the panel that reads it
  // (`SettingsScreen.notifications.cache.test.tsx`): this build is the web one,
  // where notifications never ask a machine at all.
});
