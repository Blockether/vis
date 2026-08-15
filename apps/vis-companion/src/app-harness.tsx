// Mounting the WHOLE app, so the shell's own tests can look at the chrome.
//
// `App` reads its paired machines out of the mirrored web storage on the first
// frame (`loadConnectionsSync`), and everything after that is `fetch` against
// those gateways — so seeding one key and holding that one seam is the whole
// setup. Each mount gets a fresh gateway ORIGIN: the gateway client cache and
// its snapshots are keyed by URL, and a reused one lets a previous test's rows
// paint the next test's first frame — so a test that is ABOUT addresses (the
// durability order lives in `lib/endpoints`, which reads the HOST) names its
// own `url`/`alts` and owns the reuse it takes on.
import { render } from "@testing-library/react";

import { App } from "./App";
import { APP_MIN_GATEWAY_PROTOCOL, APP_PROTOCOL } from "./lib/compat";
import type { GatewayConn, Session } from "./lib/types";

export interface AppMachine {
  label?: string;
  sessions?: Session[];
  /** Extra routes, by pathname: whatever they return is the JSON body. */
  routes?: Record<string, unknown>;
  /** The saved address, when the test needs a real LAN/tailnet shape. */
  url?: string;
  /** Every other address the SAME gateway answers on, as the app saved them. */
  alts?: string[];
  /** This device was told to use `url` BY NAME. */
  pinned?: boolean;
}

let origins = 0;

export function renderApp({
  machines = [{}] as AppMachine[],
  /** Addresses that answer nothing at all — a LAN address from another network. */
  unreachable = [] as string[],
} = {}) {
  const conns: GatewayConn[] = machines.map((machine, index) => {
    const id = `app-gateway-${++origins}`;
    return {
      url: machine.url ?? `http://${id}.example.com`,
      token: "t",
      id,
      label: machine.label ?? `machine-${index + 1}`,
      ...(machine.alts ? { alts: machine.alts } : {}),
      ...(machine.pinned ? { pinned: true } : {}),
    };
  });
  // Every address of a machine answers AS that machine: one gateway on the LAN
  // and on the tailnet is one row, not two.
  const byOrigin = new Map<string, { machine: AppMachine; conn: GatewayConn }>();
  conns.forEach((conn, index) => {
    for (const address of [conn.url, ...(conn.alts ?? [])])
      byOrigin.set(new URL(address).origin, { machine: machines[index]!, conn });
  });
  const dead = new Set(unreachable.map((address) => new URL(address).origin));
  // Both mirrors: the sync read is plain web storage, the async one comes back
  // through Capacitor Preferences, whose web implementation prefixes its keys.
  for (const prefix of ["", "CapacitorStorage."]) {
    localStorage.setItem(`${prefix}vis.connections`, JSON.stringify(conns));
    localStorage.setItem(
      `${prefix}vis.activeConnection`,
      JSON.stringify(conns[0]),
    );
    localStorage.setItem(
      `${prefix}vis.primaryConnection`,
      JSON.stringify(conns[0]),
    );
  }

  const answer = (body: unknown) =>
    new Response(JSON.stringify(body), {
      status: 200,
      headers: { "Content-Type": "application/json", ETag: `"${origins}"` },
    });

  const previousFetch = globalThis.fetch;
  globalThis.fetch = (async (input: RequestInfo | URL) => {
    const url = new URL(
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.href
          : input.url,
    );
    // An address that answers NOTHING fails the way a dead LAN address does:
    // a network error, never a status, so failover has to see it as one.
    if (dead.has(url.origin)) throw new TypeError("Failed to fetch");
    const entry = byOrigin.get(url.origin);
    if (!entry) return answer({});
    const machine = entry.machine;
    if (machine.routes && url.pathname in machine.routes)
      return answer(machine.routes[url.pathname]);
    if (url.pathname === "/v1/sessions") {
      const sessions = machine.sessions ?? [];
      return answer({ sessions, total: sessions.length, has_more: false });
    }
    if (url.pathname === "/v1/sessions/actions/search")
      return answer({ matches: [] });
    // The handshake every screen waits on: a gateway speaking this build's wire.
    const protocol = {
      protocol: APP_PROTOCOL,
      min_client: APP_PROTOCOL,
      min_gateway: APP_MIN_GATEWAY_PROTOCOL,
      version: "0.0.0-test",
    };
    if (url.pathname === "/healthz")
      return answer({
        status: "ok",
        id: entry.conn.id,
        protocol,
      });
    // A capabilities answer a session screen can actually mount against: it
    // reads `features` on its first frame, so a gateway without one is not a
    // gateway this app runs on.
    if (url.pathname === "/v1/capabilities")
      return answer({
        version: 1,
        protocol,
        compatibility: { is_compatible: true },
        features: {
          chat: { enabled: true },
          attachments: {
            enabled: true,
            transport: "inline-base64",
            media_types: ["image/png"],
            max_files: 4,
            max_file_bytes: 1_000_000,
          },
          voice: {
            enabled: false,
            transport: "audio/wav",
            transcription: "gateway-local",
            model: { state: "absent" },
          },
        },
      });
    return answer({});
  }) as typeof fetch;

  const view = render(<App />);

  return {
    ...view,
    conns,
    /** Put the real `fetch` and an empty pairing back; every mount must call it. */
    restore() {
      globalThis.fetch = previousFetch;
      localStorage.clear();
    },
  };
}
