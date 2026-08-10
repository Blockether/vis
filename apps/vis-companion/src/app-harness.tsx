// Mounting the WHOLE app, so the shell's own tests can look at the chrome.
//
// `App` reads its paired machines out of the mirrored web storage on the first
// frame (`loadConnectionsSync`), and everything after that is `fetch` against
// those gateways — so seeding one key and holding that one seam is the whole
// setup. Each mount gets a fresh gateway ORIGIN: the gateway client cache and
// its snapshots are keyed by URL, and a reused one lets a previous test's rows
// paint the next test's first frame.
import { render } from "@testing-library/react";

import { App } from "./App";
import { APP_MIN_GATEWAY_PROTOCOL, APP_PROTOCOL } from "./lib/compat";
import type { GatewayConn, Session } from "./lib/types";

export interface AppMachine {
  label?: string;
  sessions?: Session[];
  /** Extra routes, by pathname: whatever they return is the JSON body. */
  routes?: Record<string, unknown>;
}

let origins = 0;

export function renderApp({ machines = [{}] as AppMachine[] } = {}) {
  const conns: GatewayConn[] = machines.map((machine, index) => ({
    url: `http://app-gateway-${++origins}.example.com`,
    token: "t",
    id: `app-gateway-${origins}`,
    label: machine.label ?? `machine-${index + 1}`,
  }));
  const byOrigin = new Map(
    conns.map((conn, index) => [new URL(conn.url).origin, machines[index]]),
  );
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
    const machine = byOrigin.get(url.origin);
    if (!machine) return answer({});
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
        id: byOrigin.get(url.origin) ? conns.find((one) => new URL(one.url).origin === url.origin)?.id : undefined,
        protocol,
      });
    if (url.pathname === "/v1/capabilities")
      return answer({ version: 1, protocol, compatibility: { is_compatible: true } });
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
