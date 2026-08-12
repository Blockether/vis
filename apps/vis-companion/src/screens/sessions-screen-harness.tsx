// Mounting the SESSIONS screen, so its tests can look at the list.
//
// This screen builds its own `GatewayClient` per paired machine (`clientFor`),
// so the seam a test can hold is `fetch`: every machine here is a fake gateway
// answering the two reads the list makes, and every request it receives is
// recorded. Each mount gets its own gateway ORIGIN because the client cache and
// its snapshots are keyed by URL — reusing one would let a previous test's rows
// paint the next test's first frame.
import { render } from "@testing-library/react";

import { SessionsScreen } from "./SessionsScreen";
import type { GatewayConn, Session } from "../lib/types";

export interface MachineFixture {
  /** Shown as the machine's name; the URL is assigned by the harness. */
  label?: string;
  sessions?: Session[];
  /** This gateway is not answering — every read rejects, as an offline one does. */
  down?: boolean;
  /** Extra routes, by pathname: whatever they return is the JSON body. */
  routes?: Record<string, unknown>;
}

export interface FleetRequest {
  machine: string;
  method: string;
  path: string;
  /** Parsed JSON body when the request carried one. */
  body?: unknown;
  /** The caller's cancellation, so a test can see a superseded flight aborted. */
  signal?: AbortSignal | null;
}

let origins = 0;

/** A session row, filled in enough for the list to group and sort it. */
export function listSession(overrides: Partial<Session> = {}): Session {
  return {
    id: "s1",
    title: "A session",
    status: "idle",
    turn_count: 1,
    modified_at: new Date("2024-05-01T10:00:00Z").toISOString(),
    workspace: { root: "/Users/dev/project" },
    ...overrides,
  } as Session;
}

export function renderSessionsScreen({
  machines = [{}] as MachineFixture[],
  query = "",
  onQuery = () => {},
  onOpen = () => {},
  onUnreachable,
}: {
  machines?: MachineFixture[];
  query?: string;
  onQuery?: (next: string) => void;
  onOpen?: (conn: GatewayConn, sid: string, fresh?: boolean) => void;
  onUnreachable?: (message: string | null) => void;
} = {}) {
  const requests: FleetRequest[] = [];
  const conns: GatewayConn[] = machines.map((machine, index) => ({
    url: `http://gateway-${++origins}.example.com`,
    token: "t",
    label: machine.label ?? `machine-${index + 1}`,
  }));
  const byOrigin = new Map(
    conns.map((conn, index) => [new URL(conn.url).origin, machines[index]]),
  );

  const answer = (body: unknown) =>
    new Response(JSON.stringify(body), {
      status: 200,
      headers: { "Content-Type": "application/json", ETag: `"${origins}"` },
    });

  const previousFetch = globalThis.fetch;
  globalThis.fetch = (async (input: RequestInfo | URL, init?: RequestInit) => {
    const url = new URL(
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.href
          : input.url,
    );
    const machine = byOrigin.get(url.origin);
    const sent = typeof init?.body === "string" ? init.body : undefined;
    requests.push({
      machine: url.origin,
      method: init?.method ?? "GET",
      path: url.pathname + url.search,
      body: sent === undefined ? undefined : (JSON.parse(sent) as unknown),
      signal: init?.signal,
    });
    if (!machine) return answer({});
    if (machine.down) throw new TypeError("Failed to fetch");
    if (machine.routes && url.pathname in machine.routes)
      return answer(machine.routes[url.pathname]);
    if (url.pathname === "/v1/sessions") {
      const sessions = machine.sessions ?? [];
      return answer({ sessions, total: sessions.length, has_more: false });
    }
    if (url.pathname === "/v1/sessions/actions/search") return answer({ matches: [] });
    return answer({});
  }) as typeof fetch;

  const screen = (next: string) => (
    <SessionsScreen
      conns={conns}
      query={next}
      onQuery={onQuery}
      subscriptions={null}
      onOpen={onOpen}
      onUnreachable={onUnreachable}
    />
  );
  const view = render(screen(query));

  return {
    ...view,
    conns,
    requests,
    /** Hand the list a new filter, the way the app bar's field does. */
    setQuery(next: string) {
      view.rerender(screen(next));
    },
    /** Put the real `fetch` back; every test that mounts must call this. */
    restore() {
      globalThis.fetch = previousFetch;
    },
  };
}
