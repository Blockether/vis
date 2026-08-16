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
  /**
   * A `down` gateway that comes back on the SECOND read it receives, so a test can
   * watch a retry wake a machine instead of only watching it fail.
   */
  heals?: boolean;
  /**
   * A `down` gateway that BLACKHOLES the reads after the first one: they are
   * accepted and never answered, the way a closed laptop takes a socket without
   * refusing it. The read ends only when the caller cancels it, so a test can watch
   * a press give up on its own deadline instead of on a transport error.
   */
  hangs?: boolean;
  /** Answers its list reads, but never answers a search. */
  searchHangs?: boolean;
  /**
   * Answers its FIRST page and holds every later one until `releasePages`, the way
   * a machine with more history than one window trickles in behind the machines
   * beside it.
   */
  holdsPages?: boolean;
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
let created = 0;
// Reads per GATEWAY, so `heals` and `hangs` can answer the read they were given — and
// keep counting across a relaunch that mounts the same machines again (`at`), which is
// the only way to watch what this device REMEMBERED about a machine outlive the screen.
const reads = new Map<string, number>();

/** A read nobody answers: it ends when — and only when — the caller aborts it. */
const blackhole = (signal?: AbortSignal | null) =>
  new Promise<Response>((_resolve, reject) => {
    signal?.addEventListener(
      "abort",
      () => reject(new DOMException("Aborted", "AbortError")),
      { once: true },
    );
  });

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
  isVisible = true,
  at,
}: {
  machines?: MachineFixture[];
  query?: string;
  onQuery?: (next: string) => void;
  onOpen?: (conn: GatewayConn, sid: string, fresh?: boolean) => void;
  onUnreachable?: (message: string | null) => void;
  /** Mounted but off the glass, the way the shell parks it behind a session. */
  isVisible?: boolean;
  /**
   * Mount these very machines again, instead of minting fresh ones. A relaunch
   * builds the list from nothing while what the app SAVED is still there, and that
   * is the only way to watch a preference outlive the screen that set it.
   */
  at?: GatewayConn[];
} = {}) {
  const requests: FleetRequest[] = [];
  const conns: GatewayConn[] =
    at ??
    machines.map((machine, index) => ({
      url: `http://gateway-${++origins}.example.com`,
      token: "t",
      label: machine.label ?? `machine-${index + 1}`,
    }));
  // Each mount gets its OWN copy of the fixture rows. A PATCH lands on the gateway's
  // own row — that is where a star lives — so one test's star must never leak into
  // the next through a shared fixture.
  const byOrigin = new Map(
    conns.map((conn, index) => [
      new URL(conn.url).origin,
      { ...machines[index], sessions: machines[index].sessions?.map((row) => ({ ...row })) },
    ]),
  );

  const answer = (body: unknown) =>
    new Response(JSON.stringify(body), {
      status: 200,
      headers: { "Content-Type": "application/json", ETag: `"${origins}"` },
    });

  // A list read that a test can hold open, so it can watch what does NOT wait for
  // the fleet to drain (a create opens its session while the rows are still in
  // flight). Held reads resolve normally the moment `releaseList` runs.
  let held: Promise<void> | null = null;
  // A machine that answers its first window and holds the rest, so a test can put
  // a later page on the glass at a moment of its choosing.
  let heldPages: Promise<void> | null = null;
  let releasePagesFn: (() => void) | null = null;
  let release: (() => void) | null = null;

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
    const seen = (reads.get(url.origin) ?? 0) + 1;
    reads.set(url.origin, seen);
    if (machine.hangs && seen > 1) return blackhole(init?.signal);
    // Alive to the list, dark to the search: the machine whose transcripts nobody is
    // reading answers everything else, so its darkness is something only a search can
    // discover — and something the next search has to remember.
    if (machine.searchHangs && url.pathname === "/v1/sessions/actions/search")
      return blackhole(init?.signal);
    if (machine.down && !(machine.heals && seen > 1)) throw new TypeError("Failed to fetch");
    if (machine.routes && url.pathname in machine.routes)
      return answer(machine.routes[url.pathname]);
    // PATCH /v1/sessions/:sid — the gateway OWNS what this changes: it applies the
    // star (or the rename) to its own row, echoes the row back, and every later list
    // read from this machine tells the same story.
    const one = /^\/v1\/sessions\/([^/]+)$/.exec(url.pathname);
    if (one && (init?.method ?? "GET") === "PATCH") {
      const rows = machine.sessions ?? [];
      const index = rows.findIndex((row) => row.id === decodeURIComponent(one[1]));
      if (index < 0) return answer({});
      const body = (sent ? JSON.parse(sent) : {}) as Record<string, unknown>;
      const ranks = rows
        .map((row) => (typeof row.favorite_rank === "number" ? row.favorite_rank : 0))
        .concat(0);
      const row = { ...rows[index] };
      if ("is_favorite" in body)
        row.favorite_rank = body.is_favorite ? Math.max(...ranks) + 1 : null;
      if (typeof body.title === "string") row.title = body.title;
      rows[index] = row;
      return answer(row);
    }
    if (url.pathname === "/v1/sessions") {
      // The create answers a session with an id, the way the gateway's 201 does:
      // without one the screen has nothing to open.
      if ((init?.method ?? "GET") === "POST")
        return answer({ id: `created-${++created}`, channel: "web", title: null });
      if (held) await held;
      if (machine.holdsPages && Number(url.searchParams.get("offset") ?? 0) > 0) {
        if (!heldPages)
          heldPages = new Promise<void>((resolve) => {
            releasePagesFn = resolve;
          });
        await heldPages;
      }
      const sessions = machine.sessions ?? [];
      // A real gateway answers a WINDOW, and a machine with more history than one
      // page makes the client come back for the rest — which is what paints a
      // second machine's rows a beat after the first machine's.
      const limit = Number(url.searchParams.get("limit") ?? sessions.length);
      const offset = Number(url.searchParams.get("offset") ?? 0);
      const window = sessions.slice(offset, offset + (limit || sessions.length));
      return answer({
        sessions: window,
        total: sessions.length,
        has_more: offset + window.length < sessions.length,
      });
    }
    if (url.pathname === "/v1/sessions/actions/search") return answer({ matches: [] });
    return answer({});
  }) as typeof fetch;

  let shownQuery = query;
  let shownVisible = isVisible;
  const screen = (next: string, visible: boolean) => (
    <SessionsScreen
      conns={conns}
      isVisible={visible}
      query={next}
      onQuery={onQuery}
      subscriptions={null}
      onOpen={onOpen}
      onUnreachable={onUnreachable}
    />
  );
  const view = render(screen(query, isVisible));

  return {
    ...view,
    conns,
    requests,
    /** Suspend every further list read until `releaseList`. */
    holdList() {
      held = new Promise<void>((resolve) => {
        release = resolve;
      });
    },
    /** Let every held later page answer. */
    releasePages() {
      releasePagesFn?.();
      heldPages = null;
      releasePagesFn = null;
    },
    releaseList() {
      held = null;
      release?.();
      release = null;
    },
    /** Hand the list a new filter, the way the app bar's field does. */
    setQuery(next: string) {
      shownQuery = next;
      view.rerender(screen(next, shownVisible));
    },
    /** Park the list behind a session, or bring it back — mounted either way. */
    setVisible(next: boolean) {
      shownVisible = next;
      view.rerender(screen(shownQuery, next));
    },
    /** Put the real `fetch` back; every test that mounts must call this. */
    restore() {
      globalThis.fetch = previousFetch;
    },
  };
}
