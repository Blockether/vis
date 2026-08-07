// Cross-platform persistent settings. On native (iOS/Android) this uses the
// Capacitor Preferences plugin; on the web it falls back to localStorage. This
// is the companion's mirror of the TUI's on-disk settings: the gateway
// connection (url + token) plus the active connection id are stored here so the
// app reconnects to the SAME gateway the TUI/other channels use.

import { Preferences } from "@capacitor/preferences";
import { bridged } from "./bridge";
import type { GatewayConn, ThemePref, ThemeSummary } from "./types";

const CONNS_KEY = "vis.connections";
const ACTIVE_KEY = "vis.activeConnection";
const PRIMARY_KEY = "vis.primaryConnection";
const THEME_PREF_KEY = "vis.themePref";
const THEME_PALETTE_KEY = "vis.themePalette";

// Appearance belongs to this application installation, never a gateway.
const DEFAULT_THEME_PREF: ThemePref = "blockether-light";

function localGet(key: string): string | null {
  try {
    return globalThis.localStorage?.getItem(key) ?? null;
  } catch {
    return null;
  }
}

/** Read the mirrored web storage without waiting for the native Preferences bridge. */
export function loadConnectionsSync(): GatewayConn[] {
  const raw = localGet(CONNS_KEY);
  if (!raw) return [];
  try {
    const parsed = JSON.parse(raw);
    return Array.isArray(parsed) ? (parsed as GatewayConn[]) : [];
  } catch {
    return [];
  }
}

function localSet(key: string, value: string): void {
  try {
    globalThis.localStorage?.setItem(key, value);
  } catch {
    // Private mode / quota: the Preferences write is the real store.
  }
}

// Reads and writes go through BOTH stores on purpose. Preferences is the
// durable one (it survives a webview data reset), localStorage is the one that
// answers even when the native bridge does not — see `lib/bridge.ts`. Mirroring
// every value into localStorage is what makes the fallback real data instead of
// an empty app: a silent bridge must never look like "no gateway paired".
async function getRaw(key: string): Promise<string | null> {
  const value = await bridged(
    async () => (await Preferences.get({ key })).value ?? null,
    () => localGet(key),
  );
  if (value !== null) localSet(key, value);
  return value;
}

async function setRaw(key: string, value: string): Promise<void> {
  localSet(key, value);
  await bridged(
    async () => {
      await Preferences.set({ key, value });
    },
    () => undefined,
  );
}

export async function loadConnections(): Promise<GatewayConn[]> {
  const raw = await getRaw(CONNS_KEY);
  if (!raw) return [];
  try {
    const parsed = JSON.parse(raw);
    return Array.isArray(parsed) ? (parsed as GatewayConn[]) : [];
  } catch {
    return [];
  }
}

export async function saveConnections(conns: GatewayConn[]): Promise<void> {
  await setRaw(CONNS_KEY, JSON.stringify(conns));
}

/** Insert-or-replace a connection keyed by its URL; returns the new list. */
export async function upsertConnection(
  conn: GatewayConn,
): Promise<GatewayConn[]> {
  const conns = await loadConnections();
  const idx = conns.findIndex((c) => c.url === conn.url);
  if (idx >= 0) conns[idx] = { ...conns[idx], ...conn };
  else conns.push(conn);
  await saveConnections(conns);
  return conns;
}

export async function removeConnection(url: string): Promise<GatewayConn[]> {
  const conns = (await loadConnections()).filter((c) => c.url !== url);
  await saveConnections(conns);
  if ((await getActiveUrl()) === url) await setActiveUrl(conns[0]?.url ?? null);
  if ((await getPrimaryUrl()) === url)
    await setPrimaryUrl(conns[0]?.url ?? null);
  await forgetGatewayNotify(url);
  return conns;
}

/**
 * Move one saved gateway onto a different address, keeping its identity.
 *
 * A connection is keyed by URL, so switching to the Tailscale address must
 * REWRITE the entry rather than add a second machine: the token, label, id and
 * known alternates travel with it, both the current and primary pointers follow,
 * and the per-gateway subscribed-session list is re-keyed so live sessions survive
 * the move. Returns the new list.
 */
export async function switchConnectionUrl(
  from: string,
  to: string,
  patch: Partial<GatewayConn> = {},
): Promise<GatewayConn[]> {
  const conns = await loadConnections();
  const idx = conns.findIndex((c) => c.url === from);
  if (idx < 0 || from === to) return conns;
  const moved: GatewayConn = { ...conns[idx], ...patch, url: to };
  const rest = conns.filter((c, i) => i !== idx && c.url !== to);
  rest.splice(Math.min(idx, rest.length), 0, moved);
  await saveConnections(rest);
  if ((await getActiveUrl()) === from) await setActiveUrl(to);
  if ((await getPrimaryUrl()) === from) await setPrimaryUrl(to);
  const store = await loadSubscriptionStore();
  if (store[from]) {
    store[to] = Array.from(
      new Set([...(store[to] ?? []), ...store[from]]),
    ).slice(0, MAX_SUBSCRIBED_SESSIONS);
    delete store[from];
    await setRaw(SUBSCRIPTIONS_KEY, JSON.stringify(store));
  }
  await moveGatewayNotify(from, to);
  return rest;
}

export async function getActiveUrl(): Promise<string | null> {
  return getRaw(ACTIVE_KEY);
}

export async function setActiveUrl(url: string | null): Promise<void> {
  await setRaw(ACTIVE_KEY, url ?? "");
}

export async function getActiveConnection(): Promise<GatewayConn | null> {
  const url = await getActiveUrl();
  if (!url) return null;
  const conns = await loadConnections();
  return conns.find((c) => c.url === url) ?? null;
}
/** The default gateway opened when the app starts. Exactly one saved gateway is primary. */
export async function getPrimaryUrl(): Promise<string | null> {
  return getRaw(PRIMARY_KEY);
}

export async function setPrimaryUrl(url: string | null): Promise<void> {
  await setRaw(PRIMARY_KEY, url ?? "");
}

/**
 * Resolve the app's primary gateway, migrating the pre-primary active selection
 * (and finally the first saved gateway) so existing installations keep a default.
 */
export async function getPrimaryConnection(): Promise<GatewayConn | null> {
  const conns = await loadConnections();
  if (conns.length === 0) return null;
  const stored = await getPrimaryUrl();
  const legacy = await getActiveUrl();
  const primary =
    conns.find((c) => c.url === stored || c.url === legacy) ?? conns[0];
  if (primary.url !== stored) await setPrimaryUrl(primary.url);
  return primary;
}

/** The selected app-local palette, migrated from the old light/dark preference. */
export async function getThemePref(): Promise<ThemePref> {
  const raw = await getRaw(THEME_PREF_KEY);
  if (raw === "light") return "blockether-light";
  if (raw === "dark") return "blockether-dark";
  return raw?.trim() || DEFAULT_THEME_PREF;
}

export async function setThemePref(pref: ThemePref): Promise<void> {
  await setRaw(THEME_PREF_KEY, pref);
}

/** Cache the selected palette so it still paints before any gateway is reachable. */
export async function getThemePalette(): Promise<ThemeSummary | null> {
  const raw = await getRaw(THEME_PALETTE_KEY);
  if (!raw) return null;
  try {
    const theme = JSON.parse(raw) as Partial<ThemeSummary>;
    return typeof theme.id === "string" &&
      typeof theme.display_name === "string" &&
      (theme.mode === "light" || theme.mode === "dark")
      ? (theme as ThemeSummary)
      : null;
  } catch {
    return null;
  }
}

export async function setThemePalette(theme: ThemeSummary): Promise<void> {
  await setRaw(THEME_PALETTE_KEY, JSON.stringify(theme));
}
const SESSIONS_PER_PROJECT_KEY = "vis.sessionsPerProject";
export const DEFAULT_SESSION_PAGE_SIZE = 10;
export const SESSION_PAGE_SIZES: readonly number[] = [5, 10, 15];

function normalizePageSize(value: number): number {
  return SESSION_PAGE_SIZES.includes(value) ? value : DEFAULT_SESSION_PAGE_SIZE;
}

// App-local only — never sent to the gateway. One knob for two views: how many
// live sessions a collapsed project exposes, and the step size when a project is
// expanded and paged.
let pageSizeCache: number | null = null;
const pageSizeListeners = new Set<(value: number) => void>();

export async function getSessionsPerPage(): Promise<number> {
  if (pageSizeCache !== null) return pageSizeCache;
  const raw = await getRaw(SESSIONS_PER_PROJECT_KEY);
  pageSizeCache = normalizePageSize(
    raw ? Number(raw) : DEFAULT_SESSION_PAGE_SIZE,
  );
  return pageSizeCache;
}

export async function setSessionsPerPage(value: number): Promise<void> {
  const normalized = normalizePageSize(value);
  await setRaw(SESSIONS_PER_PROJECT_KEY, String(normalized));
  pageSizeCache = normalized;
  for (const listener of pageSizeListeners) listener(normalized);
}

export function subscribeSessionsPerPage(
  listener: (value: number) => void,
): () => void {
  pageSizeListeners.add(listener);
  return () => {
    pageSizeListeners.delete(listener);
  };
}

const SUBSCRIPTIONS_KEY = "vis.sessionSubscriptions";
const MAX_SUBSCRIBED_SESSIONS = 24;

type SubscriptionStore = Record<string, string[]>;

async function loadSubscriptionStore(): Promise<SubscriptionStore> {
  const raw = await getRaw(SUBSCRIPTIONS_KEY);
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    return parsed && typeof parsed === "object" && !Array.isArray(parsed)
      ? (parsed as SubscriptionStore)
      : {};
  } catch {
    return {};
  }
}

/** Sessions the user has visited and keeps live-subscribed, scoped per gateway. */
export async function loadSubscribedSessions(
  gatewayUrl: string,
): Promise<string[]> {
  const store = await loadSubscriptionStore();
  return Array.from(new Set(store[gatewayUrl] ?? [])).slice(
    0,
    MAX_SUBSCRIBED_SESSIONS,
  );
}

/** Mark one visited session as most-recently subscribed and persist across reloads. */
export async function rememberSubscribedSession(
  gatewayUrl: string,
  sid: string,
): Promise<string[]> {
  const store = await loadSubscriptionStore();
  const sessions = [
    sid,
    ...(store[gatewayUrl] ?? []).filter((id) => id !== sid),
  ].slice(0, MAX_SUBSCRIBED_SESSIONS);
  store[gatewayUrl] = sessions;
  await setRaw(SUBSCRIPTIONS_KEY, JSON.stringify(store));
  return sessions;
}

// ── Notifications, per gateway ──────────────────────────────────────
// Native push is a decision about ONE machine, not about the app: the phone can
// want a buzz when the work laptop finishes a turn and want silence from the
// build box it is only watching. So the switch lives in that gateway's settings
// and is stored per gateway URL, exactly like the subscribed-session list.
const NOTIFY_KEY = "vis.gatewayNotifications";

/** Only an EXPLICIT entry is stored; absence means "not decided yet". */
type NotifyStore = Record<string, boolean>;

async function loadNotifyStore(): Promise<NotifyStore> {
  const raw = await getRaw(NOTIFY_KEY);
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed))
      return {};
    const store: NotifyStore = {};
    for (const [url, on] of Object.entries(parsed as Record<string, unknown>)) {
      if (typeof on === "boolean") store[url] = on;
    }
    return store;
  } catch {
    return {};
  }
}

async function saveNotifyStore(store: NotifyStore): Promise<void> {
  await setRaw(NOTIFY_KEY, JSON.stringify(store));
}

/**
 * Whether this device wants native pushes FROM ONE gateway.
 *
 * Defaults to on: a machine you paired is a machine you want to hear from, and
 * nothing is delivered anyway until the OS permission exists. Turning the switch
 * off in that gateway's settings is the only thing that silences it — and it
 * stays off, including across relaunches, which is what makes the choice
 * per gateway instead of "whichever gateway the app happened to open".
 */
export async function getGatewayNotify(url: string): Promise<boolean> {
  return (await loadNotifyStore())[url] ?? true;
}

export async function setGatewayNotify(
  url: string,
  on: boolean,
): Promise<void> {
  const store = await loadNotifyStore();
  store[url] = on;
  await saveNotifyStore(store);
}

/** The answer belongs to the machine, so it follows the machine to a new address. */
async function moveGatewayNotify(from: string, to: string): Promise<void> {
  const store = await loadNotifyStore();
  if (!(from in store)) return;
  store[to] = store[from];
  delete store[from];
  await saveNotifyStore(store);
}

/** Forgetting a gateway forgets its answer; re-pairing starts from the default. */
async function forgetGatewayNotify(url: string): Promise<void> {
  const store = await loadNotifyStore();
  if (!(url in store)) return;
  delete store[url];
  await saveNotifyStore(store);
}

// ── Relay grants, per relay ─────────────────────────────────────────
// A machine that holds no push credentials of its own can still wake this phone,
// but only through a relay, and only by presenting a capability this DEVICE
// minted for itself. That grant names the device, not the machine, so it is
// minted ONCE per relay and reused by every machine that needs it — the relay
// rate-limits minting on purpose, and a launch sweep over five paired machines
// that minted five grants would be refused by it.
//
// It is stored because it must outlive the launch that created it: re-minting on
// every start would eventually be throttled into silence, and the app would have
// no way to name — and therefore to REVOKE — the grant a gateway is still
// holding.
const GRANT_KEY = "vis.relayGrants";

/** One relay's grant for this device, with the token it was minted for. */
export interface RelayGrant {
  /** The OS push token this grant carries; a rotated token makes it useless. */
  token: string;
  grant: string;
  /** Epoch ms the relay stamped into it, when it said. */
  expires_at?: number;
}

type GrantStore = Record<string, RelayGrant>;

function asGrant(value: unknown): RelayGrant | null {
  if (!value || typeof value !== "object") return null;
  const row = value as Record<string, unknown>;
  if (typeof row.grant !== "string" || !row.grant) return null;
  if (typeof row.token !== "string") return null;
  return {
    token: row.token,
    grant: row.grant,
    expires_at: typeof row.expires_at === "number" ? row.expires_at : undefined,
  };
}

async function loadGrantStore(): Promise<GrantStore> {
  const raw = await getRaw(GRANT_KEY);
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed))
      return {};
    const store: GrantStore = {};
    for (const [url, value] of Object.entries(
      parsed as Record<string, unknown>,
    )) {
      const grant = asGrant(value);
      if (grant) store[url] = grant;
    }
    return store;
  } catch {
    return {};
  }
}

/** The grant this device already holds for one relay, if any. */
export async function getRelayGrant(
  relayUrl: string,
): Promise<RelayGrant | null> {
  return (await loadGrantStore())[relayUrl] ?? null;
}

export async function setRelayGrant(
  relayUrl: string,
  grant: RelayGrant,
): Promise<void> {
  const store = await loadGrantStore();
  store[relayUrl] = grant;
  await setRaw(GRANT_KEY, JSON.stringify(store));
}

/**
 * Every grant this device ever minted.
 *
 * Turning notifications off has to name what the gateway is holding, and a
 * relayed registration is filed under the GRANT, not under the push token.
 */
export async function relayGrants(): Promise<RelayGrant[]> {
  return Object.values(await loadGrantStore());
}
