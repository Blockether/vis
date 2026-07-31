// Cross-platform persistent settings. On native (iOS/Android) this uses the
// Capacitor Preferences plugin; on the web it falls back to localStorage. This
// is the companion's mirror of the TUI's on-disk settings: the gateway
// connection (url + token) plus the active connection id are stored here so the
// app reconnects to the SAME gateway the TUI/other channels use.

import { Preferences } from '@capacitor/preferences';
import { bridged } from './bridge';
import type { GatewayConn, ThemePref } from './types';

const CONNS_KEY = 'vis.connections';
const ACTIVE_KEY = 'vis.activeConnection';
const THEME_PREF_KEY = 'vis.themePref';

// The companion defaults to the light theme regardless of what a gateway/TUI
// persists, and remembers the user's own choice locally across reloads.
const DEFAULT_THEME_PREF: ThemePref = 'light';

function localGet(key: string): string | null {
  try {
    return globalThis.localStorage?.getItem(key) ?? null;
  } catch {
    return null;
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
export async function upsertConnection(conn: GatewayConn): Promise<GatewayConn[]> {
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
  const active = await getActiveUrl();
  if (active === url) await setActiveUrl(conns[0]?.url ?? null);
  return conns;
}

/**
 * Move one saved gateway onto a different address, keeping its identity.
 *
 * A connection is keyed by URL, so switching to the Tailscale address must
 * REWRITE the entry rather than add a second machine: the token, label, id and
 * known alternates travel with it, the active pointer follows, and the
 * per-gateway subscribed-session list is re-keyed so live sessions survive the
 * move. Returns the new list.
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
  const store = await loadSubscriptionStore();
  if (store[from]) {
    store[to] = Array.from(new Set([...(store[to] ?? []), ...store[from]])).slice(
      0,
      MAX_SUBSCRIBED_SESSIONS,
    );
    delete store[from];
    await setRaw(SUBSCRIPTIONS_KEY, JSON.stringify(store));
  }
  return rest;
}

export async function getActiveUrl(): Promise<string | null> {
  return getRaw(ACTIVE_KEY);
}

export async function setActiveUrl(url: string | null): Promise<void> {
  await setRaw(ACTIVE_KEY, url ?? '');
}

export async function getActiveConnection(): Promise<GatewayConn | null> {
  const url = await getActiveUrl();
  if (!url) return null;
  const conns = await loadConnections();
  return conns.find((c) => c.url === url) ?? null;
}

/** The app-local theme preference. Defaults to the light theme. */
export async function getThemePref(): Promise<ThemePref> {
  const raw = await getRaw(THEME_PREF_KEY);
  return (raw ?? DEFAULT_THEME_PREF) as ThemePref;
}

export async function setThemePref(pref: ThemePref): Promise<void> {
  await setRaw(THEME_PREF_KEY, pref);
}
const SESSIONS_PER_PROJECT_KEY = 'vis.sessionsPerProject';
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
  pageSizeCache = normalizePageSize(raw ? Number(raw) : DEFAULT_SESSION_PAGE_SIZE);
  return pageSizeCache;
}

export async function setSessionsPerPage(value: number): Promise<void> {
  const normalized = normalizePageSize(value);
  await setRaw(SESSIONS_PER_PROJECT_KEY, String(normalized));
  pageSizeCache = normalized;
  for (const listener of pageSizeListeners) listener(normalized);
}

export function subscribeSessionsPerPage(listener: (value: number) => void): () => void {
  pageSizeListeners.add(listener);
  return () => {
    pageSizeListeners.delete(listener);
  };
}
const SUBSCRIPTIONS_KEY = 'vis.sessionSubscriptions';
const MAX_SUBSCRIBED_SESSIONS = 24;

type SubscriptionStore = Record<string, string[]>;

async function loadSubscriptionStore(): Promise<SubscriptionStore> {
  const raw = await getRaw(SUBSCRIPTIONS_KEY);
  if (!raw) return {};
  try {
    const parsed = JSON.parse(raw) as unknown;
    return parsed && typeof parsed === 'object' && !Array.isArray(parsed)
      ? parsed as SubscriptionStore
      : {};
  } catch {
    return {};
  }
}

/** Sessions the user has visited and keeps live-subscribed, scoped per gateway. */
export async function loadSubscribedSessions(gatewayUrl: string): Promise<string[]> {
  const store = await loadSubscriptionStore();
  return Array.from(new Set(store[gatewayUrl] ?? [])).slice(0, MAX_SUBSCRIBED_SESSIONS);
}

/** Mark one visited session as most-recently subscribed and persist across reloads. */
export async function rememberSubscribedSession(gatewayUrl: string, sid: string): Promise<string[]> {
  const store = await loadSubscriptionStore();
  const sessions = [sid, ...(store[gatewayUrl] ?? []).filter((id) => id !== sid)]
    .slice(0, MAX_SUBSCRIBED_SESSIONS);
  store[gatewayUrl] = sessions;
  await setRaw(SUBSCRIPTIONS_KEY, JSON.stringify(store));
  return sessions;
}