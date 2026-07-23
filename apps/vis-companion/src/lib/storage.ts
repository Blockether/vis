// Cross-platform persistent settings. On native (iOS/Android) this uses the
// Capacitor Preferences plugin; on the web it falls back to localStorage. This
// is the companion's mirror of the TUI's on-disk settings: the gateway
// connection (url + token) plus the active connection id are stored here so the
// app reconnects to the SAME gateway the TUI/other channels use.

import { Preferences } from '@capacitor/preferences';
import type { GatewayConn, ThemePref } from './types';

const CONNS_KEY = 'vis.connections';
const ACTIVE_KEY = 'vis.activeConnection';
const THEME_PREF_KEY = 'vis.themePref';

// The companion defaults to the light theme regardless of what a gateway/TUI
// persists, and remembers the user's own choice locally across reloads.
const DEFAULT_THEME_PREF: ThemePref = 'light';

async function getRaw(key: string): Promise<string | null> {
  try {
    const { value } = await Preferences.get({ key });
    return value ?? null;
  } catch {
    // Web fallback when the plugin is unavailable.
    return globalThis.localStorage?.getItem(key) ?? null;
  }
}

async function setRaw(key: string, value: string): Promise<void> {
  try {
    await Preferences.set({ key, value });
  } catch {
    globalThis.localStorage?.setItem(key, value);
  }
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