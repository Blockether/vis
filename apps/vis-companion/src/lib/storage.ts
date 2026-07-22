// Cross-platform persistent settings. On native (iOS/Android) this uses the
// Capacitor Preferences plugin; on the web it falls back to localStorage. This
// is the companion's mirror of the TUI's on-disk settings: the gateway
// connection (url + token) plus the active connection id are stored here so the
// app reconnects to the SAME gateway the TUI/other channels use.

import { Preferences } from '@capacitor/preferences';
import type { GatewayConn } from './types';

const CONNS_KEY = 'vis.connections';
const ACTIVE_KEY = 'vis.activeConnection';

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
