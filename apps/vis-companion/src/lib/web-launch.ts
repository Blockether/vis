// One-time web launch. `vis-agent web` asks its gateway for a launch code
// (src/com/blockether/vis/internal/gateway/server/web.clj) and opens
//
//   http://127.0.0.1:7890/#launch=<code>&gw=<gateway id>
//
// The fragment never reaches a server. The app takes it out of the address bar
// before the first render, exchanges the code once for the gateway token, and
// saves the gateway under the address this browser reached it at. That address is
// the one that works here, also when WSL forwards a Windows `localhost` port.

import { mergeAddresses, normalizeAddress } from './endpoints';
import {
  clearRevocation,
  loadConnections,
  removeConnection,
  switchConnectionUrl,
  updateConnection,
  upsertConnection,
} from './storage';
import type { GatewayConn } from './types';

/** A launch read from the address bar: the code and the gateway that minted it. */
export interface WebLaunch {
  code: string;
  gatewayId: string;
}

/** What the gateway gives for a redeemed code. */
export interface WebLaunchGrant {
  gatewayId: string;
  token: string;
  requireToken: boolean;
}

/** A launch that cannot connect this browser. The message is for the reader. */
export class WebLaunchError extends Error {}

/** Read `#launch=<code>&gw=<id>`, or null for any other fragment. */
export function parseWebLaunch(hash: string): WebLaunch | null {
  const params = new URLSearchParams(String(hash ?? '').replace(/^#/, ''));
  const code = params.get('launch')?.trim();
  const gatewayId = params.get('gw')?.trim();
  return code && gatewayId ? { code, gatewayId } : null;
}

/**
 * Take a launch out of the address bar. A reload, the history and a copied
 * address then never carry the used code, and the hash router starts clean.
 */
export function takeWebLaunch(
  win: Pick<Window, 'location' | 'history'> = window,
): WebLaunch | null {
  const launch = parseWebLaunch(win.location.hash);
  if (launch)
    win.history.replaceState(win.history.state, '', win.location.pathname + win.location.search);
  return launch;
}

/** Exchange the code at `origin`, and refuse a grant from another gateway. */
export async function redeemWebLaunch(
  origin: string,
  launch: WebLaunch,
  fetchImpl: typeof fetch = fetch,
): Promise<WebLaunchGrant> {
  let response: Response;
  try {
    response = await fetchImpl(`${normalizeAddress(origin)}/v1/web/launches/redeem`, {
      method: 'POST',
      headers: { Accept: 'application/json', 'Content-Type': 'application/json' },
      body: JSON.stringify({ code: launch.code }),
      cache: 'no-store',
    });
  } catch {
    throw new WebLaunchError(`Vis could not reach the gateway at ${origin} to connect this browser.`);
  }
  if (response.status === 404)
    throw new WebLaunchError(
      'This launch address was already used or has expired. Run `vis-agent web` again.',
    );
  if (!response.ok)
    throw new WebLaunchError(
      `The gateway refused to connect this browser (HTTP ${response.status}). Run \`vis-agent web\` again.`,
    );
  const body = (await response.json().catch(() => null)) as Record<string, unknown> | null;
  const gatewayId = typeof body?.gateway_id === 'string' ? body.gateway_id : '';
  const token = typeof body?.token === 'string' ? body.token : '';
  if (!gatewayId || gatewayId !== launch.gatewayId)
    throw new WebLaunchError(
      `The gateway at ${origin} is not the one that \`vis-agent web\` started. Nothing was saved.`,
    );
  return { gatewayId, token, requireToken: body?.require_token === true };
}

/** How a grant changes the saved machines. */
export type WebLaunchPlan =
  /** A gateway this browser has not saved. `release` drops its address from other machines. */
  | { kind: 'add'; conn: GatewayConn; release: { url: string; alts: string[] }[] }
  /** The saved machine for this gateway or this address, with a new token and id. */
  | { kind: 'update'; url: string; patch: Partial<GatewayConn> }
  /** This gateway, saved at an address this browser did not open: move it here. */
  | { kind: 'move'; from: string; to: string; patch: Partial<GatewayConn> }
  /**
   * Another gateway is saved at this address, but this gateway served the page.
   * The saved one is stale: forget it, then apply `then`.
   */
  | { kind: 'replace'; stale: string; then: WebLaunchPlan };

function credentials(grant: WebLaunchGrant): Partial<GatewayConn> {
  return { id: grant.gatewayId, ...(grant.token ? { token: grant.token } : {}) };
}

/**
 * Decide what a grant does to the saved machines `conns`. Only the machine with
 * this gateway's id, or the one saved at `origin`, changes. The gateway at `origin`
 * minted the code, so a machine saved there with another id no longer answers there.
 */
export function planWebLaunch(
  conns: readonly GatewayConn[],
  origin: string,
  grant: WebLaunchGrant,
): WebLaunchPlan {
  const address = normalizeAddress(origin);
  const patch = credentials(grant);
  const occupant = conns.find((conn) => normalizeAddress(conn.url) === address);
  if (occupant) {
    if (occupant.id && occupant.id !== grant.gatewayId)
      return {
        kind: 'replace',
        stale: occupant.url,
        then: planWebLaunch(
          conns.filter((conn) => conn !== occupant),
          origin,
          grant,
        ),
      };
    return { kind: 'update', url: occupant.url, patch };
  }
  const same = conns.find((conn) => conn.id === grant.gatewayId);
  if (same) {
    // A hand-picked address stays; this browser's address becomes an alternate.
    if (same.pinned)
      return {
        kind: 'update',
        url: same.url,
        patch: { ...patch, alts: mergeAddresses(same.alts, [address]) },
      };
    return {
      kind: 'move',
      from: same.url,
      to: address,
      patch: {
        ...patch,
        alts: mergeAddresses(same.alts, [same.url]).filter((url) => url !== address),
      },
    };
  }
  const release = conns
    .filter((conn) => (conn.alts ?? []).some((alt) => normalizeAddress(alt) === address))
    .map((conn) => ({
      url: conn.url,
      alts: (conn.alts ?? []).filter((alt) => normalizeAddress(alt) !== address),
    }));
  const label = (() => {
    try {
      return new URL(address).host;
    } catch {
      return address;
    }
  })();
  return { kind: 'add', conn: { url: address, label, ...patch }, release };
}

/** Write `plan` to the saved machines and return the machine to open. */
export async function applyWebLaunch(plan: WebLaunchPlan): Promise<GatewayConn> {
  switch (plan.kind) {
    case 'replace':
      await removeConnection(plan.stale);
      // The gateway that held this device's push registration is gone from here.
      await clearRevocation(plan.stale);
      return applyWebLaunch(plan.then);
    case 'add': {
      for (const { url, alts } of plan.release) await updateConnection(url, { alts });
      await upsertConnection(plan.conn);
      return plan.conn;
    }
    case 'update': {
      const saved = await updateConnection(plan.url, plan.patch);
      return saved.find((conn) => conn.url === plan.url) ?? { url: plan.url, ...plan.patch };
    }
    case 'move': {
      const saved = await switchConnectionUrl(plan.from, plan.to, plan.patch);
      return saved.find((conn) => conn.url === plan.to) ?? { url: plan.to, ...plan.patch };
    }
  }
}

const connecting = new WeakMap<WebLaunch, Promise<GatewayConn>>();

/**
 * Redeem `launch` at `origin` and save its gateway. One launch connects once, also
 * when a development double render asks twice: the code itself works only once.
 */
export function connectWebLaunch(
  origin: string,
  launch: WebLaunch,
  fetchImpl: typeof fetch = fetch,
): Promise<GatewayConn> {
  let pending = connecting.get(launch);
  if (!pending) {
    pending = (async () => {
      const grant = await redeemWebLaunch(origin, launch, fetchImpl);
      return applyWebLaunch(planWebLaunch(await loadConnections(), origin, grant));
    })();
    connecting.set(launch, pending);
  }
  return pending;
}
