// Which paired machines may buzz this device.
//
// A push token belongs to the DEVICE, but a registration belongs to ONE gateway:
// every machine keeps its own device list and pushes from its own turns. So the
// app cannot treat notifications as a single app-wide switch — it has to bring
// EVERY paired gateway in line with the switch in that gateway's own settings.
//
// The sweep runs on launch (and whenever the paired set changes) because both
// halves drift: the OS rotates the push token, so a gateway that should notify
// goes quiet holding a stale one, and a "stop notifying" that was made while the
// machine was unreachable never landed. Re-asserting both directions is cheap,
// idempotent, and heals both.

import { getGatewayNotify, setGatewayNotify } from './storage';
import type { GatewayConn } from './types';

/** How this device's token is handed to (or taken from) one gateway. */
export interface PushRegistrar {
  register: (conn: GatewayConn, token: string) => Promise<unknown>;
  unregister: (conn: GatewayConn, token: string) => Promise<unknown>;
}

/** What the sweep actually managed to assert, per gateway URL. */
export interface PushSyncResult {
  registered: string[];
  unregistered: string[];
  failed: string[];
}

/**
 * Assert each paired gateway's own notification switch for this device.
 *
 * One unreachable machine must not silence the rest, so every gateway is swept
 * independently and its failure is only reported: the next launch or wake tries
 * again.
 */
export async function syncPushRegistrations(
  conns: readonly GatewayConn[],
  token: string,
  registrar: PushRegistrar,
  isCancelled: () => boolean = () => false,
): Promise<PushSyncResult> {
  const result: PushSyncResult = { registered: [], unregistered: [], failed: [] };
  const seen = new Set<string>();
  for (const conn of conns) {
    if (isCancelled()) break;
    if (!conn.url || seen.has(conn.url)) continue;
    seen.add(conn.url);
    const wanted = await getGatewayNotify(conn.url);
    if (isCancelled()) break;
    try {
      if (wanted) {
        await registrar.register(conn, token);
        result.registered.push(conn.url);
      } else {
        await registrar.unregister(conn, token);
        result.unregistered.push(conn.url);
      }
    } catch {
      result.failed.push(conn.url);
    }
  }
  return result;
}

/**
 * Record one gateway's switch, THEN try to assert it on that gateway.
 *
 * The order is the whole point. The durable half is the answer, not the call:
 * the moment you most want to silence a machine is often the moment it is
 * unreachable, and if the network call went first its failure would throw the
 * answer away — the machine would keep buzzing forever. Storing first means
 * `syncPushRegistrations` lands the choice on the next launch or wake.
 *
 * The failure is still raised, so the caller can say the machine has not caught
 * up yet.
 */
export async function applyGatewayNotify(
  url: string,
  on: boolean,
  assert: () => Promise<unknown>,
): Promise<void> {
  await setGatewayNotify(url, on);
  await assert();
}

/** Store the web tab's per-gateway switch without contacting the native push API. */
export async function applyWebGatewayNotify(url: string, on: boolean): Promise<void> {
  await setGatewayNotify(url, on);
}
