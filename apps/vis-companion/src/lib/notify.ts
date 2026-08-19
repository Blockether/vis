// Which paired machines may buzz this device.
//
// A push token belongs to the DEVICE, but a registration belongs to ONE gateway:
// every machine keeps its own device list and pushes from its own turns. So the
// app cannot treat notifications as a single app-wide switch — it has to bring
// EVERY paired gateway in line with the switch in that gateway's own settings.
// That switch starts at NO (`getGatewayNotify`): pairing a machine is not asking
// it to wake this device, so the sweep registers nothing until that machine's
// own Connect has been pressed — and unregisters a machine that has not been.
//
// The sweep runs on launch (and whenever the paired set changes) because both
// halves drift: the OS rotates the push token, so a gateway that should notify
// goes quiet holding a stale one, and a "stop notifying" that was made while the
// machine was unreachable never landed.
//
// It costs each machine ONE request. Reported as: four or five requests fly at
// every paired machine before a single row is painted. So the sweep ASKS first —
// one `GET /v1/devices`, which is also what the notifications row and the push
// banners are painted from — and writes only to a machine that disagrees with
// this device's switch. A fleet that already agrees is swept without a single
// write, and a panel opened on top of that read asks nothing at all.
//
// Forgetting a machine takes it out of the sweep for good, so the revocation it
// is owed cannot live in the paired set: it is stored by `removeConnection` and
// drained here by `drainPushRevocations`.

import {
  clearRevocation,
  getGatewayNotify,
  pendingRevocations,
  setGatewayNotify,
} from './storage';
import { isHeldBy, notifyVerdict, rememberNotifyVerdict } from './notify-verdict';
import type { GatewayConn, PushDevice, PushStatus } from './types';

/** One machine's answer: who it will wake, and whether it can wake anyone. */
export interface MachinePush {
  devices: PushDevice[];
  push?: PushStatus;
}

/** How this device is handed to — or taken from — ONE gateway. */
export interface FleetPush {
  /**
   * That machine's device list: the ONE request the sweep makes on it, shared
   * with every other reader of the same question (`gateway.ts`).
   */
  read: (conn: GatewayConn) => Promise<MachinePush>;
  register: (conn: GatewayConn) => Promise<unknown>;
  unregister: (conn: GatewayConn) => Promise<unknown>;
  /**
   * Whether a machine ALREADY holding this device must be re-registered anyway:
   * a relay grant carries its own expiry, and a machine handed a lapsed one goes
   * quiet on a device that never changed its mind. Answered from this device's
   * own storage (`lib/relay.ts`), so it costs no request.
   */
  isRenewalDue?: (
    conn: GatewayConn,
    push: PushStatus | undefined,
  ) => Promise<boolean>;
}

/** What the sweep found, and the little it had to change, per gateway URL. */
export interface PushSyncResult {
  registered: string[];
  unregistered: string[];
  /** Already in line with this device's switch: asked nothing but the one read. */
  unchanged: string[];
  failed: string[];
}

/**
 * Bring every paired machine in line with its own switch, and answer its
 * Notifications row, in ONE request each.
 *
 * Both jobs are the same question — is that machine holding this device? — so
 * they are one pass over the fleet. Asking it also ANSWERS the row before it is
 * opened: that row assembles its verdict from asynchronous answers, so a machine
 * whose Settings this device had never opened could only paint `Checking…` on
 * its first frame, a pulsing amber `Connect` that settled into a quiet
 * `Disconnect` a moment later. Reported as: it flickers, on every machine, every
 * time. The verdict is left where the row reads it synchronously
 * (`lib/notify-verdict.ts`), and it is computed by the SAME function the panel
 * uses, so a warm answer can never disagree with the panel that opens on it.
 *
 * What is asked of each machine:
 *
 *   * the OS silenced this app — nothing any machine holds can reach this
 *     device, so the whole fleet is answered without one round trip;
 *   * the machine already agrees with the switch — one read, nothing written;
 *   * it disagrees (or holds a grant about to lapse) — that one write, then the
 *     verdict its own panel would have settled on;
 *   * it does not answer at all — it keeps the verdict it last settled on. A
 *     warm cache is a frame, never a claim, and a guess would be painted as fact.
 *
 * One unreachable machine must not silence the rest, so every gateway is swept
 * independently and its failure is only reported: the next launch or wake tries
 * again.
 */
export async function syncFleetPush(
  conns: readonly GatewayConn[],
  fleet: FleetPush,
  /** Masked ids this device may appear under: its push token, and any grant. */
  ids: readonly string[],
  isBlocked: boolean,
  isCancelled: () => boolean = () => false,
): Promise<PushSyncResult> {
  const result: PushSyncResult = {
    registered: [],
    unregistered: [],
    unchanged: [],
    failed: [],
  };
  const seen = new Set<string>();
  for (const conn of conns) {
    if (isCancelled()) break;
    if (!conn.url || seen.has(conn.url)) continue;
    seen.add(conn.url);
    const isWanted = await getGatewayNotify(conn.url);
    if (isCancelled()) break;
    if (isBlocked) {
      rememberNotifyVerdict(conn.url, false);
      result.unchanged.push(conn.url);
      continue;
    }
    let state: MachinePush;
    try {
      state = await fleet.read(conn);
      // A machine too old for the route, or one whose answer carries no list at
      // all, has told us nothing about what it is holding.
      if (!Array.isArray(state.devices)) throw new Error('no device list');
    } catch {
      // Unreachable, or unreadable: this machine keeps the verdict it last
      // settled on, and the next launch or wake asks it again.
      result.failed.push(conn.url);
      continue;
    }
    if (isCancelled()) break;
    let isHeld = isHeldBy(state.devices, ids);
    try {
      const isRenewalDue =
        isHeld && (await fleet.isRenewalDue?.(conn, state.push)) === true;
      if (isWanted && (!isHeld || isRenewalDue)) {
        await fleet.register(conn);
        isHeld = true;
        result.registered.push(conn.url);
      } else if (!isWanted && isHeld) {
        await fleet.unregister(conn);
        isHeld = false;
        result.unregistered.push(conn.url);
      } else {
        result.unchanged.push(conn.url);
      }
    } catch {
      // The machine refused the one write it was owed. What it is holding is
      // still what was READ, so the row is answered with that and the next
      // launch or wake tries again.
      result.failed.push(conn.url);
    }
    rememberNotifyVerdict(conn.url, notifyVerdict({ isHeld, isWanted, isBlocked }));
  }
  return result;
}

/**
 * Take this device off every machine it was FORGOTTEN on.
 *
 * The sweep above can only reach machines that are still paired, and a machine
 * that was forgotten is by definition not one of them — while it goes on
 * holding this device's registration and pushing to it. `removeConnection`
 * therefore keeps that machine named, with the credential the DELETE needs, and
 * this drains what is owed.
 *
 * An entry is dropped only once that machine has accepted the revocation, so a
 * machine that was unreachable at the moment it was dropped is stopped on the
 * next launch or wake instead of buzzing for good. Returns the URLs that
 * accepted it.
 */
export async function drainPushRevocations(
  token: string,
  unregister: (conn: GatewayConn, token: string) => Promise<unknown>,
  isCancelled: () => boolean = () => false,
): Promise<string[]> {
  const revoked: string[] = [];
  for (const conn of await pendingRevocations()) {
    if (isCancelled()) break;
    try {
      await unregister(conn, token);
    } catch {
      // Still holding this device. Kept, and asked again next time.
      continue;
    }
    await clearRevocation(conn.url);
    revoked.push(conn.url);
  }
  return revoked;
}

/**
 * Record one gateway's switch, THEN try to assert it on that gateway.
 *
 * The order is the whole point. The durable half is the answer, not the call:
 * the moment you most want to silence a machine is often the moment it is
 * unreachable, and if the network call went first its failure would throw the
 * answer away — the machine would keep buzzing forever. Storing first means
 * `syncFleetPush` lands the choice on the next launch or wake.
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

