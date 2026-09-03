// Park notification taps until paired gateways and launch routing are ready. Resolve the
// sender by `gateway_id` at drain time; never route a session through whichever machine
// happens to be active, and drop senders no longer paired.

import type { GatewayConn } from './types';
import type { PushTap } from './push';

/**
 * How long a parked tap still steers navigation. A tap is user intent, but the
 * intent can outlive its usefulness: a launch that has no gateway to open it on
 * must not yank the user into an old session minutes later, when they finally
 * pair one.
 */
export const RESUMABLE_PUSH_MS = 5 * 60 * 1000;

/** A tapped notification, waiting for somewhere to land. */
export interface PushIntent {
  /** Session the alert was about. */
  sessionId: string;
  /** Gateway that sent it, when the payload named one. */
  gatewayId: string | null;
  /** When the tap arrived, for staleness. */
  at: number;
}

/** Everything the drain needs to decide, snapshotted at drain time. */
export interface PushIntentState {
  /**
   * The launch route has been applied. Draining earlier is pointless: the hash
   * the app booted with is applied once and would clear the session again.
   */
  isRouteApplied: boolean;
  /** Paired machines, as read back from storage. Empty until hydrated. */
  conns: GatewayConn[];
  /** The gateway the shell is currently pointed at, if one is resolved. */
  active: GatewayConn | null;
  /** Now, in ms. */
  now: number;
}

/**
 * `wait` keeps the intent parked for a later state change, `open` navigates,
 * `drop` forgets it. Only `wait` leaves the intent in place.
 */
export type PushIntentOutcome =
  | { action: 'wait' }
  | { action: 'drop' }
  | { action: 'open'; conn: GatewayConn; sid: string };

/** The intent a tap carries, or null when it names no session. */
export function pushIntentFrom(tap: PushTap, at: number): PushIntent | null {
  const sessionId = (tap.sessionId ?? '').trim();
  const gatewayId = (tap.gatewayId ?? '').trim();
  return sessionId ? { sessionId, gatewayId: gatewayId || null, at } : null;
}

/**
 * What to do with a parked tap right now.
 *
 * Waiting is the answer to every "not yet" — no machines read back, no route
 * applied, an id still missing off a paired machine — because those all resolve
 * moments later during a cold start, and that window is exactly where the tap
 * used to be dropped.
 */
export function resolvePushIntent(
  intent: PushIntent | null,
  state: PushIntentState,
): PushIntentOutcome {
  if (!intent) return { action: 'wait' };
  if (state.now - intent.at > RESUMABLE_PUSH_MS) return { action: 'drop' };
  if (!state.isRouteApplied) return { action: 'wait' };
  if (intent.gatewayId) {
    const named = state.conns.find((conn) => conn.id === intent.gatewayId);
    if (named) return { action: 'open', conn: named, sid: intent.sessionId };
    // Every paired machine has answered `/healthz` and none of them is the
    // sender: this phone is not paired with that gateway, and no other machine
    // can serve the session. Anything less than that is still hydrating.
    const isEveryConnIdentified = state.conns.length > 0 && state.conns.every((conn) => conn.id);
    return isEveryConnIdentified ? { action: 'drop' } : { action: 'wait' };
  }
  const conn = state.active ?? state.conns[0] ?? null;
  if (!conn) return { action: 'wait' };
  return { action: 'open', conn, sid: intent.sessionId };
}
