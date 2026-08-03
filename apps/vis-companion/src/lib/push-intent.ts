// Opening the session a tapped notification is about.
//
// A tap is a HANDOFF across a cold start, not an event the app can handle where
// it arrives. Capacitor RETAINS `pushNotificationActionPerformed` until the
// first listener consumes it (`retainUntilConsumed` on iOS,
// `notifyListeners(..., true)` on Android), so a tap that launched the app is
// replayed into the listener that attaches on the first render — before the
// saved machines have been read back off the bridge, and before the launch
// route has been applied. Reading the active gateway right there finds `null`,
// and returning CONSUMES the tap: the app opens on the session list and the
// notification silently does nothing.
//
// So the tap is parked as an intent and drained once there is something to open
// it with. The gateway is resolved at DRAIN time for the same reason — it does
// not exist yet at tap time.
//
// Push payloads carry no gateway id (see `gateway/push.clj`: `session_id`,
// `turn_id`, `status`, `type`), so the intent opens on the active gateway, or
// the only paired one when the active gateway has not been chosen yet.

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
  return sessionId ? { sessionId, at } : null;
}

/**
 * What to do with a parked tap right now.
 *
 * Waiting is the answer to every "not yet" — no machines read back, no route
 * applied — because those all resolve moments later during a cold start, and
 * that window is exactly where the tap used to be dropped.
 */
export function resolvePushIntent(
  intent: PushIntent | null,
  state: PushIntentState,
): PushIntentOutcome {
  if (!intent) return { action: 'wait' };
  if (state.now - intent.at > RESUMABLE_PUSH_MS) return { action: 'drop' };
  if (!state.isRouteApplied) return { action: 'wait' };
  const conn = state.active ?? state.conns[0] ?? null;
  if (!conn) return { action: 'wait' };
  return { action: 'open', conn, sid: intent.sessionId };
}
