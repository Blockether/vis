/**
 * The device owns its absolute badge count across all gateways. iOS reconciles unread
 * sessions with delivered notifications; Android removes tagged notifications so its
 * launcher dot reflects the same remaining set.
 */
import { Capacitor, registerPlugin } from '@capacitor/core';
import type { FleetMachine } from './fleet';
import { dropDeliveredPushes } from './push';
import { hasSessionReadMark, unreadTurnCount } from './unread';

/** The whole native surface: one verb, no state. */
interface VisBadgePlugin {
  set(options: { count: number }): Promise<void>;
}

const VisBadge = registerPlugin<VisBadgePlugin>('VisBadge');

/**
 * Only iOS has a number to write. An Android launcher owns its badge and
 * derives it from the tray, so there `syncBadge`'s tidy is the whole of it.
 */
export function isBadgeSupported(): boolean {
  return Capacitor.getPlatform() === 'ios' && Capacitor.isPluginAvailable('VisBadge');
}

/** What the app believes, and what the OS was last told. */
let desired = 0;
let written: number | null = null;

/**
 * Tell the OS a number. Repeating one already showing costs no bridge hop —
 * this is called on every fleet change, which on a busy machine is every poll.
 */
export async function setBadge(count: number): Promise<void> {
  const next = Number.isFinite(count) && count > 0 ? Math.floor(count) : 0;
  desired = next;
  if (!isBadgeSupported() || written === next) return;
  written = next;
  try {
    await VisBadge.set({ count: next });
  } catch {
    // A build without the VisBadge target answers "not implemented". The badge
    // is never worth an unhandled rejection; forget the write so a later app
    // that does have it starts from the truth.
    written = null;
  }
}

/**
 * Say it again, whatever we think is showing.
 *
 * While the app was away the service extension moved the badge without telling
 * anyone here, so the cached value is a lie the moment the app resumes.
 */
export async function reassertBadge(): Promise<void> {
  written = null;
  await setBadge(desired);
}

/**
 * The one call the screen makes: reconcile Notification Center and the badge
 * with the fleet on screen.
 *
 * A machine that is not answering is skipped for the same reason its rows
 * leave the `All` view — its counts are last week's news. An alert is only
 * dropped when this device KNOWS its session and knows it is no longer unread;
 * an alert for a session outside the loaded window is left exactly where the
 * OS put it.
 *
 * What survives that tidying is the badge: the alerts still waiting, which is
 * exactly what `VisNotify` counts when the next one arrives.
 */
export async function syncBadge(machines: readonly FleetMachine[]): Promise<void> {
  const known = new Set<string>();
  const unread = new Set<string>();
  for (const machine of machines) {
    if (machine.error) continue;
    for (const session of machine.sessions ?? []) {
      // Until durable marks have loaded, this device does not KNOW the alert was read.
      if (!hasSessionReadMark(session.id)) continue;
      known.add(session.id);
      if (unreadTurnCount(session) > 0) unread.add(session.id);
    }
  }
  const waiting = await dropDeliveredPushes(
    (sessionId) => sessionId !== undefined && known.has(sessionId) && !unread.has(sessionId),
  );
  await setBadge(waiting);
}
