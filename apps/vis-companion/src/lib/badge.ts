/**
 * The icon badge — how many alerts you have not dealt with.
 *
 * iOS paints the badge from ONE number, `aps.badge`, and it is ABSOLUTE: APNs
 * never increments anything. That is why no gateway can supply it. This phone
 * is paired with several, each one knowing only its own sessions, so the last
 * push to arrive would overwrite every other machine's count and the number
 * would mean nothing.
 *
 * Two halves that can see the whole device own it instead:
 *
 *   - `VisNotify` (`scripts/ios-prepare.mjs` stamps
 *     `ios/App/VisNotify/NotificationService.swift`) runs inside every arriving
 *     alert and counts the alerts still waiting in Notification Center, plus
 *     the one being delivered;
 *   - this module writes what that tray holds once the app has tidied it: the
 *     alerts of sessions the reader has since dealt with are removed, and what
 *     is left IS the number.
 *
 * Both halves therefore count one set — the notifications still waiting — so
 * the number never jumps when one writes after the other. The list's own unread
 * marks decide what counts as dealt with.
 */
import { Capacitor, registerPlugin } from '@capacitor/core';
import type { FleetMachine } from './fleet';
import { dropDeliveredPushes } from './push';
import { unreadTurnCount } from './unread';

/** The whole native surface: one verb, no state. */
interface VisBadgePlugin {
  set(options: { count: number }): Promise<void>;
}

const VisBadge = registerPlugin<VisBadgePlugin>('VisBadge');

/**
 * iOS only. An Android launcher badges itself from the notification FCM
 * delivers, so writing a number there would be a second, competing source.
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
      known.add(session.id);
      if (unreadTurnCount(session) > 0) unread.add(session.id);
    }
  }
  const waiting = await dropDeliveredPushes(
    (sessionId) => sessionId !== undefined && known.has(sessionId) && !unread.has(sessionId),
  );
  await setBadge(waiting);
}
