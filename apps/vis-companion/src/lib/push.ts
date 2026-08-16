/**
 * Native push notifications — the device half of
 * `com.blockether.vis.internal.gateway.push`.
 *
 * The gateway pushes exactly once per terminal turn event, to every device
 * token registered with it. This module owns the OS side of that contract:
 * permission, APNs/FCM registration, and the tap that reopens the session the
 * alert came from. It never talks to the gateway itself — `GatewayClient`
 * does that — so the same token can be registered with several gateways.
 *
 * On the web, `web-push.ts` owns the service worker and gateway-local Web Push
 * subscription. This module only owns native APNs/FCM registration.
 */

import { Capacitor } from '@capacitor/core';
import { PushNotifications } from '@capacitor/push-notifications';
import type { PushNotificationSchema } from '@capacitor/push-notifications';
import { APP_NAME, APP_VERSION } from './compat';
import type { PushDeviceInput } from './types';

/** OS permission for alerts, plus the platform verdict browsers get. */
export type PushPermission = 'granted' | 'denied' | 'prompt' | 'unsupported';

/** What a tapped notification tells the app to open. */
export interface PushTap {
  sessionId?: string;
  /**
   * Gateway instance id (`/healthz` `id`) that sent the alert. A session id
   * only means anything on the gateway that minted it, so this is what decides
   * WHICH paired machine the tap opens.
   */
  gatewayId?: string;
  turnId?: string;
  status?: string;
  type?: string;
}

/** True only where a real OS push token can exist (iOS / Android native). */
export function isPushSupported(): boolean {
  return Capacitor.isNativePlatform() && Capacitor.isPluginAvailable('PushNotifications');
}

/**
 * Whether this device has a door to its own notification settings.
 *
 * iOS is the one platform that can be sent there from the webview: Capacitor's
 * navigation delegate hands a top-level navigation it cannot serve to
 * `UIApplication.shared.open` (`@capacitor/ios` `WebViewDelegationHandler`),
 * which lands on this app's page in Settings. Android needs an Intent and has no
 * URL for it, so the verb is not offered there — the banner carries the whole
 * instruction instead of a button that would do nothing.
 */
export function canOpenSystemNotificationSettings(): boolean {
  return isPushSupported() && Capacitor.getPlatform() === 'ios';
}

/** Opens this app's page in system Settings, where such a door exists. */
export function openSystemNotificationSettings(): void {
  if (!canOpenSystemNotificationSettings()) return;
  window.location.href = 'app-settings:';
}

/**
 * APNs has two disjoint token spaces. A debug build installed from Xcode gets a
 * sandbox token; TestFlight and the App Store get production ones — sending to
 * the wrong one fails with `BadDeviceToken`, so the app declares which it holds
 * rather than letting the gateway guess. (The gateway still retries the other
 * environment once, which is what makes a mislabelled dev build survive.)
 */
export function pushEnvironment(): 'sandbox' | 'production' {
  return import.meta.env.DEV ? 'sandbox' : 'production';
}

/**
 * Which push provider this device belongs to: `ios`/`ipados` are delivered via
 * APNs, `android` via FCM. The gateway dispatches on exactly this value, so the
 * panel reads the matching half of its status instead of the APNs one.
 */
export function pushPlatform(): string {
  return Capacitor.getPlatform();
}

/** Same masking as the gateway, so a device can find ITS row in `/v1/devices`. */
export function maskToken(token: string): string {
  return token.length <= 12 ? '…' : `${token.slice(0, 6)}…${token.slice(-4)}`;
}

let lastToken: string | null = null;

/** The token this app last obtained from the OS, if any. */
export function cachedPushToken(): string | null {
  return lastToken;
}

export async function pushPermission(): Promise<PushPermission> {
  if (!isPushSupported()) return 'unsupported';
  const { receive } = await PushNotifications.checkPermissions();
  return receive as PushPermission;
}

/**
 * The one Android channel every Vis alert is filed under.
 *
 * `scripts/android-prepare.mjs` reads THIS literal and stamps it into the
 * manifest as Firebase's `default_notification_channel_id`, so the id the app
 * creates and the id the tray files alerts under cannot drift apart.
 */
export const PUSH_CHANNEL_ID = 'vis_answers';

/**
 * Create that channel, on Android, before any alert can arrive.
 *
 * Android 8+ posts nothing without a channel, so an app that declares none
 * hands the decision to Firebase: it files every alert under its own
 * `fcm_fallback_notification_channel` — shown to the reader as "Miscellaneous",
 * at default importance, with no description and no heads-up banner. The reader
 * who wants to keep answers and drop everything else has nothing to switch.
 *
 * Re-creating it on every launch is safe: Android keeps a channel's user edits
 * once it exists, so this never overrides a reader who turned the sound off.
 */
export async function ensureAndroidChannel(): Promise<void> {
  if (!isPushSupported() || Capacitor.getPlatform() !== 'android') return;
  try {
    await PushNotifications.createChannel({
      id: PUSH_CHANNEL_ID,
      name: 'Answers',
      description: 'One alert per finished turn: a session on a paired machine answered you.',
      importance: 4, // HIGH — a finished answer earns a heads-up banner
      vibration: true,
    });
  } catch {
    // A device that refuses the channel still gets the alert through Firebase's
    // fallback; tray plumbing must never break push registration.
  }
}

/**
 * Ask for permission (if not already answered) and register with APNs/FCM,
 * resolving the device token. Rejects with a human-readable reason — a denied
 * permission is a normal outcome the UI must explain, not a crash.
 *
 * `register()` is fire-and-forget in Capacitor: the token arrives on the
 * `registration` listener, so this bridges that back into a promise and gives
 * up rather than hanging forever if the OS never answers.
 */
export async function acquirePushToken(timeoutMs = 15000): Promise<string> {
  if (!isPushSupported()) {
    throw new Error('Push notifications need the native iOS or Android app.');
  }
  let perm = await PushNotifications.checkPermissions();
  if (perm.receive === 'prompt' || perm.receive === 'prompt-with-rationale') {
    perm = await PushNotifications.requestPermissions();
  }
  if (perm.receive !== 'granted') {
    throw new Error('Notifications are turned off for Vis in system Settings.');
  }

  return await new Promise<string>((resolve, reject) => {
    let settled = false;
    const handles: { remove: () => Promise<void> }[] = [];
    const done = (fn: () => void) => {
      if (settled) return;
      settled = true;
      window.clearTimeout(timer);
      for (const h of handles) void h.remove();
      fn();
    };
    const timer = window.setTimeout(
      () => done(() => reject(new Error('The device never returned a push token.'))),
      timeoutMs,
    );

    void PushNotifications.addListener('registration', (token) => {
      lastToken = token.value;
      done(() => resolve(token.value));
    }).then((h) => handles.push(h));
    void PushNotifications.addListener('registrationError', (err) => {
      done(() => reject(new Error(String(err?.error ?? 'push registration failed'))));
    }).then((h) => handles.push(h));

    void PushNotifications.register().catch((e: unknown) =>
      done(() => reject(new Error((e as Error).message))),
    );
  });
}

/** Everything the gateway stores about this device, for `POST /v1/devices`. */
export function deviceRegistration(token: string): PushDeviceInput {
  return {
    token,
    platform: Capacitor.getPlatform(),
    environment: pushEnvironment(),
    client: APP_NAME,
    client_version: APP_VERSION,
    label: deviceLabel(),
    bundle_id: undefined,
  };
}

/**
 * Run `handler` when the user taps one of our notifications. Returns an
 * unsubscribe fn; a no-op on the web.
 */
export function onPushTap(handler: (tap: PushTap) => void): () => void {
  if (!isPushSupported()) return () => undefined;
  let handle: { remove: () => Promise<void> } | null = null;
  let cancelled = false;
  void PushNotifications.addListener('pushNotificationActionPerformed', (action) => {
    const data = (action?.notification?.data ?? {}) as Record<string, unknown>;
    handler({
      sessionId: str(data.session_id),
      gatewayId: str(data.gateway_id),
      turnId: str(data.turn_id),
      status: str(data.status),
      type: str(data.type),
    });
  }).then((h) => {
    if (cancelled) void h.remove();
    else handle = h;
  });
  return () => {
    cancelled = true;
    if (handle) void handle.remove();
  };
}

/**
 * Drop the delivered alerts the reader has dealt with, and answer how many are
 * still waiting.
 *
 * Notification Center is what the badge counts — `VisNotify`, the service
 * extension, sets the number to the alerts sitting in the tray plus the one
 * arriving, and `lib/badge.ts` writes the same tally from the other side. So
 * the tray must hold exactly the answers still owed to the reader.
 *
 * On Android the tray is the WHOLE badge: the launcher dots the icon while this
 * app holds a notification and there is no number to write, so this tidy is the
 * only thing that ever puts the dot out.
 *
 * This used to clear it wholesale on every foreground, which threw away the
 * alerts for every session you had NOT opened and left the badge counting from
 * zero again.
 */
export async function dropDeliveredPushes(
  isDone: (sessionId: string | undefined) => boolean,
): Promise<number> {
  if (!isPushSupported()) return 0;
  try {
    const { notifications } = await PushNotifications.getDeliveredNotifications();
    const done = notifications.filter((notification) => isDone(deliveredSession(notification)));
    if (done.length > 0) await PushNotifications.removeDeliveredNotifications({ notifications: done });
    return notifications.length - done.length;
  } catch {
    // Purely cosmetic — never let tray housekeeping break a foreground resume.
    return 0;
  }
}

function str(v: unknown): string | undefined {
  return typeof v === 'string' && v ? v : undefined;
}

/**
 * Which session a delivered alert belongs to.
 *
 * iOS keeps the whole payload, so it is in `data`. Android does not: Firebase
 * builds the tray entry itself and copies only its own `android.*` keys into
 * the notification, so by the time this app can list what was delivered the
 * data map is gone. The gateway therefore sends the session id as the
 * notification TAG, which survives — and, being a tag, keeps one live alert per
 * session instead of a pile the reader has to swipe away one by one.
 */
function deliveredSession(notification: PushNotificationSchema): string | undefined {
  return str(notification.data?.session_id) ?? str(notification.tag);
}

/** A name the user will recognise in the gateway's device list. */
function deviceLabel(): string {
  const platform = Capacitor.getPlatform();
  const ua = navigator.userAgent;
  const model = /iPhone/.test(ua) ? 'iPhone' : /iPad/.test(ua) ? 'iPad' : platform;
  return `${model} · ${APP_NAME}`;
}
