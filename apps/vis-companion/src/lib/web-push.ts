import { Capacitor } from "@capacitor/core";
import {
  clearRevocation,
  getGatewayNotify,
  pendingRevocations,
} from "./storage";
import type { PushGateway } from "./relay";
import { GatewayClient } from "./gateway";
import { type PushPermission } from "./push";
import type { GatewayConn, PushDeviceInput, PushStatus } from "./types";

export interface WebPushSubscriptionJSON {
  endpoint: string;
  expirationTime?: number | null;
  keys: {
    p256dh: string;
    auth: string;
  };
}

export function isWebNotificationsPlatform(): boolean {
  return !Capacitor.isNativePlatform();
}

export function isWebPushSupported(): boolean {
  return (
    isWebNotificationsPlatform() &&
    typeof globalThis.Notification !== "undefined" &&
    typeof globalThis.PushManager !== "undefined" &&
    typeof navigator !== "undefined" &&
    "serviceWorker" in navigator
  );
}

export function webPushPermission(): PushPermission {
  if (!isWebPushSupported()) return "unsupported";
  return globalThis.Notification.permission as PushPermission;
}

export async function requestWebPushPermission(): Promise<PushPermission> {
  if (!isWebPushSupported()) return "unsupported";
  return (await globalThis.Notification.requestPermission()) as PushPermission;
}

function gatewayScopeId(gatewayUrl: string): string {
  const bytes = new TextEncoder().encode(gatewayUrl);
  let binary = "";
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/g, "");
}

/** The root worker is installed on app startup; push registrations use one scope per gateway. */
export async function registerWebServiceWorker(
  gatewayUrl?: string,
): Promise<ServiceWorkerRegistration | null> {
  if (!isWebNotificationsPlatform() || typeof navigator === "undefined" || !navigator.serviceWorker)
    return null;
  if (!gatewayUrl) return navigator.serviceWorker.register("/sw.js", { scope: "/" });
  const scope = `/__vis_push/${gatewayScopeId(gatewayUrl)}/`;
  const script = `/sw.js?gateway=${encodeURIComponent(gatewayUrl)}`;
  return navigator.serviceWorker.register(script, { scope });
}

export async function getExistingWebPushSubscription(
  gatewayUrl?: string,
): Promise<PushSubscription | null> {
  if (!isWebPushSupported()) return null;
  try {
    const registration = await registerWebServiceWorker(gatewayUrl);
    return registration?.pushManager.getSubscription() ?? null;
  } catch {
    return null;
  }
}

export function webPushSubscriptionJSON(
  subscription: PushSubscription,
): WebPushSubscriptionJSON {
  const json = subscription.toJSON();
  const endpoint = json.endpoint ?? subscription.endpoint;
  const p256dh = json.keys?.p256dh ?? "";
  const auth = json.keys?.auth ?? "";
  if (!endpoint || !p256dh || !auth)
    throw new Error("This browser returned an incomplete Web Push subscription.");
  return {
    endpoint,
    expirationTime: json.expirationTime,
    keys: { p256dh, auth },
  };
}

export function webPushToken(subscription: PushSubscription): string {
  return JSON.stringify(webPushSubscriptionJSON(subscription));
}

export function webPushDeviceRegistration(subscription: PushSubscription): PushDeviceInput {
  return {
    token: webPushToken(subscription),
    platform: "web",
    client: "vis-companion",
  };
}

function base64urlBytes(value: string): ArrayBuffer {
  const normalized = value.replace(/-/g, "+").replace(/_/g, "/").padEnd(Math.ceil(value.length / 4) * 4, "=");
  const binary = atob(normalized);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
  return bytes.buffer;
}

export function webPushApplicationServerKey(status: PushStatus): string {
  const key = status.web_push?.application_server_key;
  if (status.web_push?.is_available !== true || typeof key !== "string" || !key)
    throw new Error("Web Push is not configured on this gateway.");
  return key;
}

export async function ensureWebPushSubscription(
  gatewayUrl: string,
  applicationServerKey: string,
): Promise<PushSubscription> {
  if (!isWebPushSupported()) throw new Error("This browser does not support background Web Push.");
  const permission = await requestWebPushPermission();
  if (permission !== "granted")
    throw new Error("Notifications are blocked in this browser. Allow them in browser settings first.");
  const registration = await registerWebServiceWorker(gatewayUrl);
  if (!registration) throw new Error("The Web Push service worker could not be registered.");
  const existing = await registration.pushManager.getSubscription();
  if (existing) return existing;
  return registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: base64urlBytes(applicationServerKey),
  });
}

function gatewayPushTarget(conn: GatewayConn): PushGateway {
  return new GatewayClient(conn).pushTarget();
}

export async function registerWebPushForGateway(
  conn: GatewayConn,
  subscription: PushSubscription,
): Promise<void> {
  await gatewayPushTarget(conn).register(webPushDeviceRegistration(subscription));
}

export async function unregisterWebPushForGateway(
  conn: GatewayConn,
  subscription: PushSubscription,
): Promise<void> {
  await gatewayPushTarget(conn).unregister(webPushToken(subscription));
}

export async function syncWebPushRegistrations(
  conns: readonly GatewayConn[],
  isCancelled: () => boolean = () => false,
): Promise<void> {
  const seen = new Set<string>();
  for (const conn of conns) {
    if (isCancelled() || !conn.url || seen.has(conn.url)) continue;
    seen.add(conn.url);
    try {
      const target = gatewayPushTarget(conn);
      if (await getGatewayNotify(conn.url)) {
        if (webPushPermission() !== "granted") continue;
        const subscription = await getExistingWebPushSubscription(conn.url);
        if (subscription) await target.register(webPushDeviceRegistration(subscription));
      } else {
        const subscription = await getExistingWebPushSubscription(conn.url);
        if (subscription) await target.unregister(webPushToken(subscription));
      }
    } catch {
      // An unreachable or older gateway is retried on the next wake or settings visit.
    }
  }
}

/**
 * Take this browser off every gateway it was FORGOTTEN on.
 *
 * The same contract as the native drain (see lib/notify.ts): the subscription
 * lives on the GATEWAY, and forgetting the pairing is exactly what removes that
 * gateway from the sweep, so what is owed is stored with the credential and
 * drained here until it lands.
 */
export async function drainWebPushRevocations(
  isCancelled: () => boolean = () => false,
): Promise<void> {
  for (const conn of await pendingRevocations()) {
    if (isCancelled()) break;
    try {
      const subscription = await getExistingWebPushSubscription(conn.url);
      if (subscription)
        await gatewayPushTarget(conn).unregister(webPushToken(subscription));
      await clearRevocation(conn.url);
    } catch {
      // That gateway is still holding it; asked again on the next wake.
    }
  }
}
