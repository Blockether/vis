/**
 * APNs half of the relay: the ES256 provider token and one HTTP/2 POST.
 *
 * `fetch` in a Cloudflare Worker speaks HTTP/2 to `api.push.apple.com`, which
 * is the entire reason this relay can be a Worker. `wrangler dev` WITHOUT
 * `--remote` runs workerd locally, which does NOT — the identical code fails
 * there and works when deployed (workerd#4841). Use `wrangler dev --remote`.
 */

import { isPkcs8Pem, signJwt } from "./jwt";
import type { Deps, Env, Notification, PushResult } from "./types";

export const APNS_PRODUCTION_HOST = "https://api.push.apple.com";
export const APNS_SANDBOX_HOST = "https://api.sandbox.push.apple.com";

/**
 * Apple rejects a provider token older than one hour with `ExpiredProviderToken`
 * and rejects re-minting more often than every 20 minutes with
 * `TooManyProviderTokenUpdates`. 45 minutes sits inside both.
 */
export const JWT_TTL_MS = 45 * 60 * 1000;

/**
 * A provider that stops answering must not hold this request open: the Worker's
 * CPU and wall time is exactly what an attacker would otherwise be spending.
 */
export const PROVIDER_TIMEOUT_MS = 10_000;

export interface ApnsConfig {
  keyP8: string;
  keyId: string;
  teamId: string;
  topic: string;
  defaultEnvironment: string;
}

export function apnsConfig(env: Env): ApnsConfig | null {
  const keyP8 = (env.APNS_KEY_P8 ?? "").trim();
  const keyId = (env.APNS_KEY_ID ?? "").trim();
  const teamId = (env.APNS_TEAM_ID ?? "").trim();
  const topic = (env.APNS_TOPIC ?? "").trim();
  if (!keyP8 || !keyId || !teamId || !topic) return null;
  if (!isPkcs8Pem(keyP8)) return null;
  return {
    keyP8,
    keyId,
    teamId,
    topic,
    defaultEnvironment: env.APNS_DEFAULT_ENV === "sandbox" ? "sandbox" : "production",
  };
}

const providerTokens = new Map<string, { jwt: string; mintedAt: number }>();

/** Only a test needs this: the cache outlives a request inside one isolate. */
export function resetProviderTokens(): void {
  providerTokens.clear();
}

export async function providerToken(cfg: ApnsConfig, now: number): Promise<string> {
  const cacheKey = `${cfg.teamId}:${cfg.keyId}`;
  const cached = providerTokens.get(cacheKey);
  if (cached && now - cached.mintedAt < JWT_TTL_MS) return cached.jwt;
  const jwt = await signJwt(
    "ES256",
    cfg.keyP8,
    { kid: cfg.keyId },
    { iss: cfg.teamId, iat: Math.floor(now / 1000) },
  );
  providerTokens.set(cacheKey, { jwt, mintedAt: now });
  return jwt;
}

export function apnsHost(environment: string): string {
  return environment === "sandbox" ? APNS_SANDBOX_HOST : APNS_PRODUCTION_HOST;
}

/**
 * The `aps` keys Apple defines are kebab-case and it SILENTLY ignores anything
 * else, so `thread-id` and `mutable-content` must be spelled exactly.
 */
export function apnsPayload(notification: Notification): string {
  const aps: Record<string, unknown> = {
    alert: { title: notification.title, body: notification.body },
    sound: "default",
    "interruption-level": "active",
  };
  if (notification.threadId) aps["thread-id"] = notification.threadId;
  if (typeof notification.badge === "number") aps.badge = notification.badge;
  if (notification.isMutable) aps["mutable-content"] = 1;
  return JSON.stringify({ aps, ...(notification.data ?? {}) });
}

async function post(
  cfg: ApnsConfig,
  environment: string,
  deviceToken: string,
  notification: Notification,
  deps: Deps,
): Promise<PushResult> {
  const headers: Record<string, string> = {
    authorization: `bearer ${await providerToken(cfg, deps.now())}`,
    "apns-topic": cfg.topic,
    "apns-push-type": "alert",
    "apns-priority": "10",
    "content-type": "application/json",
  };
  if (notification.collapseId) headers["apns-collapse-id"] = notification.collapseId.slice(0, 64);

  try {
    const response = await deps.fetch(
      `${apnsHost(environment)}/3/device/${encodeURIComponent(deviceToken)}`,
      {
        method: "POST",
        headers,
        body: apnsPayload(notification),
        signal: AbortSignal.timeout(PROVIDER_TIMEOUT_MS),
      },
    );
    const text = await response.text();
    let reason = "";
    try {
      reason = String((JSON.parse(text || "{}") as { reason?: string }).reason ?? "");
    } catch {
      reason = "";
    }
    return { status: response.status, reason, environment };
  } catch (error) {
    return { status: 0, reason: error instanceof Error ? error.message : "transport-error" };
  }
}

/** APNs' verdicts that mean the registration is gone for good. */
export const APNS_DEAD_REASONS = new Set(["BadDeviceToken", "Unregistered", "DeviceTokenNotForTopic"]);

const APNS_WRONG_ENVIRONMENT = new Set(["BadDeviceToken", "BadEnvironmentKeyInToken"]);

/**
 * Send, retrying once against the other environment. A TestFlight build
 * registered as `sandbox` (or the reverse) is the single most common
 * misconfiguration, and the retry is what keeps it working anyway. The relay
 * stores nothing, so the correction is never remembered: such a grant pays the
 * extra round trip on every push until the app re-registers and is handed a
 * grant sealed with the environment that actually answered.
 */
export async function sendApns(
  cfg: ApnsConfig,
  args: { deviceToken: string; environment: string; notification: Notification },
  deps: Deps,
): Promise<PushResult> {
  const environment = args.environment === "sandbox" ? "sandbox" : "production";
  const attempt = await post(cfg, environment, args.deviceToken, args.notification, deps);
  if (attempt.status === 200 || !APNS_WRONG_ENVIRONMENT.has(attempt.reason)) return attempt;
  const other = environment === "sandbox" ? "production" : "sandbox";
  const retry = await post(cfg, other, args.deviceToken, args.notification, deps);
  return retry.status === 200 ? retry : attempt;
}
