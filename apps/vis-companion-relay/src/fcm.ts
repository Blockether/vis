/**
 * Android half of the relay: FCM HTTP v1, authorised by an OAuth access token
 * minted from the service account's RS256 assertion. Plain HTTPS/1.1 — unlike
 * APNs this needs nothing special from the runtime.
 */

import { PROVIDER_TIMEOUT_MS } from "./apns";
import { signJwt } from "./jwt";
import type { Deps, Env, Notification, PushResult } from "./types";

const TOKEN_URI = "https://oauth2.googleapis.com/token";
const SCOPE = "https://www.googleapis.com/auth/firebase.messaging";
const ASSERTION_TTL_SECONDS = 3600;

export interface FcmConfig {
  projectId: string;
  clientEmail: string;
  privateKey: string;
}

export function fcmConfig(env: Env): FcmConfig | null {
  const raw = (env.FCM_SERVICE_ACCOUNT ?? "").trim();
  if (!raw) return null;
  try {
    const sa = JSON.parse(raw) as Record<string, string>;
    if (!sa.project_id || !sa.client_email || !sa.private_key) return null;
    return { projectId: sa.project_id, clientEmail: sa.client_email, privateKey: sa.private_key };
  } catch {
    return null;
  }
}

const accessTokens = new Map<string, { token: string; expiresAt: number }>();

export function resetAccessTokens(): void {
  accessTokens.clear();
}

/** Google's access tokens live an hour; refresh at 50 minutes. */
export async function accessToken(cfg: FcmConfig, deps: Deps): Promise<string | null> {
  const now = deps.now();
  const cached = accessTokens.get(cfg.clientEmail);
  if (cached && now < cached.expiresAt) return cached.token;

  const issuedAt = Math.floor(now / 1000);
  const assertion = await signJwt(
    "RS256",
    cfg.privateKey,
    { typ: "JWT" },
    {
      iss: cfg.clientEmail,
      scope: SCOPE,
      aud: TOKEN_URI,
      iat: issuedAt,
      exp: issuedAt + ASSERTION_TTL_SECONDS,
    },
  );

  const response = await deps.fetch(TOKEN_URI, {
    method: "POST",
    headers: { "content-type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({
      grant_type: "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion,
    }).toString(),
    signal: AbortSignal.timeout(PROVIDER_TIMEOUT_MS),
  });
  if (!response.ok) return null;
  const parsed = (await response.json()) as { access_token?: string };
  if (!parsed.access_token) return null;
  accessTokens.set(cfg.clientEmail, { token: parsed.access_token, expiresAt: now + 50 * 60 * 1000 });
  return parsed.access_token;
}

/** FCM rejects a `data` map whose values are not strings. */
function message(deviceToken: string, notification: Notification): unknown {
  const data: Record<string, string> = {};
  for (const [key, value] of Object.entries(notification.data ?? {})) data[key] = String(value);
  return {
    message: {
      token: deviceToken,
      notification: { title: notification.title, body: notification.body },
      data,
      android: {
        priority: "HIGH",
        notification: { sound: "default" },
        ...(notification.collapseId ? { collapse_key: notification.collapseId } : {}),
      },
    },
  };
}

export const FCM_DEAD_REASONS = new Set(["UNREGISTERED", "NOT_FOUND", "INVALID_ARGUMENT"]);

export async function sendFcm(
  cfg: FcmConfig,
  args: { deviceToken: string; notification: Notification },
  deps: Deps,
): Promise<PushResult> {
  try {
    const token = await accessToken(cfg, deps);
    if (!token) return { status: 0, reason: "oauth-failed" };
    const response = await deps.fetch(
      `https://fcm.googleapis.com/v1/projects/${cfg.projectId}/messages:send`,
      {
        method: "POST",
        headers: { authorization: `Bearer ${token}`, "content-type": "application/json" },
        body: JSON.stringify(message(args.deviceToken, args.notification)),
        signal: AbortSignal.timeout(PROVIDER_TIMEOUT_MS),
      },
    );
    const text = await response.text();
    let reason = "";
    try {
      const parsed = JSON.parse(text || "{}") as {
        error?: { status?: string; details?: { errorCode?: string }[] };
      };
      reason = String(parsed.error?.details?.[0]?.errorCode ?? parsed.error?.status ?? "");
    } catch {
      reason = "";
    }
    return { status: response.status, reason };
  } catch (error) {
    return { status: 0, reason: error instanceof Error ? error.message : "transport-error" };
  }
}
