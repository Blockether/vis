/** Shared shapes. Nothing here ever carries key material to a caller. */

export interface Env {
  DB: D1Database;
  /** The whole `AuthKey_<kid>.p8` PEM. A SECRET — `wrangler secret put`. */
  APNS_KEY_P8?: string;
  APNS_KEY_ID?: string;
  APNS_TEAM_ID?: string;
  APNS_TOPIC?: string;
  APNS_DEFAULT_ENV?: string;
  /** The whole Firebase service-account JSON. A SECRET. */
  FCM_SERVICE_ACCOUNT?: string;
  PUSH_RATE_LIMIT?: string;
  PUSH_RATE_WINDOW_MS?: string;
  GRANT_RATE_LIMIT?: string;
  MAX_GRANTS_PER_DEVICE?: string;
}

/** Everything non-deterministic, injected so a test can pin it. */
export interface Deps {
  fetch: typeof fetch;
  now: () => number;
  randomGrant: () => string;
}

export interface Notification {
  title: string;
  body: string;
  data?: Record<string, string>;
  threadId?: string;
  collapseId?: string;
  badge?: number;
  /** Ask iOS to run the Notification Service Extension (encrypted bodies). */
  isMutable?: boolean;
}

export interface PushResult {
  status: number;
  reason: string;
  /** The environment that actually worked, when it is not the stored one. */
  environment?: string;
}

export type Platform = "ios" | "ipados" | "android";

export const PLATFORMS: readonly Platform[] = ["ios", "ipados", "android"];
