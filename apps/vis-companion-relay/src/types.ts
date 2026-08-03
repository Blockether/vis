/** Shared shapes. Nothing here ever carries key material to a caller. */

export interface Env {
  /**
   * The seal. 32+ random bytes, `wrangler secret put RELAY_SEAL_KEY` — the one
   * piece of state this relay has, and it lives in Cloudflare's secret store
   * rather than in any database. Rotating it invalidates every grant at once.
   */
  RELAY_SEAL_KEY?: string;
  /** The outgoing key during a rotation: opened, never issued. */
  RELAY_SEAL_KEY_PREVIOUS?: string;
  /** How long a freshly minted grant stays valid. Default 90 days. */
  GRANT_TTL_DAYS?: string;

  /** The whole `AuthKey_<kid>.p8` PEM. A SECRET — `wrangler secret put`. */
  APNS_KEY_P8?: string;
  APNS_KEY_ID?: string;
  APNS_TEAM_ID?: string;
  APNS_TOPIC?: string;
  APNS_DEFAULT_ENV?: string;
  /** The whole Firebase service-account JSON. A SECRET. */
  FCM_SERVICE_ACCOUNT?: string;

  /**
   * Metering, from the platform rather than from a table: Cloudflare's rate
   * limiting bindings keep their counters at the edge, so refusing a flood
   * costs no storage operation at all. Declared in `wrangler.jsonc`.
   */
  MINT_LIMIT: RateLimit;
  PUSH_ADDRESS_LIMIT: RateLimit;
  PUSH_DEVICE_LIMIT: RateLimit;
}

/** Everything non-deterministic, injected so a test can pin it. */
export interface Deps {
  fetch: typeof fetch;
  now: () => number;
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
  /** The environment that actually worked, when it is not the sealed one. */
  environment?: string;
}

export type Platform = "ios" | "ipados" | "android";

export const PLATFORMS: readonly Platform[] = ["ios", "ipados", "android"];
