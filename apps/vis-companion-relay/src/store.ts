/**
 * D1 access. A GRANT is the whole security model: an opaque bearer capability
 * naming exactly one device, handed to exactly one gateway, revocable on its
 * own. The relay stores `sha256(grant)`, so the database never holds a usable
 * capability — and neither does a backup of it.
 */

import { base64url, sha256Hex } from "./jwt";
import type { Deps, Platform } from "./types";

export interface GrantRow {
  id: string;
  device_token: string;
  platform: Platform;
  environment: string;
  label: string | null;
  created_at: number;
  last_push_at: number | null;
  push_count: number;
}

/** 32 bytes of entropy — the only thing standing between a caller and a push. */
export function randomGrant(): string {
  return base64url(crypto.getRandomValues(new Uint8Array(32)));
}

export function grantId(grant: string): Promise<string> {
  return sha256Hex(grant);
}

export async function createGrant(
  db: D1Database,
  args: { deviceToken: string; platform: Platform; environment: string; label?: string | null },
  deps: Deps,
  maxPerDevice: number,
): Promise<{ grant: string; row: GrantRow }> {
  const grant = deps.randomGrant();
  const id = await grantId(grant);
  const row: GrantRow = {
    id,
    device_token: args.deviceToken,
    platform: args.platform,
    environment: args.environment === "sandbox" ? "sandbox" : "production",
    label: args.label ?? null,
    created_at: deps.now(),
    last_push_at: null,
    push_count: 0,
  };
  await db
    .prepare(
      `INSERT INTO grants (id, device_token, platform, environment, label, created_at, push_count)
       VALUES (?, ?, ?, ?, ?, ?, 0)`,
    )
    .bind(row.id, row.device_token, row.platform, row.environment, row.label, row.created_at)
    .run();
  await trimDeviceGrants(db, args.platform, args.deviceToken, maxPerDevice);
  return { grant, row };
}

/**
 * A device pairs with several gateways, so several live grants per token is
 * normal — but not unbounded. Keep the newest `keep`.
 */
export async function trimDeviceGrants(
  db: D1Database,
  platform: Platform,
  deviceToken: string,
  keep: number,
): Promise<void> {
  await db
    .prepare(
      `DELETE FROM grants
        WHERE platform = ? AND device_token = ?
          AND id NOT IN (SELECT id FROM grants
                          WHERE platform = ? AND device_token = ?
                          ORDER BY created_at DESC, id DESC
                          LIMIT ?)`,
    )
    .bind(platform, deviceToken, platform, deviceToken, Math.max(1, keep))
    .run();
}

export async function findGrant(db: D1Database, grant: string): Promise<GrantRow | null> {
  const id = await grantId(grant);
  return await db.prepare("SELECT * FROM grants WHERE id = ?").bind(id).first<GrantRow>();
}

export async function revokeGrant(db: D1Database, grant: string): Promise<boolean> {
  const id = await grantId(grant);
  const result = await db.prepare("DELETE FROM grants WHERE id = ?").bind(id).run();
  return (result.meta?.changes ?? 0) > 0;
}

export async function deleteGrantById(db: D1Database, id: string): Promise<void> {
  await db.prepare("DELETE FROM grants WHERE id = ?").bind(id).run();
}

export async function notePush(db: D1Database, id: string, now: number): Promise<void> {
  await db
    .prepare("UPDATE grants SET last_push_at = ?, push_count = push_count + 1 WHERE id = ?")
    .bind(now, id)
    .run();
}

export async function setEnvironment(
  db: D1Database,
  id: string,
  environment: string,
): Promise<void> {
  await db.prepare("UPDATE grants SET environment = ? WHERE id = ?").bind(environment, id).run();
}

export interface QuotaVerdict {
  isAllowed: boolean;
  remaining: number;
  resetAt: number;
}

/**
 * One fixed window per subject. Abuse costs the abuser their own grant, not
 * everyone's push: a leaked grant is rate-limited and deletable, which is
 * precisely what a shared `.p8` can never be.
 */
export async function consumeQuota(
  db: D1Database,
  subject: string,
  limit: number,
  windowMs: number,
  now: number,
): Promise<QuotaVerdict> {
  const existing = await db
    .prepare("SELECT window_start, count FROM quota WHERE subject = ?")
    .bind(subject)
    .first<{ window_start: number; count: number }>();

  const isFresh = !existing || now - existing.window_start >= windowMs;
  const windowStart = isFresh ? now : existing.window_start;
  const count = (isFresh ? 0 : existing.count) + 1;
  const resetAt = windowStart + windowMs;

  if (count > limit) return { isAllowed: false, remaining: 0, resetAt };

  await db
    .prepare(
      `INSERT INTO quota (subject, window_start, count) VALUES (?, ?, ?)
         ON CONFLICT(subject) DO UPDATE SET window_start = excluded.window_start,
                                            count = excluded.count`,
    )
    .bind(subject, windowStart, count)
    .run();
  return { isAllowed: true, remaining: limit - count, resetAt };
}
