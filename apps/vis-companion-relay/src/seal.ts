/**
 * A grant is not a database key. It IS the record.
 *
 * The relay used to keep a D1 row per grant — device token, platform,
 * environment — and hand out a random string that pointed at it. That table
 * was the single most valuable thing this service owned: every user's push
 * token, at rest, in one file, forever. Deleting the table deletes the breach.
 *
 * So the grant carries its own contents, sealed with AES-256-GCM under a key
 * that exists only as a Worker secret:
 *
 *   vg1.<base64url( iv(12) || AES-GCM(device token, platform, env, expiry) )>
 *
 * The consequences are the whole security argument of the relay:
 *
 *   * the gateway holding the grant cannot read the device token out of it —
 *     it is ciphertext, not an encoding;
 *   * a grant cannot be forged, retargeted at another device, or given a later
 *     expiry: GCM authenticates every byte, `additionalData` pins the version;
 *   * a grant expires by itself, because the expiry travels inside it and the
 *     relay needs no calendar and no sweeper to enforce it;
 *   * there is nothing to dump, nothing to back up, nothing to migrate, and no
 *     row anyone can exhaust by signing up a million times.
 *
 * What is given up: per-grant revocation, which needed the row. Revocation is
 * now expiry (`GRANT_TTL_DAYS`, refreshed whenever the app registers) and, for
 * "everything, now", rotating `RELAY_SEAL_KEY` — with the outgoing key kept in
 * `RELAY_SEAL_KEY_PREVIOUS` for as long as the rollover needs.
 */

import { base64url } from "./jwt";
import { PLATFORMS, type Platform } from "./types";

const encoder = new TextEncoder();
const decoder = new TextDecoder();

/** Version prefix: a future format changes it and old grants stop verifying. */
const PREFIX = "vg1.";
const AAD = encoder.encode("vis-companion-relay/grant/v1");
const IV_BYTES = 12;
const TAG_BYTES = 16;

export interface Grant {
  deviceToken: string;
  platform: Platform;
  environment: string;
  expiresAt: number;
}

/**
 * One AES key per secret, derived by SHA-256 so any length of secret works and
 * the key material itself never sits in a variable a log could reach.
 */
const derivedKeys = new Map<string, Promise<CryptoKey>>();

function keyFor(secret: string): Promise<CryptoKey> {
  const cached = derivedKeys.get(secret);
  if (cached) return cached;
  const derived = crypto.subtle
    .digest("SHA-256", encoder.encode(secret))
    .then((bits) =>
      crypto.subtle.importKey("raw", bits, { name: "AES-GCM" }, false, ["encrypt", "decrypt"]),
    );
  derivedKeys.set(secret, derived);
  return derived;
}

function fromBase64url(text: string): Uint8Array | null {
  try {
    const padded = text.replace(/-/g, "+").replace(/_/g, "/");
    const binary = atob(padded + "=".repeat((4 - (padded.length % 4)) % 4));
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i += 1) bytes[i] = binary.charCodeAt(i);
    return bytes;
  } catch {
    return null;
  }
}

export async function seal(secret: string, grant: Grant): Promise<string> {
  const iv = crypto.getRandomValues(new Uint8Array(IV_BYTES));
  const plaintext = encoder.encode(
    JSON.stringify({
      t: grant.deviceToken,
      p: grant.platform,
      e: grant.environment,
      x: grant.expiresAt,
    }),
  );
  const sealed = new Uint8Array(
    await crypto.subtle.encrypt(
      { name: "AES-GCM", iv, additionalData: AAD },
      await keyFor(secret),
      plaintext,
    ),
  );
  const packed = new Uint8Array(iv.length + sealed.length);
  packed.set(iv);
  packed.set(sealed, iv.length);
  return `${PREFIX}${base64url(packed)}`;
}

/**
 * Open a grant, or answer `null` — forged, sealed under a key this relay no
 * longer holds, or past its expiry are the same answer to a caller, and none of
 * them cost more than one AES-GCM open.
 */
export async function unseal(
  secrets: readonly string[],
  token: string,
  now: number,
): Promise<Grant | null> {
  if (!token.startsWith(PREFIX)) return null;
  const packed = fromBase64url(token.slice(PREFIX.length));
  if (!packed || packed.length <= IV_BYTES + TAG_BYTES) return null;
  const iv = packed.subarray(0, IV_BYTES);
  const body = packed.subarray(IV_BYTES);

  for (const secret of secrets) {
    let opened: ArrayBuffer;
    try {
      opened = await crypto.subtle.decrypt(
        { name: "AES-GCM", iv, additionalData: AAD },
        await keyFor(secret),
        body,
      );
    } catch {
      continue;
    }
    try {
      const claims = JSON.parse(decoder.decode(opened)) as {
        t?: unknown;
        p?: unknown;
        e?: unknown;
        x?: unknown;
      };
      const deviceToken = typeof claims.t === "string" ? claims.t : "";
      const platform = claims.p as Platform;
      const expiresAt = typeof claims.x === "number" ? claims.x : 0;
      if (!deviceToken || !PLATFORMS.includes(platform) || expiresAt <= now) return null;
      return {
        deviceToken,
        platform,
        environment: claims.e === "sandbox" ? "sandbox" : "production",
        expiresAt,
      };
    } catch {
      return null;
    }
  }
  return null;
}
