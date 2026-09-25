// Parse the gateway pairing link produced by `vis-agent gateway pair`
// (src/com/blockether/vis/internal/gateway/pairing.clj). The QR encodes:
//
//   vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890&alt=<urls>&token=<bearer>
//
// `alt` and `token` are optional: `alt` lists other candidate addresses and
// `token` carries the gateway's bearer token.

import type { GatewayConn } from './types';

function hostLabel(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

/** Parse a `vis://gateway?...` deep link into a connection, or null. */
export function parsePairingUrl(input: string): GatewayConn | null {
  const trimmed = input.trim();
  if (!/^vis:\/\/gateway\b/i.test(trimmed)) return null;
  // vis://gateway?url=…&token=… — normalize to a parseable URL.
  const q = trimmed.replace(/^vis:\/\/gateway\??/i, '');
  const params = new URLSearchParams(q);
  const url = params.get('url');
  if (!url) return null;
  const token = params.get('token') ?? undefined;
  const alts = (params.get('alt') ?? '')
    .split(',')
    .map((s) => s.trim())
    .filter((s) => /^https?:\/\//i.test(s));
  return { url, token, label: hostLabel(url), ...(alts.length ? { alts } : {}) };
}

/**
 * Best-effort parse of anything a scan or paste can yield: the vis:// URL or a
 * bare gateway URL (http://host:port).
 */
export function parsePairing(input: string): GatewayConn | null {
  return (
    parsePairingUrl(input) ??
    (/^https?:\/\//i.test(input.trim())
      ? { url: input.trim(), label: hostLabel(input.trim()) }
      : null)
  );
}
