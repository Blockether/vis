// Parse the gateway pairing payloads produced by `vis gateway pair`
// (src/com/blockether/vis/internal/gateway/pairing.clj).
//
// Two shapes are supported, matching the Clojure producers exactly:
//
//   1. URL form (the QR default):
//        vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890&token=<bearer>
//
//   2. JSON form (pairing-json):
//        {"type":"vis-gateway-pairing","version":1,
//         "url":"http://100.64.0.10:7890","hosts":[...],"token":"<bearer>"}

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
  return { url, token, label: hostLabel(url) };
}

/** Parse the JSON pairing payload into a connection, or null. */
export function parsePairingJson(input: string): GatewayConn | null {
  try {
    const obj = JSON.parse(input.trim());
    if (obj && obj.type === 'vis-gateway-pairing' && typeof obj.url === 'string') {
      return {
        url: obj.url,
        token: typeof obj.token === 'string' ? obj.token : undefined,
        label: hostLabel(obj.url),
      };
    }
  } catch {
    /* not JSON */
  }
  return null;
}

/**
 * Best-effort parse of anything a scan or paste can yield: the vis:// URL, the
 * JSON payload, or a bare gateway URL (http://host:port).
 */
export function parsePairing(input: string): GatewayConn | null {
  return (
    parsePairingUrl(input) ??
    parsePairingJson(input) ??
    (/^https?:\/\//i.test(input.trim())
      ? { url: input.trim(), label: hostLabel(input.trim()) }
      : null)
  );
}
