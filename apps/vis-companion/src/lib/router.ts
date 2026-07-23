// Hash-based routing so a session is a shareable URL.
//
// Hash routing (not path routing) is deliberate: it needs no server rewrite
// rule and works identically for the web build, a static file:// bundle, and
// the Capacitor iOS/Android WebView. A shared link therefore stays valid on
// every surface.
//
// Shape:
//   #/                                  → sessions navigator
//   #/connect                           → gateways
//   #/s/<sid>?gw=<gateway-id>           → open session <sid> on gateway <gw>
//
// The gateway *id* (its stable, opaque `/healthz` instance id — NOT its URL)
// rides in the link so the recipient's app can pick the right paired gateway
// without leaking its URL or bearer token. They still need that gateway paired;
// the link only says *which* gateway and *which* session. Legacy links that
// carried a full gateway URL still resolve (matched by URL as a fallback).

export type Route =
  | { name: 'sessions' }
  | { name: 'connect' }
  | { name: 'session'; sid: string; gw?: string };

/** Parse `window.location.hash` into a Route. Unknown hashes → sessions. */
export function parseRoute(hash: string): Route {
  const raw = (hash || '').replace(/^#/, '');
  if (!raw || raw === '/' || raw === '') return { name: 'sessions' };
  const [pathPart, queryPart = ''] = raw.split('?');
  const segs = pathPart.split('/').filter(Boolean);
  if (segs[0] === 'connect') return { name: 'connect' };
  if (segs[0] === 's' && segs[1]) {
    const params = new URLSearchParams(queryPart);
    const gw = params.get('gw');
    return { name: 'session', sid: decodeURIComponent(segs[1]), gw: gw ? decodeURIComponent(gw) : undefined };
  }
  return { name: 'sessions' };
}

/** Build the hash for a session, tagging its gateway by stable id. */
export function sessionHash(sid: string, gatewayId?: string): string {
  const base = `#/s/${encodeURIComponent(sid)}`;
  return gatewayId ? `${base}?gw=${encodeURIComponent(gatewayId)}` : base;
}

/** Build the hash for a top-level tab. */
export function tabHash(tab: 'sessions' | 'connect'): string {
  return tab === 'connect' ? '#/connect' : '#/';
}

/**
 * Absolute, shareable link to a session — the full origin + hash, so it can be
 * pasted into a message and opened by another user's app.
 */
export function sessionShareUrl(sid: string, gatewayId?: string): string {
  const origin = typeof window !== 'undefined'
    ? window.location.origin + window.location.pathname
    : '';
  return `${origin}${sessionHash(sid, gatewayId)}`;
}
