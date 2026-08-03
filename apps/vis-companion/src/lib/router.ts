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
// the link only says *which* gateway and *which* session.

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

/**
 * Deep link that opens a session in the INSTALLED app: `vis://s/<sid>?gw=<id>`.
 *
 * Inside the Capacitor WebView `window.location.origin` is `capacitor://localhost`
 * (iOS) — a private origin nothing outside the app can open — so an http(s) share
 * URL simply does not exist there. The `vis` scheme is registered by the app
 * (ios/App/App/Info.plist CFBundleURLSchemes), so this link is the shareable form
 * on native and is routed by `parseSessionDeepLink` on `appUrlOpen`.
 */
export function sessionDeepLink(sid: string, gatewayId?: string): string {
  const base = `vis://s/${encodeURIComponent(sid)}`;
  return gatewayId ? `${base}?gw=${encodeURIComponent(gatewayId)}` : base;
}

/** Parse a `vis://s/<sid>?gw=<id>` deep link into its hash route, or null. */
export function parseSessionDeepLink(url: string): string | null {
  const trimmed = (url || '').trim();
  if (!/^vis:\/\/s\//i.test(trimmed)) return null;
  const rest = trimmed.replace(/^vis:\/\/s\//i, '');
  const [sidPart = '', queryPart = ''] = rest.split('?');
  const sid = decodeURIComponent(sidPart.replace(/\/+$/, ''));
  if (!sid) return null;
  const gw = new URLSearchParams(queryPart).get('gw');
  return sessionHash(sid, gw ?? undefined);
}

/**
 * The best link to hand to someone else for the session currently on screen:
 * the absolute https URL on the web, the `vis://` deep link inside the app.
 */
export function shareableSessionLink(): string {
  if (typeof window === 'undefined') return '';
  const href = window.location.href;
  if (/^https?:/i.test(href)) return href;
  const route = parseRoute(window.location.hash);
  return route.name === 'session' ? sessionDeepLink(route.sid, route.gw) : href;
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

/**
 * Identity of the screen currently filling the shell: the open session, or the
 * empty string for the navigator. Compared, never parsed.
 */
export function screenKey(
  target: { conn: { url: string }; sid: string } | null | undefined,
): string {
  return target ? `${target.conn.url}\u0000${target.sid}` : '';
}

/**
 * True when the shell moved INTO a session screen, or across to a different
 * one.
 *
 * Overlays belong to the screen that opened them. Navigation here is not always
 * a tap: creating a session from the list is an async POST, so a user who opens
 * Settings while it flies gets the transcript swapped in UNDERNEATH the modal —
 * the list header and tab bar unmount, and a `fresh` session focuses its
 * composer, which pins the shell and re-anchors the dialog's `fixed` box to a
 * now-transformed ancestor. The dialog jumps out of the safe area and floats
 * over a screen it was never opened from.
 *
 * Leaving a session is deliberately NOT a dismissal: the gateway settings
 * dialog closes the open session itself when it switches the primary gateway,
 * and must survive doing so.
 */
export function isSessionEntered(previous: string, next: string): boolean {
  return next !== '' && next !== previous;
}
