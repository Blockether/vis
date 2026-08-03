/**
 * Gateway addresses, ranked by DURABILITY.
 *
 * One gateway answers on several addresses at once — a tailnet IP, a LAN IP, a
 * tunnel hostname, loopback — and the pairing QR ships all of them (`alt=` /
 * `hosts`). They are NOT interchangeable: the LAN address dies the moment the
 * phone leaves the house, while the Tailscale address keeps working from
 * anywhere. Picking "whichever answered first" therefore quietly pins the app
 * to the most fragile address, because at pairing time the phone is standing
 * next to the machine and the LAN address is the fastest to reply.
 *
 * So the app carries an explicit preference and re-applies it whenever it
 * learns a better address: tailnet first, then a public tunnel hostname, then
 * LAN, then anything else, loopback last (only the machine itself can use it).
 */

export type Reach = 'tailscale' | 'tunnel' | 'lan' | 'loopback' | 'other';

/** Lower is more durable. Used for both selection and display order. */
const RANK: Record<Reach, number> = {
  tailscale: 0,
  tunnel: 1,
  lan: 2,
  other: 3,
  loopback: 4,
};

export const REACH_LABEL: Record<Reach, string> = {
  tailscale: 'Tailscale',
  tunnel: 'Tunnel',
  lan: 'Local network',
  loopback: 'This machine',
  other: 'Direct',
};

export const REACH_HINT: Record<Reach, string> = {
  tailscale: 'Works from anywhere your tailnet reaches',
  tunnel: 'Public hostname — works from anywhere',
  lan: 'Only while on the same Wi-Fi',
  loopback: 'Only on the machine running vis',
  other: 'Reachable address',
};

/** Trailing slash and case differences must not create a second connection. */
export function normalizeAddress(url: string): string {
  const trimmed = String(url ?? '').trim();
  if (!trimmed) return '';
  try {
    const u = new URL(trimmed);
    const port = u.port ? `:${u.port}` : '';
    return `${u.protocol.toLowerCase()}//${u.hostname.toLowerCase()}${port}`;
  } catch {
    return trimmed.replace(/\/+$/, '');
  }
}

/**
 * A gateway address the way a human types it into the connect form:
 * `10.0.0.5:7890`, `my-mac.local:7890`, `gateway.example.com`, or a full URL.
 * Returns an absolute http(s) URL, or null when the input cannot become one.
 *
 * A missing scheme is SUPPLIED, never rejected — typing a host is the obvious
 * thing to do and refusing it teaches nothing. Plain HTTP for addresses that
 * only exist on a private network (IP literal, `localhost`, `*.local`) or that
 * name an explicit port, HTTPS for a bare public hostname, which is what a
 * tunnel terminates.
 */
export function normalizeGatewayUrl(input: string): string | null {
  const trimmed = String(input ?? '')
    .trim()
    .replace(/\/+$/, '');
  if (!trimmed || /\s/.test(trimmed)) return null;
  const hasScheme = /^[a-z][a-z0-9+.-]*:\/\//i.test(trimmed);
  if (hasScheme && !/^https?:\/\//i.test(trimmed)) return null;
  const probe = hasScheme ? trimmed : `http://${trimmed}`;
  let u: URL;
  try {
    u = new URL(probe);
  } catch {
    return null;
  }
  if (!u.hostname) return null;
  if (!hasScheme && !isPlainHttpHost(u.hostname) && !u.port) u.protocol = 'https:';
  return u.toString().replace(/\/+$/, '');
}

/** Hosts that no public certificate can cover, so they are served over plain HTTP. */
function isPlainHttpHost(hostname: string): boolean {
  const host = hostname.toLowerCase();
  if (host === 'localhost' || host.endsWith('.localhost') || host.endsWith('.local')) return true;
  if (host.startsWith('[')) return true; // IPv6 literal
  return ipv4(host) !== null;
}

export function hostOf(url: string): string {
  try {
    return new URL(url).host;
  } catch {
    return url;
  }
}

function ipv4(host: string): number[] | null {
  const m = /^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/.exec(host);
  if (!m) return null;
  const parts = m.slice(1).map(Number);
  return parts.every((n) => n >= 0 && n <= 255) ? parts : null;
}

/** How this address survives leaving the network it was paired on. */
export function reachOf(url: string): Reach {
  let host: string;
  try {
    host = new URL(url).hostname.toLowerCase();
  } catch {
    return 'other';
  }
  if (!host) return 'other';
  if (host === 'localhost' || host === '::1' || host === '[::1]') return 'loopback';

  const ip = ipv4(host);
  if (ip) {
    const [a, b] = ip;
    if (a === 127) return 'loopback';
    // Tailscale CGNAT range 100.64.0.0/10.
    if (a === 100 && b >= 64 && b <= 127) return 'tailscale';
    if (a === 10) return 'lan';
    if (a === 192 && b === 168) return 'lan';
    if (a === 172 && b >= 16 && b <= 31) return 'lan';
    // 169.254/16 link-local: routable by nobody useful, but still LAN-shaped.
    if (a === 169 && b === 254) return 'lan';
    return 'other';
  }
  // MagicDNS names are Tailscale addresses by another spelling.
  if (host.endsWith('.ts.net')) return 'tailscale';
  // mDNS names only resolve on the same LAN.
  if (host.endsWith('.local')) return 'lan';
  return 'tunnel';
}

export function addressRank(url: string): number {
  return RANK[reachOf(url)];
}

/** Most durable first; ties broken alphabetically so ordering is stable. */
export function compareAddresses(a: string, b: string): number {
  return addressRank(a) - addressRank(b) || a.localeCompare(b);
}

/**
 * Every address we know for one gateway: normalized, de-duplicated and sorted
 * by durability. Sources are the pairing payload and, later, whatever the
 * gateway itself advertises — so an app paired on the LAN can still discover
 * the tailnet address without re-scanning a QR.
 */
export function mergeAddresses(...lists: (readonly string[] | undefined | null)[]): string[] {
  const seen = new Set<string>();
  for (const list of lists) {
    for (const raw of list ?? []) {
      const url = normalizeAddress(raw);
      if (/^https?:\/\//i.test(url)) seen.add(url);
    }
  }
  return Array.from(seen).sort(compareAddresses);
}

/** The most durable address in the list, or undefined when it is empty. */
export function bestAddress(urls: readonly string[]): string | undefined {
  return [...urls].sort(compareAddresses)[0];
}

/**
 * Should the app move itself from `current` to `candidate`?
 *
 * Only ever towards a more durable address, and never away from loopback: a
 * browser open on the gateway's own machine is deliberately on 127.0.0.1 and
 * must not be pushed onto the tailnet behind the user's back.
 */
export function isUpgrade(candidate: string, current: string): boolean {
  if (normalizeAddress(candidate) === normalizeAddress(current)) return false;
  if (reachOf(current) === 'loopback') return false;
  return addressRank(candidate) < addressRank(current);
}
