/**
 * Private links: links to company hosts that only the gateway can reach.
 *
 * A gateway with `gateway.proxy` in its configuration runs a forward proxy for a list of
 * domains (`GET /v1/proxy`). The app opens a link to such a domain in an in-app browser that
 * sends only these hosts through the proxy, with the gateway token as the proxy user name.
 * Other hosts of the same page go direct. The native `ProxyBrowser` plugin is that browser on
 * Android and iOS. The desktop app has the same browser as Vis commands in
 * `native/desktop/proxy_browser.rs`.
 *
 * The rules are the rules of the gateway and of the Python SDK. The shared cases are in
 * `vis-contract/fixtures/proxy-routes.json`.
 */
import { Capacitor, registerPlugin } from '@capacitor/core';

/** The answer of `GET /v1/proxy` (`proxy` in `vis-contract/schema/gateway.json`). */
export type ProxyAnswer = {
  is_enabled: boolean;
  port: number | null;
  allowed_domains: string[];
  denied_domains: string[];
  ports: number[];
};

/** The domain and port rules of one proxy. */
export type ProxyRules = Pick<ProxyAnswer, 'allowed_domains' | 'denied_domains' | 'ports'>;

/** The proxy of the active gateway. `token` is secret: keep it out of logs. */
export type PrivateLinkRoute = ProxyRules & { host: string; port: number; token: string };

/** The options of the native browser. The same names go to the desktop command. */
export type ProxyBrowserOptions = {
  url: string;
  proxyHost: string;
  proxyPort: number;
  token: string;
  allowedDomains: string[];
  deniedDomains: string[];
  ports: number[];
};

type ProxyBrowserPlugin = {
  /** False on a system that cannot send a web view through a proxy (iOS before 17). */
  isAvailable: () => Promise<{ available: boolean }>;
  open: (options: ProxyBrowserOptions) => Promise<void>;
};

/** The command channel of the desktop window (`desktopInvoke` in `desktop.ts`). */
export type HostInvoke = (command: string, payload?: Record<string, unknown>) => Promise<unknown>;

const DEFAULT_PORTS: Record<string, number> = { 'http:': 80, 'https:': 443 };

const hostKey = (host: string): string =>
  host.trim().replace(/^\[|\]$/g, '').toLowerCase().replace(/\.+$/, '');

/** `example.com` and `*.example.com` match example.com and each subdomain. */
function matches(pattern: string, host: string): boolean {
  const key = hostKey(pattern);
  const base = key.startsWith('*.') ? key.slice(2) : key;
  return base !== '' && base !== '*' && (host === base || host.endsWith(`.${base}`));
}

/** True when the proxy opens `href`: an HTTP(S) link to an allowed host and port. */
export function proxyCovers(rules: ProxyRules, href: string): boolean {
  let url: URL;
  try {
    url = new URL(href);
  } catch {
    return false;
  }
  const defaultPort = DEFAULT_PORTS[url.protocol];
  if (defaultPort === undefined || !url.hostname) return false;
  const host = hostKey(url.hostname);
  if (rules.denied_domains.some((pattern) => matches(pattern, host))) return false;
  if (!rules.allowed_domains.some((pattern) => matches(pattern, host))) return false;
  return rules.ports.includes(url.port ? Number(url.port) : defaultPort);
}

const LOOPBACK = /^(localhost|127(?:\.\d{1,3}){3}|::1)$/;

/**
 * The proxy of the gateway at `base`, or null when the app must open links as before:
 * the proxy is off, the app has no token, or the gateway is on this computer. A gateway on
 * this computer reaches no host that this computer cannot reach.
 */
export function privateLinkRoute(
  answer: ProxyAnswer | null,
  base: string,
  token: string | undefined,
): PrivateLinkRoute | null {
  if (!answer?.is_enabled || answer.port === null || !token) return null;
  let host: string;
  try {
    host = hostKey(new URL(base).hostname);
  } catch {
    return null;
  }
  if (!host || LOOPBACK.test(host)) return null;
  return {
    host,
    port: answer.port,
    token,
    allowed_domains: answer.allowed_domains,
    denied_domains: answer.denied_domains,
    ports: answer.ports,
  };
}

let plugin: ProxyBrowserPlugin | undefined;
const proxyBrowser = () => (plugin ??= registerPlugin<ProxyBrowserPlugin>('ProxyBrowser'));

/** Where the in-app browser is: the desktop channel, the native plugin, or nowhere. */
type Opener = { open: (options: ProxyBrowserOptions) => Promise<unknown> };

let active: { route: PrivateLinkRoute; opener: Opener } | null = null;
let generation = 0;

async function findOpener(invoke: HostInvoke | undefined): Promise<Opener | null> {
  if (invoke) {
    const available = await invoke('proxy_browser_available').catch(() => false);
    return available === true
      ? { open: (options) => invoke('proxy_browser_open', options) }
      : null;
  }
  if (!Capacitor.isNativePlatform() || !Capacitor.isPluginAvailable('ProxyBrowser')) return null;
  const { available } = await proxyBrowser()
    .isAvailable()
    .catch(() => ({ available: false }));
  return available ? { open: (options) => proxyBrowser().open(options) } : null;
}

/**
 * Use `route` for the links that the person opens from now on. A null route, or a host
 * without an in-app browser, turns private links off. A later call wins over an earlier
 * call that has not completed.
 */
export async function setPrivateLinkRoute(
  route: PrivateLinkRoute | null,
  invoke?: HostInvoke,
): Promise<void> {
  const call = ++generation;
  const opener = route ? await findOpener(invoke) : null;
  if (call !== generation) return;
  active = route && opener ? { route, opener } : null;
}

/** True when a click on `href` must open the in-app browser. */
export const isPrivateLink = (href: string): boolean =>
  active !== null && proxyCovers(active.route, href);

/**
 * Open `href` in the in-app browser through the gateway proxy. It rejects when `href` is
 * not a private link or the browser cannot open, so the caller can open it as before.
 */
export async function openPrivateLink(href: string): Promise<void> {
  if (!active || !proxyCovers(active.route, href)) throw new Error('Not a private link.');
  const { route, opener } = active;
  await opener.open({
    url: href,
    proxyHost: route.host,
    proxyPort: route.port,
    token: route.token,
    allowedDomains: route.allowed_domains,
    deniedDomains: route.denied_domains,
    ports: route.ports,
  });
}
