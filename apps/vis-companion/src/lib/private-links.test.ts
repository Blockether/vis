// @vitest-environment jsdom
import { afterEach, beforeEach, expect, it, vi } from 'vitest';
import routes from '../../../../packages/vis-contract/resources/vis-contract/fixtures/proxy-routes.json';
import { captureLinkClicks } from './desktop';
import {
  privateLinkRoute,
  proxyCovers,
  setPrivateLinkRoute,
  type PrivateLinkRoute,
  type ProxyAnswer,
} from './private-links';

const phone = vi.hoisted(() => ({
  native: true,
  available: true,
  isAvailable: vi.fn(),
  open: vi.fn(),
}));
vi.mock('@capacitor/core', () => ({
  Capacitor: {
    isNativePlatform: () => phone.native,
    isPluginAvailable: () => phone.native,
  },
  registerPlugin: () => phone,
}));

const answer: ProxyAnswer = {
  is_enabled: true,
  port: 7891,
  allowed_domains: ['corp.internal', '*.acme.io'],
  denied_domains: ['secret.corp.internal'],
  ports: [80, 443],
};
const route = privateLinkRoute(answer, 'http://100.64.0.10:7890/', 'gateway-token') as PrivateLinkRoute;

function click(href: string) {
  const anchor = document.createElement('a');
  anchor.setAttribute('href', href);
  anchor.setAttribute('target', '_blank');
  document.body.append(anchor);
  const event = new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 });
  anchor.dispatchEvent(event);
  anchor.remove();
  return event;
}

beforeEach(() => {
  phone.native = true;
  phone.isAvailable.mockImplementation(async () => ({ available: phone.available }));
  phone.open.mockResolvedValue(undefined);
  vi.spyOn(window, 'open').mockReturnValue(null);
  captureLinkClicks();
});

afterEach(async () => {
  await setPrivateLinkRoute(null);
  delete (window as { __TAURI__?: unknown }).__TAURI__;
  phone.available = true;
  vi.restoreAllMocks();
  vi.clearAllMocks();
});

it('sends the same links through the proxy as the gateway and the SDK', () => {
  for (const group of routes.groups) {
    for (const { url, is_proxied } of group.cases) {
      expect([url, proxyCovers(group, url)]).toEqual([url, is_proxied]);
    }
  }
});

it('uses no proxy when it is off, when the app has no token or when the gateway is on this computer', () => {
  expect(route).toMatchObject({ host: '100.64.0.10', port: 7891, token: 'gateway-token' });
  expect(privateLinkRoute({ ...answer, is_enabled: false, port: null }, 'http://10.0.0.5:7890', 't')).toBeNull();
  expect(privateLinkRoute(answer, 'http://10.0.0.5:7890', undefined)).toBeNull();
  expect(privateLinkRoute(answer, 'http://127.0.0.1:7890', 't')).toBeNull();
  expect(privateLinkRoute(answer, 'http://localhost:7890', 't')).toBeNull();
  expect(privateLinkRoute(answer, 'http://[::1]:7890', 't')).toBeNull();
});

it('opens a private link on a phone in the in-app browser through the gateway', async () => {
  await setPrivateLinkRoute(route);

  expect(click('https://grafana.acme.io/d/1').defaultPrevented).toBe(true);
  expect(phone.open).toHaveBeenCalledWith({
    url: 'https://grafana.acme.io/d/1',
    proxyHost: '100.64.0.10',
    proxyPort: 7891,
    token: 'gateway-token',
    allowedDomains: ['corp.internal', '*.acme.io'],
    deniedDomains: ['secret.corp.internal'],
    ports: [80, 443],
  });
  // The web view opens every other link itself, as before.
  expect(click('https://example.com/').defaultPrevented).toBe(false);
  expect(click('https://secret.corp.internal/').defaultPrevented).toBe(false);
  expect(phone.open).toHaveBeenCalledTimes(1);
});

it('opens private links as before on a phone without the in-app browser', async () => {
  phone.available = false;
  await setPrivateLinkRoute(route);

  expect(click('https://corp.internal/').defaultPrevented).toBe(false);
  expect(phone.open).not.toHaveBeenCalled();
});

it('opens the link in the system browser when the in-app browser fails', async () => {
  phone.open.mockRejectedValue(new Error('no web view'));
  await setPrivateLinkRoute(route);

  click('https://corp.internal/wiki');
  await vi.waitFor(() =>
    expect(window.open).toHaveBeenCalledWith('https://corp.internal/wiki', '_blank', 'noopener,noreferrer'),
  );
});

it('opens a private link in the desktop browser window through the gateway', async () => {
  const invoke = vi.fn(async (command: string) => (command === 'proxy_browser_available' ? true : undefined));
  (window as { __TAURI__?: unknown }).__TAURI__ = { core: { invoke } };
  await setPrivateLinkRoute(route, invoke);

  click('http://corp.internal/');
  expect(invoke).toHaveBeenCalledWith('proxy_browser_open', expect.objectContaining({ url: 'http://corp.internal/', proxyPort: 7891 }));
  click('https://example.com/');
  expect(invoke).toHaveBeenCalledWith('plugin:shell|open', { path: 'https://example.com/' });
  expect(phone.open).not.toHaveBeenCalled();
});

it('keeps the route of the last call when an earlier call completes later', async () => {
  let release: (value: { available: boolean }) => void = () => {};
  phone.isAvailable.mockImplementationOnce(() => new Promise((resolve) => (release = resolve)));
  const first = setPrivateLinkRoute(route);
  await setPrivateLinkRoute(null);
  release({ available: true });
  await first;

  expect(click('https://corp.internal/').defaultPrevented).toBe(false);
});
