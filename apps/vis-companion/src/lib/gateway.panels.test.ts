// Regression, user report (paraphrased: "going to settings flickers the MCP
// servers, the providers and the notifications every time it opens the primary
// machine"): those three panels held their answer nowhere durable, so every
// open painted them empty and filled them a round trip later — Providers from
// a memory-only map that dies with the webview, MCP from no cache at all, and
// Notifications from a capability refusal nothing remembered.
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { McpServer, RouterProvider } from './types';

const conn = { url: 'http://gateway.example.com:7890' };

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { 'Content-Type': 'application/json' },
  });

const servers: McpServer[] = [
  {
    name: 'files',
    transport: 'stdio',
    enabled: true,
    is_connected: true,
    is_managed: true,
    tools: 3,
    is_killed: false,
    command: 'mcp-files',
  },
];

const providers: RouterProvider[] = [
  {
    id: 'anthropic',
    label: 'Anthropic',
    models: ['claude-opus-5'],
    is_default: true,
    default_model: 'claude-opus-5',
    is_fallback: false,
    fallback_model: null,
  },
];

/** What a cold start is: the tab is gone, its storage is not. */
async function relaunch(fetching: typeof fetch) {
  vi.resetModules();
  vi.stubGlobal('fetch', fetching);
  return await import('./gateway');
}

beforeEach(() => {
  localStorage.clear();
  // A `node` test file has no window; the client reaches for its timers.
  vi.stubGlobal('window', globalThis);
  vi.resetModules();
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe('settings panels across a cold start', () => {
  it('paints the MCP servers this machine last answered, asking nothing', async () => {
    const warm = vi.fn(() => Promise.resolve(json({ servers })));
    const first = await relaunch(warm as unknown as typeof fetch);
    await new first.GatewayClient(conn).mcpServers();
    first.persistGatewayCaches();

    const cold = vi.fn(() => Promise.resolve(json({ servers })));
    const second = await relaunch(cold as unknown as typeof fetch);
    expect(new second.GatewayClient(conn).cachedMcpServers()).toEqual(servers);
    expect(cold).not.toHaveBeenCalled();
  });

  it('keeps that seed in step with a row this device just changed', async () => {
    const disabled = { ...servers[0]!, enabled: false };
    const answers = vi
      .fn()
      .mockResolvedValueOnce(json({ servers }))
      .mockResolvedValueOnce(json(disabled));
    vi.stubGlobal('fetch', answers);
    const mod = await import('./gateway');
    const client = new mod.GatewayClient(conn);

    await client.mcpServers();
    await client.setMcpServerEnabled('files', false);

    // Reopening the panel must paint the press, not the state before it.
    expect(client.cachedMcpServers()).toEqual([disabled]);
  });

  it('paints the provider fleet it last saw, and drops it when a sign-out lands', async () => {
    const warm = vi.fn(() => Promise.resolve(json({ providers })));
    const first = await relaunch(warm as unknown as typeof fetch);
    await new first.GatewayClient(conn).router();
    first.persistGatewayCaches();

    const cold = vi.fn(() => Promise.resolve(json({ providers })));
    const second = await relaunch(cold as unknown as typeof fetch);
    const client = new second.GatewayClient(conn);
    expect(client.cachedRouter()).toEqual(providers);
    expect(cold).not.toHaveBeenCalled();

    // Every auth mutation invalidates: a provider just signed out of must never
    // paint as signed in while the re-probe is in flight.
    client.invalidateRouter();
    expect(client.cachedRouter()).toBeNull();
  });

  it('remembers a machine that carries no /v1/devices, and forgets it once it does', async () => {
    const old = vi.fn(() => Promise.resolve(json({ error: 'no route' }, 404)));
    vi.stubGlobal('fetch', old);
    const mod = await import('./gateway');
    const client = new mod.GatewayClient(conn);

    await expect(client.devices()).rejects.toThrow();
    expect(client.isDevicesUnsupported()).toBe(true);

    const upgraded = vi.fn(() =>
      Promise.resolve(json({ devices: [], push: { is_available: false } })),
    );
    vi.stubGlobal('fetch', upgraded);
    await client.devices();
    expect(client.isDevicesUnsupported()).toBe(false);
  });
});

// The panel a reader opens has already answered, whichever machine it is: the
// sweep is fleet-wide, and TTL-stamped so a wake costs nothing.
describe('warming a machine\'s settings panels', () => {
  it('asks each question once, and asks nothing at all on the next sweep', async () => {
    const asked: string[] = [];
    const answer = vi.fn((url: string) => {
      asked.push(new URL(url, conn.url).pathname);
      return Promise.resolve(json({ servers, providers, devices: [], groups: [] }));
    });
    vi.stubGlobal('fetch', answer);
    const mod = await import('./gateway');
    const client = new mod.GatewayClient(conn);

    client.prefetchPanels();
    await vi.waitFor(() => expect(asked).toHaveLength(5));
    expect(new Set(asked)).toEqual(
      new Set([
        '/v1/settings',
        '/v1/capabilities',
        '/v1/mcp/servers',
        '/v1/devices',
        '/v1/router',
      ]),
    );

    // Wake, a re-render, another machine added: the sweep runs again and the
    // fleet is charged nothing for it.
    client.prefetchPanels();
    new mod.GatewayClient(conn).prefetchPanels();
    await new Promise((resolve) => setTimeout(resolve, 10));
    expect(asked).toHaveLength(5);
  });
});
