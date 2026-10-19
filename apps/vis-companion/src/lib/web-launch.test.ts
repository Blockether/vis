// @vitest-environment jsdom
// Regression for #397: `vis-agent web` must connect the browser it opens to the
// exact gateway it started, without a pasted pairing link or token.
import { afterEach, describe, expect, it, vi } from 'vitest';

import {
  WebLaunchError,
  connectWebLaunch,
  parseWebLaunch,
  planWebLaunch,
  takeWebLaunch,
  type WebLaunch,
} from './web-launch';
import type { GatewayConn } from './types';

const GATEWAY = 'b3f1c2d4e5f60718';
const OTHER = '0123456789abcdef';
const LOCAL = 'http://localhost:7890';

function seed(conns: GatewayConn[]) {
  for (const prefix of ['', 'CapacitorStorage.'])
    localStorage.setItem(`${prefix}vis.connections`, JSON.stringify(conns));
}

const saved = (): GatewayConn[] =>
  JSON.parse(localStorage.getItem('CapacitorStorage.vis.connections') ?? '[]');

/** A gateway that redeems `code` once, as `server/web.clj` does. */
function gateway({
  id = GATEWAY,
  code = 'one-time-code',
  token = 'gateway-token',
  requireToken = true,
} = {}) {
  let used = false;
  const calls: { url: string; init?: RequestInit }[] = [];
  const fetchImpl = vi.fn(async (input: RequestInfo | URL, init?: RequestInit) => {
    const url = String(input);
    calls.push({ url, init });
    const body = JSON.parse(String(init?.body ?? '{}')) as { code?: string };
    if (!url.endsWith('/v1/web/launches/redeem') || body.code !== code || used)
      return new Response(JSON.stringify({ error: { type: 'launch_not_found' } }), { status: 404 });
    used = true;
    return new Response(JSON.stringify({ gateway_id: id, token, require_token: requireToken }), {
      status: 200,
    });
  }) as unknown as typeof fetch;
  return { fetchImpl, calls };
}

const launch = (gatewayId = GATEWAY): WebLaunch => ({ code: 'one-time-code', gatewayId });

afterEach(() => localStorage.clear());

describe('web launch address', () => {
  it('reads the code and the gateway id from the fragment only', () => {
    expect(parseWebLaunch('#launch=abc_-1&gw=b3f1')).toEqual({ code: 'abc_-1', gatewayId: 'b3f1' });
    expect(parseWebLaunch('#/s/123')).toBeNull();
    expect(parseWebLaunch('#launch=abc')).toBeNull();
    expect(parseWebLaunch('')).toBeNull();
  });

  it('takes the launch out of the address bar before the router reads it', () => {
    window.history.replaceState(null, '', '/?x=1#launch=abc&gw=b3f1');
    expect(takeWebLaunch()).toEqual({ code: 'abc', gatewayId: 'b3f1' });
    expect(window.location.hash).toBe('');
    expect(window.location.search).toBe('?x=1');
    // A plain address keeps its route.
    window.history.replaceState(null, '', '/#/s/123');
    expect(takeWebLaunch()).toBeNull();
    expect(window.location.hash).toBe('#/s/123');
  });
});

describe('connecting a launched browser', () => {
  it('saves the gateway in fresh browser storage with its token', async () => {
    const { fetchImpl, calls } = gateway();
    const conn = await connectWebLaunch(LOCAL, launch(), fetchImpl);
    expect(conn).toEqual({ url: LOCAL, label: 'localhost:7890', id: GATEWAY, token: 'gateway-token' });
    expect(saved()).toEqual([conn]);
    // The code travels in a POST body, never in a URL.
    expect(calls.map((call) => call.url)).toEqual([`${LOCAL}/v1/web/launches/redeem`]);
    expect(calls[0]!.init?.method).toBe('POST');
  });

  it('works once: a reused or expired address saves nothing', async () => {
    const { fetchImpl } = gateway();
    await connectWebLaunch(LOCAL, launch(), fetchImpl);
    localStorage.clear();
    await expect(connectWebLaunch(LOCAL, { ...launch() }, fetchImpl)).rejects.toThrow(
      /already used or has expired/,
    );
    expect(saved()).toEqual([]);
  });

  it('redeems once when a double render asks twice', async () => {
    const { fetchImpl } = gateway();
    const one = launch();
    const [a, b] = await Promise.all([
      connectWebLaunch(LOCAL, one, fetchImpl),
      connectWebLaunch(LOCAL, one, fetchImpl),
    ]);
    expect(a).toEqual(b);
    expect(fetchImpl).toHaveBeenCalledTimes(1);
  });

  it('keeps the WSL localhost address the Windows browser reached', async () => {
    // The gateway binds 0.0.0.0 inside WSL; Windows forwards localhost to it.
    // The machine was saved earlier at the WSL address, which Windows cannot open.
    seed([{ url: 'http://172.20.1.5:7890', id: GATEWAY, label: 'wsl', token: 'old' }]);
    const { fetchImpl } = gateway();
    const conn = await connectWebLaunch(LOCAL, launch(), fetchImpl);
    expect(conn).toMatchObject({ url: LOCAL, id: GATEWAY, token: 'gateway-token', label: 'wsl' });
    expect(conn.alts).toEqual(['http://172.20.1.5:7890']);
    expect(saved()).toHaveLength(1);
  });

  it('repairs a saved tokenless entry for this address and leaves other machines alone', async () => {
    const other: GatewayConn = { url: 'http://10.0.0.5:7890', id: OTHER, token: 'theirs', label: 'desk' };
    seed([{ url: `${LOCAL}/`, label: 'my laptop' }, other]);
    const { fetchImpl } = gateway();
    const conn = await connectWebLaunch(LOCAL, launch(), fetchImpl);
    expect(conn).toEqual({ url: `${LOCAL}/`, label: 'my laptop', id: GATEWAY, token: 'gateway-token' });
    expect(saved()).toEqual([conn, other]);
  });

  it('repairs a stale token of the same gateway', async () => {
    seed([{ url: LOCAL, id: GATEWAY, token: 'rotated-away', label: 'here' }]);
    const { fetchImpl } = gateway({ token: 'new-token' });
    await connectWebLaunch(LOCAL, launch(), fetchImpl);
    expect(saved()).toEqual([{ url: LOCAL, id: GATEWAY, token: 'new-token', label: 'here' }]);
  });

  it('refuses a grant from another gateway than the launch names', async () => {
    const { fetchImpl } = gateway({ id: OTHER });
    await expect(connectWebLaunch(LOCAL, launch(GATEWAY), fetchImpl)).rejects.toThrow(
      WebLaunchError,
    );
    expect(saved()).toEqual([]);
  });

  it('never overwrites another gateway saved at the same address', async () => {
    const theirs: GatewayConn = { url: LOCAL, id: OTHER, token: 'theirs', label: 'other db' };
    seed([theirs]);
    const { fetchImpl } = gateway();
    await expect(connectWebLaunch(LOCAL, launch(), fetchImpl)).rejects.toThrow(
      /Another machine is saved at/,
    );
    expect(saved()).toEqual([theirs]);
  });

  it('respects a hand-picked address of the same gateway', () => {
    const pinned: GatewayConn = { url: 'https://gateway.example.com', id: GATEWAY, pinned: true };
    expect(
      planWebLaunch([pinned], LOCAL, { gatewayId: GATEWAY, token: 'tok', requireToken: true }),
    ).toEqual({
      kind: 'update',
      url: 'https://gateway.example.com',
      patch: { id: GATEWAY, token: 'tok', alts: [LOCAL] },
    });
  });

  it('takes this address away from a machine that only listed it as an alternate', () => {
    const lan: GatewayConn = { url: 'http://10.0.0.5:7890', id: OTHER, alts: [LOCAL, 'http://10.0.0.6:7890'] };
    expect(
      planWebLaunch([lan], LOCAL, { gatewayId: GATEWAY, token: 'tok', requireToken: false }),
    ).toEqual({
      kind: 'add',
      conn: { url: LOCAL, label: 'localhost:7890', id: GATEWAY, token: 'tok' },
      release: [{ url: 'http://10.0.0.5:7890', alts: ['http://10.0.0.6:7890'] }],
    });
  });
});
