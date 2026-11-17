// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from 'vitest';

type Host = (command: string) => Promise<unknown>;
type DesktopWindow = Window & { __TAURI__?: unknown };

/** This computer as the desktop host and the loopback gateway report it. */
function computer(start: {
  isInstalled: boolean;
  installs: boolean;
  /** The gateway serves only a request with this token. */
  token?: string;
  /** The token that the host reads on this computer. */
  saved?: string;
}) {
  const world = { isInstalled: start.isInstalled, isUp: false, calls: [] as string[] };
  vi.stubGlobal('fetch', async (input: RequestInfo | URL, init?: RequestInit) => {
    if (!world.isUp) throw new TypeError('connection refused');
    // The health route answers each client, also one that the gateway refuses.
    if (String(input).includes('/healthz')) {
      return Response.json({ id: 'gw-local', require_token: Boolean(start.token) });
    }
    const sent = new Headers(init?.headers).get('Authorization');
    return start.token && sent !== `Bearer ${start.token}`
      ? new Response('{}', { status: 401 })
      : Response.json({});
  });
  const host: Host = async (command) => {
    world.calls.push(command);
    if (command === 'engine_run') world.isInstalled = start.installs;
    if (command === 'engine_gateway_token') return start.saved ?? null;
    if (command === 'engine_gateway_start') world.isUp = true;
    if (command === 'engine_progress') {
      return { task: 'install', running: false, ok: world.isInstalled, log: ['HTTP 503'] };
    }
    if (command === 'engine_status') {
      const version = world.isInstalled ? '0.5.21' : null;
      return { supported: true, installed: world.isInstalled, version, track: 'dev' };
    }
    return null;
  };
  (window as DesktopWindow).__TAURI__ = { core: { invoke: host } };
  return world;
}

async function bootedEngine() {
  vi.resetModules();
  const engine = await import('./local-engine');
  await engine.bootLocalEngine();
  return engine;
}

afterEach(() => {
  delete (window as DesktopWindow).__TAURI__;
  vi.unstubAllGlobals();
});

describe('Vis on this computer', () => {
  it('installs the engine on request, starts its gateway and asks the app to pair', async () => {
    const world = computer({ isInstalled: false, installs: true });
    const engine = await bootedEngine();
    expect(engine.localEngineState().engine).toEqual({ kind: 'absent' });
    expect(world.calls).not.toContain('engine_run');

    await engine.installEngine();

    expect(engine.localEngineState()).toMatchObject({
      engine: { kind: 'running', version: '0.5.21' },
      isGatewayOpen: true,
      wantsPairing: true,
    });
  });

  it('keeps the log of a failed installation and starts no gateway', async () => {
    const world = computer({ isInstalled: false, installs: false });
    const engine = await bootedEngine();

    await engine.installEngine();

    expect(engine.localEngineState().engine).toEqual({
      kind: 'failed',
      task: 'install',
      reason: 'The installation stopped with an error.',
      log: ['HTTP 503'],
    });
    expect(world.calls).not.toContain('engine_gateway_start');
  });

  it('starts the gateway of an installed engine when the app opens', async () => {
    const world = computer({ isInstalled: true, installs: true });
    const engine = await bootedEngine();

    expect(world.calls).toContain('engine_gateway_start');
    expect(engine.localEngineState()).toMatchObject({
      engine: { kind: 'running' },
      isGatewayOpen: true,
      wantsPairing: false,
    });
  });

  // Report: the app paired with a gateway that asks for a token, and the gateway
  // refused each request ("Token missing or invalid").
  it('pairs with a gateway that asks for a token only with the token of this computer', async () => {
    computer({ isInstalled: true, installs: true, token: 'secret', saved: 'secret' });
    expect((await bootedEngine()).localEngineState()).toMatchObject({
      isGatewayOpen: true,
      gatewayToken: 'secret',
    });

    computer({ isInstalled: true, installs: true, token: 'secret' });
    expect((await bootedEngine()).localEngineState()).toMatchObject({
      engine: { kind: 'running' },
      isGatewayOpen: false,
      gatewayToken: null,
    });
  });

  it('compares versions by number', async () => {
    const { isNewerVersion } = await import('./local-engine');
    expect(isNewerVersion('0.5.10', '0.5.9')).toBe(true);
    expect(isNewerVersion('0.5.20', '0.5.21')).toBe(false);
  });
});
