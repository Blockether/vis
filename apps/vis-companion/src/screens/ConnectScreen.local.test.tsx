// @vitest-environment jsdom
// Report: the desktop app showed a paired machine that was offline and no way to
// use Vis on this computer, although its gateway was running.
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it, vi } from 'vitest';

type DesktopWindow = Window & { __TAURI__?: unknown };

afterEach(() => {
  cleanup();
  delete (window as DesktopWindow).__TAURI__;
  vi.unstubAllGlobals();
});

it('offers the running gateway of this computer beside a machine that is offline', async () => {
  vi.stubGlobal('fetch', async (input: RequestInfo | URL) => {
    if (!String(input).startsWith('http://127.0.0.1:7890/')) throw new TypeError('refused');
    return Response.json({ id: 'gw-local', require_token: false });
  });
  const host = async (command: string) =>
    command === 'engine_status'
      ? { supported: true, installed: true, version: '0.5.21', track: 'release' }
      : command === 'engine_progress'
        ? { task: '', running: false, ok: false, log: [] }
        : null;
  (window as DesktopWindow).__TAURI__ = { core: { invoke: host } };
  vi.resetModules();
  const engine = await import('../lib/local-engine');
  const { ConnectScreen } = await import('./ConnectScreen');
  await engine.bootLocalEngine();

  const stale = { url: 'http://127.0.0.1:7911' };
  await act(async () => {
    render(
      <ConnectScreen
        conns={[stale]}
        active={null}
        primary={stale}
        onAdd={async () => {}}
        onSettings={() => {}}
      />,
    );
  });
  fireEvent.click(screen.getByRole('button', { name: 'Connect' }));

  expect(engine.localEngineState().wantsPairing).toBe(true);
  expect(engine.isLocalPaired([stale], engine.localEngineState().gatewayId)).toBe(false);
  expect(
    engine.isLocalPaired([{ url: 'http://10.0.0.5:7890', id: 'gw-local' }], 'gw-local'),
  ).toBe(true);
});
