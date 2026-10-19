// @vitest-environment jsdom
// Regression for #397: the page `vis-agent web` opens connects itself to the
// gateway that served it, with no saved machine and no pasted token.
import { act, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';

import { renderApp } from './app-harness';
import type { GatewayConn } from './lib/types';

const saved = (): GatewayConn[] =>
  JSON.parse(localStorage.getItem('CapacitorStorage.vis.connections') ?? '[]');

let view: ReturnType<typeof renderApp> | undefined;
afterEach(() => {
  view?.unmount();
  view?.restore();
  view = undefined;
});

describe('vis-agent web launch', () => {
  it('connects a fresh browser to the gateway that served the page', async () => {
    const origin = window.location.origin;
    await act(async () => {
      view = renderApp({
        initiallyPaired: false,
        machines: [
          {
            url: origin,
            routes: {
              '/v1/web/launches/redeem': {
                gateway_id: 'launched-gateway',
                token: 'launch-token',
                require_token: true,
              },
            },
          },
        ],
        launch: { code: 'one-time-code', gatewayId: 'launched-gateway' },
      });
    });
    await waitFor(() => {
      expect(saved()).toEqual([
        expect.objectContaining({ url: origin, token: 'launch-token' }),
      ]);
    });
    // The app opens the launched gateway's sessions, not the pairing screen.
    await waitFor(() => {
      expect(view!.requests.some((url) => url.startsWith(`${origin}/v1/sessions`))).toBe(true);
    });
    expect(view!.requests).toContain(`${origin}/v1/web/launches/redeem`);
    expect(view!.queryByText(/Add a machine/i)).toBeNull();
  });

  it('shows why a used launch did not connect, on the machines screen', async () => {
    const origin = window.location.origin;
    await act(async () => {
      view = renderApp({
        initiallyPaired: false,
        machines: [{ url: origin, routes: { '/v1/web/launches/redeem': { gateway_id: 'someone-else' } } }],
        launch: { code: 'one-time-code', gatewayId: 'launched-gateway' },
      });
    });
    await waitFor(() => {
      expect(view!.getByText(/is not the one that `vis-agent web` started/)).toBeTruthy();
    });
    expect(saved()).toEqual([]);
  });

  it('keeps the launch error on screen when this address is already saved', async () => {
    // The live check of #397: a second browser that opened a used launch address
    // already held this gateway without a token, and the list hid the error.
    const origin = window.location.origin;
    await act(async () => {
      view = renderApp({
        machines: [{ url: origin, routes: { '/v1/web/launches/redeem': { gateway_id: 'someone-else' } } }],
        launch: { code: 'one-time-code', gatewayId: 'launched-gateway' },
      });
    });
    await waitFor(() => {
      expect(view!.getByText(/is not the one that `vis-agent web` started/)).toBeTruthy();
    });
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 50));
    });
    expect(view!.getByText(/is not the one that `vis-agent web` started/)).toBeTruthy();
    expect(window.location.hash).toBe('#/connect');
  });
});
