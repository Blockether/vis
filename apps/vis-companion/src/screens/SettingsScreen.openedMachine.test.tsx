// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { SettingsDialog } from './SettingsScreen';
import type { GatewayConn } from '../lib/types';

const URL_A = 'http://10.0.0.5:7890';
const URL_B = 'http://10.0.0.6:7890';

/**
 * A machine that answers every read with an empty body, so the bands can paint. Its
 * settings catalog still names a revision, because the client refuses one that does not.
 */
const quiet = (input?: RequestInfo | URL) =>
  Promise.resolve(
    new Response(
      JSON.stringify(
        input && new URL(String(input), URL_A).pathname === '/v1/settings'
          ? { revision: 'quiet-1', groups: [] }
          : {},
      ),
      {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      },
    ),
  );

/** A reachable gateway whose protocol floor excludes this app build. */
const incompatibleSettings = (input: RequestInfo | URL) => {
  const path = new URL(String(input), URL_A).pathname;
  if (path !== '/v1/settings') return quiet(input);
  return Promise.resolve(
    new Response(
      JSON.stringify({
        error: {
          type: 'incompatible_protocol',
          title: 'Update this client',
          message: 'The gateway speaks protocol 4 and no longer serves clients below protocol 4.',
        },
      }),
      { status: 426, headers: { 'Content-Type': 'application/json' } },
    ),
  );
};

let previousFetch: typeof fetch;

beforeEach(() => {
  previousFetch = globalThis.fetch;
  globalThis.fetch = vi.fn(quiet) as unknown as typeof fetch;
});

afterEach(() => {
  cleanup();
  globalThis.fetch = previousFetch;
  globalThis.localStorage?.clear();
  vi.restoreAllMocks();
});

const open = (gateways: GatewayConn[], providerMachineUrl?: string, providerId?: string) =>
  render(
    <SettingsDialog
      gateways={gateways}
      providerMachineUrl={providerMachineUrl}
      providerId={providerId}
      onAddMachine={async () => {}}
      onClose={() => {}}
    />,
  );

/** A row becomes a disclosure only once its health check answers. */
const onlineRows = (count = 1) =>
  waitFor(() => {
    const rows = screen
      .getAllByRole('button')
      .filter((row) => row.querySelector('[title="Online"]'));
    expect(rows).toHaveLength(count);
    return rows;
  });

describe('machine settings disclosures', () => {
  it('opens a sole machine automatically and keeps it open when its name is pressed', async () => {
    open([{ url: URL_A, token: 't', label: 'tower' }]);

    expect(await screen.findByText('Tools')).toBeVisible();
    expect(screen.getByText('Providers')).toBeVisible();
    const name = screen.getByText('tower');
    expect(name.closest('button')).toBeNull();
    fireEvent.click(name);
    expect(screen.getByText('Tools')).toBeVisible();
  });

  it('opens the requested machine directly at its provider settings', async () => {
    const view = open(
      [
        { url: URL_A, token: 't', id: 'be2c15686eaef0f4' },
        { url: URL_B, token: 't', id: 'cad6247b600f9bbc' },
      ],
      URL_B,
    );

    const [first, second] = await onlineRows(2);
    expect(first).toHaveAttribute('aria-expanded', 'false');
    expect(second).toHaveAttribute('aria-expanded', 'true');
    await waitFor(() => expect(screen.getByText('Providers')).toBeVisible());
    view.unmount();
  });

  // Regression, issue #406: a provider's Sign in button in the model picker opened
  // generic machine settings, so the user had to find that provider again.
  it('focuses the provider whose Sign in button opened the settings', async () => {
    const URL_C = 'http://10.0.0.9:7890';
    const signedOut = (id: string, auth_kind: 'api-key' | 'oauth') => ({
      id,
      label: id.toUpperCase(),
      is_managed: false,
      auth_kind,
      models: ['m1'],
      is_default: false,
      default_model: null,
      is_fallback: false,
      fallback_model: null,
      status: { is_authenticated: false, auth_state: 'unverified' },
    });
    globalThis.fetch = vi.fn((input?: RequestInfo | URL) =>
      input && new URL(String(input), URL_C).pathname === '/v1/router'
        ? Promise.resolve(
            new Response(
              JSON.stringify({ providers: [signedOut('oauth-plan', 'oauth'), signedOut('token-plan', 'api-key')] }),
              { status: 200, headers: { 'Content-Type': 'application/json' } },
            ),
          )
        : quiet(input),
    ) as unknown as typeof fetch;
    const view = open([{ url: URL_C, token: 't', id: 'de2c15686eaef0f4' }], URL_C, 'token-plan');

    const token = await screen.findByRole('button', { name: /^TOKEN-PLAN/ });
    const oauth = screen.getByRole('button', { name: /^OAUTH-PLAN/ });
    await waitFor(() => expect(document.activeElement).toBe(token));
    expect(token).toHaveAttribute('aria-current', 'true');
    expect(oauth).not.toHaveAttribute('aria-current');
    view.unmount();
  });
  // Regression, issue #ea166d2d-d22f-4a89-b117-d058641b7422: a protocol refusal
  // proves the machine answered, so no unreachable-machine panel may follow it.
  it('does not call a protocol-incompatible machine unreachable', async () => {
    globalThis.fetch = vi.fn(incompatibleSettings) as unknown as typeof fetch;
    const view = open([{ url: URL_A, token: 't', id: 'be2c15686eaef0f4' }]);

    await waitFor(() =>
      expect(screen.getAllByText(/gateway speaks protocol 4/i).length).toBeGreaterThan(0),
    );
    expect(screen.queryByText('Machine unreachable')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Retry' })).toBeNull();
    view.unmount();
  });
  // Regression: on a phone the settings sheet is as tall as its content. The sections
  // painted before the catalog answered, so a refusal shrank the sheet from full height.
  it('mounts no settings section before the machine answers with its catalog', async () => {
    let refuse!: () => void;
    const fetcher = vi.fn((input: RequestInfo | URL) => {
      if (new URL(String(input), URL_A).pathname !== '/v1/settings') return quiet(input);
      return new Promise<Response>((resolve) => {
        refuse = () => void incompatibleSettings(input).then(resolve);
      });
    });
    globalThis.fetch = fetcher as unknown as typeof fetch;
    // A machine no earlier case has read, so no cached catalog paints first.
    const view = open([{ url: 'http://10.0.0.7:7890', token: 't', id: 'd41f7a9e03b2c5e8' }]);

    expect(await screen.findByText('Loading', {}, { timeout: 2000 })).toBeVisible();
    expect(screen.queryByText('Providers')).toBeNull();
    expect(screen.queryByText('Tools')).toBeNull();

    await act(async () => refuse());
    await waitFor(() =>
      expect(screen.getAllByText(/gateway speaks protocol 4/i).length).toBeGreaterThan(0),
    );
    expect(screen.queryByText('Providers')).toBeNull();
    const paths = fetcher.mock.calls.map(([input]) => new URL(String(input), URL_A).pathname);
    expect(paths).not.toContain('/v1/mcp/servers');
    expect(paths).not.toContain('/v1/automations');
    view.unmount();
  });
  it('retries an offline machine without fetching or expanding its settings', async () => {
    const conn = { url: 'http://10.0.0.5:7891', label: 'laptop' };
    const fetcher = vi.fn<typeof fetch>().mockRejectedValue(new TypeError('Load failed'));
    globalThis.fetch = fetcher;
    const view = open([conn]);
    const retry = await screen.findByRole('button', { name: 'Retry connection to laptop' });
    expect(retry).not.toHaveAttribute('aria-expanded');
    expect(screen.queryByText('Machine unreachable')).toBeNull();
    expect(screen.queryByText(/Can't load settings/)).toBeNull();
    expect(screen.queryByText('Tools')).toBeNull();

    let answer!: (response: Response) => void;
    fetcher.mockImplementationOnce(
      () =>
        new Promise<Response>((resolve) => {
          answer = resolve;
        }),
    );
    fireEvent.click(retry);
    const checking = screen.getByRole('button', { name: 'Checking connection to laptop' });
    expect(checking).toHaveAttribute('aria-busy', 'true');
    fireEvent.click(checking);
    expect(fetcher).toHaveBeenCalledTimes(2);
    expect(screen.queryByText('Tools')).toBeNull();

    fetcher.mockImplementation(quiet);
    await act(async () => answer(await quiet()));
    await waitFor(() => expect(screen.getByText('Tools')).toBeVisible());
    expect(screen.getByText('laptop').closest('button')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Checking connection to laptop' })).toBeNull();
    view.unmount();
  });
});
