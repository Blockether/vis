// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { SettingsDialog } from './SettingsScreen';
import type { GatewayConn } from '../lib/types';

const URL_A = 'http://10.0.0.5:7890';
const URL_B = 'http://10.0.0.6:7890';

/** A machine that answers every read with an empty body, so the bands can paint. */
const quiet = () =>
  Promise.resolve(
    new Response(JSON.stringify({}), {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
    }),
  );

/** A reachable gateway whose protocol floor excludes this app build. */
const incompatibleSettings = (input: RequestInfo | URL) => {
  const path = new URL(String(input), URL_A).pathname;
  if (path !== '/v1/settings') return quiet();
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

const open = (gateways: GatewayConn[], providerMachineUrl?: string) =>
  render(
    <SettingsDialog
      gateways={gateways}
      providerMachineUrl={providerMachineUrl}
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
  it('starts every machine closed and opens one only after its row is pressed', async () => {
    const view = open([
      { url: URL_A, token: 't', id: 'be2c15686eaef0f4' },
      { url: URL_B, token: 't', id: 'cad6247b600f9bbc' },
    ]);

    const [first, second] = await onlineRows(2);
    expect(first).toHaveAttribute('aria-expanded', 'false');
    expect(second).toHaveAttribute('aria-expanded', 'false');
    expect(screen.queryByText('MCP servers')).toBeNull();

    fireEvent.click(first);

    expect(first).toHaveAttribute('aria-expanded', 'true');
    expect(second).toHaveAttribute('aria-expanded', 'false');
    await waitFor(() => expect(screen.getByText('MCP servers')).toBeTruthy());
    view.unmount();
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
    await waitFor(() => expect(screen.getByText('Providers')).toBeTruthy());
    view.unmount();
  });
  // Regression, issue #ea166d2d-d22f-4a89-b117-d058641b7422: a protocol refusal
  // proves the machine answered, so no unreachable-machine panel may follow it.
  it('does not call a protocol-incompatible machine unreachable', async () => {
    globalThis.fetch = vi.fn(incompatibleSettings) as unknown as typeof fetch;
    const view = open([{ url: URL_A, token: 't', id: 'be2c15686eaef0f4' }]);

    const [row] = await onlineRows();
    fireEvent.click(row);

    await waitFor(() =>
      expect(screen.getAllByText(/gateway speaks protocol 4/i).length).toBeGreaterThan(0),
    );
    expect(screen.queryByText('Machine unreachable')).toBeNull();
    expect(screen.queryByRole('button', { name: 'Retry' })).toBeNull();
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
    expect(screen.queryByText('MCP servers')).toBeNull();

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
    expect(screen.queryByText('MCP servers')).toBeNull();

    await act(async () => answer(await quiet()));
    const [row] = await onlineRows();
    expect(row).toHaveAttribute('aria-expanded', 'false');
    expect(row).not.toHaveAttribute('aria-busy');
    expect(
      fetcher.mock.calls.every(([input]) => new URL(String(input)).pathname === '/healthz'),
    ).toBe(true);
    fetcher.mockImplementation(quiet);
    fireEvent.click(row);
    await waitFor(() => expect(screen.getByText('MCP servers')).toBeTruthy());
    view.unmount();
  });
});
