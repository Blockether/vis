// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { AddMachine } from './Machines';

// Regression: refreshing an open session showed only "missing or invalid bearer
// token". A gateway that binds a non-loopback host demands the token, but it
// answers `/healthz` without one. Pairing probed `/healthz` alone, so a bare
// address, or a wrong token, was saved as "Connected". After that, every request
// of the saved machine was refused.

const URL_ONLY = 'http://10.0.0.5:7890';

beforeEach(() => {
  vi.stubGlobal(
    'fetch',
    vi.fn(async (input: string, init?: RequestInit) => {
      const path = new URL(String(input)).pathname;
      if (path === '/healthz') return new Response(JSON.stringify({ status: 'ok' }), { status: 200 });
      const auth = new Headers(init?.headers).get('authorization');
      if (auth !== 'Bearer right-token')
        return new Response(
          JSON.stringify({ error: { type: 'unauthorized', message: 'missing or invalid bearer token' } }),
          { status: 401 },
        );
      return new Response(JSON.stringify({ version: 1, addresses: [URL_ONLY] }), { status: 200 });
    }),
  );
});

afterEach(() => {
  vi.unstubAllGlobals();
});

async function pair(payload: string, onAdd = vi.fn(async () => {})) {
  render(<AddMachine onAdd={onAdd} />);
  fireEvent.change(screen.getByPlaceholderText(/vis:\/\/gateway/), { target: { value: payload } });
  await act(async () => {
    fireEvent.click(screen.getByRole('button', { name: /^Pair$/ }));
  });
  return onAdd;
}

describe('pairing a gateway that demands its token', () => {
  it('refuses a bare address instead of saving a machine without a token', async () => {
    const onAdd = await pair(URL_ONLY);
    await vi.waitFor(() => expect(screen.getByRole('status').textContent).toMatch(/rejected the token/));
    expect(onAdd).not.toHaveBeenCalled();
  });

  it('saves the machine when the token is accepted', async () => {
    const onAdd = await pair(`vis://gateway?url=${encodeURIComponent(URL_ONLY)}&token=right-token`);
    await vi.waitFor(() => expect(onAdd).toHaveBeenCalledTimes(1));
    expect(onAdd).toHaveBeenCalledWith(expect.objectContaining({ url: URL_ONLY, token: 'right-token' }));
  });
});
