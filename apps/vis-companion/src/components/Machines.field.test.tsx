// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { AddMachine } from './Machines';
import type { GatewayConn } from '../lib/types';

// Regression, user report (paraphrased: pairing was two cards, "Pairing link" and
// "URL + token", each with its own button, stretched across the desktop, and neither
// said what to run on the other machine): one handshake was asked for through two
// forms, and the reader had to work out which was theirs before knowing that the
// only difference was a token. Now ONE field takes whatever the terminal printed —
// the `vis://` link or a bare address — and the token is asked for only when the
// address does not carry one. The page itself is the instructions.

const LINK = 'vis://gateway?url=http%3A%2F%2F192.168.1.24%3A7890&token=secret-token';

/** A desktop with a mouse: the reader is at the screen that printed the code. */
function pointing(kind: 'fine' | 'coarse') {
  vi.stubGlobal('matchMedia', (query: string) => ({
    matches: query.includes('pointer: fine') ? kind === 'fine' : false,
    media: query,
    onchange: null,
    addListener: () => undefined,
    removeListener: () => undefined,
    addEventListener: () => undefined,
    removeEventListener: () => undefined,
    dispatchEvent: () => false,
  }));
}

/** Every address answers at once; only the payload is under test here. */
beforeEach(() => {
  pointing('fine');
  vi.stubGlobal(
    'fetch',
    vi.fn(() => Promise.resolve(new Response(JSON.stringify({ status: 'ok' }), { status: 200 }))),
  );
});

afterEach(() => {
  cleanup();
  vi.unstubAllGlobals();
});

const field = () => screen.getByRole('textbox', { name: 'Pairing link or machine address' });
const tokenField = () => screen.queryByRole('textbox', { name: 'Bearer token' });

function type(value: string) {
  fireEvent.change(field(), { target: { value } });
}

async function pair() {
  await act(async () => {
    fireEvent.click(screen.getByRole('button', { name: /^Pair$/ }));
  });
}

describe('one field takes whatever the terminal printed', () => {
  it('pairs from a pasted link without asking for a token', async () => {
    const onAdd = vi.fn(async (_conn: GatewayConn) => {});
    render(<AddMachine onAdd={onAdd} />);
    type(LINK);
    expect(tokenField()).toBeNull();
    await pair();
    expect(onAdd).toHaveBeenCalledTimes(1);
    expect(onAdd.mock.calls[0][0]).toMatchObject({
      url: 'http://192.168.1.24:7890',
      token: 'secret-token',
    });
  });

  it('asks for the token only once a bare address is typed, and pairs with it', async () => {
    const onAdd = vi.fn(async (_conn: GatewayConn) => {});
    render(<AddMachine onAdd={onAdd} />);
    expect(tokenField()).toBeNull();
    type('10.0.0.5:7890');
    const token = tokenField();
    expect(token).toBeVisible();
    fireEvent.change(token!, { target: { value: 'tok' } });
    await pair();
    expect(onAdd.mock.calls[0][0]).toMatchObject({ url: 'http://10.0.0.5:7890', token: 'tok' });
  });

  it('names what it could not read instead of pairing with nothing', async () => {
    const onAdd = vi.fn(async (_conn: GatewayConn) => {});
    render(<AddMachine onAdd={onAdd} />);
    type('not an address');
    await pair();
    expect(onAdd).not.toHaveBeenCalled();
    expect(document.body.textContent).toMatch(
      /"not an address" is not a pairing link or a machine address/,
    );
  });
});
