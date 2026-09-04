// @vitest-environment jsdom
import { act, cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import connectScreenSource from '../screens/ConnectScreen.tsx?raw';
import settingsScreenSource from '../screens/SettingsScreen.tsx?raw';
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
    vi.fn(() =>
      Promise.resolve(new Response(JSON.stringify({ status: 'ok' }), { status: 200 })),
    ),
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
  it('explains the three steps in the order they happen, command first', () => {
    render(<AddMachine onAdd={vi.fn(async () => {})} />);
    const steps = screen.getAllByRole('heading', { level: 3 }).map((h) => h.textContent);
    expect(steps).toEqual(['On the machine that runs vis', 'Copy the link it printed', 'Pair']);
    // Under a mouse this IS the machine with the terminal, so the second step says
    // copy, and the field's verb is the only primary one on the page.
    expect(screen.queryByRole('button', { name: 'Scan QR' })).toBeNull();
    // The command is there to be TAKEN to the other machine, not read off the screen.
    expect(screen.getByRole('button', { name: 'Copy command' })).toBeTruthy();
    const command = document.querySelector('code');
    expect(command?.textContent).toBe(
      'vis-agent gateway start --host 0.0.0.0 --require-token --pair',
    );
    // Each word is one unbreakable box so a flag never tears at its hyphen, and the
    // spaces between them are text of the line, not the leading edge of a box — a
    // space inside an inline block collapses, and the command read as one word.
    const words = Array.from(command?.querySelectorAll('span') ?? []);
    expect(words.map((w) => w.textContent)).toEqual([
      'vis-agent', 'gateway', 'start', '--host', '0.0.0.0', '--require-token', '--pair',
    ]);
    const gaps = Array.from(command?.childNodes ?? []).filter((n) => n.nodeType === Node.TEXT_NODE);
    expect(gaps.map((n) => n.textContent)).toEqual([' ', ' ', ' ', ' ', ' ', ' ']);
  });

  it('is a decision, not steps, on the device in your hand', () => {
    pointing('coarse');
    render(<AddMachine onAdd={vi.fn(async () => {})} />);
    // A phone has the camera: the two ways the code gets here are alternatives,
    // so neither carries an ordinal and scanning is the one primary verb.
    const ways = screen.getAllByRole('heading', { level: 3 }).map((h) => h.textContent);
    expect(ways).toEqual(['Scan the QR code', 'I have a pairing link']);
    expect(document.body.textContent).not.toMatch(/^\s*1\s/m);
    expect(screen.getByRole('button', { name: 'Scan QR' })).toBeTruthy();
    // The start command has nowhere to be pasted from a phone, so it is not shown;
    // the field still takes the link or a bare address.
    expect(screen.queryByRole('button', { name: 'Copy command' })).toBeNull();
    expect(document.body.textContent).not.toContain('--require-token');
    expect(field()).toBeTruthy();
  });

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
    expect(token).toBeTruthy();
    fireEvent.change(token!, { target: { value: 'tok' } });
    await pair();
    expect(onAdd.mock.calls[0][0]).toMatchObject({ url: 'http://10.0.0.5:7890', token: 'tok' });
  });

  it('submits on Enter from the field', async () => {
    const onAdd = vi.fn(async (_conn: GatewayConn) => {});
    render(<AddMachine onAdd={onAdd} />);
    type(LINK);
    await act(async () => {
      fireEvent.keyDown(field(), { key: 'Enter' });
    });
    expect(onAdd).toHaveBeenCalledTimes(1);
  });

  it('names what it could not read instead of pairing with nothing', async () => {
    const onAdd = vi.fn(async (_conn: GatewayConn) => {});
    render(<AddMachine onAdd={onAdd} />);
    type('not an address');
    await pair();
    expect(onAdd).not.toHaveBeenCalled();
    expect(document.body.textContent).toMatch(/"not an address" is not a pairing link or a machine address/);
  });

  it('is one component at every width: the owner decides the frame, not a prop', () => {
    // The settings dialog is a column and the connect page a full-width plane; the
    // component reads its own width, so neither owner is allowed to tell it.
    expect(settingsScreenSource).not.toContain('isStacked');
    expect(connectScreenSource).not.toContain('isStacked');
  });
});
