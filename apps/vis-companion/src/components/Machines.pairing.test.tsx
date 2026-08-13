// @vitest-environment jsdom
import { act, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import { AddMachine } from './Machines';

// Regression, user report ("there is an issue with the pairing — when I scanned
// the machine I have no idea what is going on; there is no information what is
// happening, what is the current state, how long the process will take"): the QR
// scanner narrated every wait, and the moment it decoded a code it unmounted and
// handed off to a probe that narrated NOTHING. A pairing payload carries one
// `alt=` per routable host on that machine, every candidate was probed with no
// deadline of its own, and `Promise.all` waited for the SLOWEST — so a laptop
// standing right there answered on the LAN in 20ms while the phone sat on the
// word "Checking…", printed on a DISABLED button, for the client's full 30s
// request timeout. Nothing named the machine, nothing said how many addresses
// were being tried or which had already answered, and nothing bounded the wait.

/** A gateway that answers `/healthz` after `ms` of round trip. */
function answers(ms: number): Promise<Response> {
  return new Promise((resolve) => {
    setTimeout(() => resolve(new Response(JSON.stringify({ status: 'ok' }), { status: 200 })), ms);
  });
}

/** An address nothing listens on: blackholed, so it ends only when aborted. */
function blackhole(init?: RequestInit): Promise<Response> {
  return new Promise((_resolve, reject) => {
    init?.signal?.addEventListener(
      'abort',
      () => reject(new DOMException('Aborted', 'AbortError')),
      { once: true },
    );
  });
}

/**
 * What `vis gateway pair` actually prints next to a laptop: the tailnet address
 * first, then one `alt=` per routable interface — Wi-Fi, and the virtual
 * bridges a developer machine always has. Only the LAN address is live here,
 * which is the normal case for a phone standing in the same room with Tailscale
 * off on one of the two devices.
 */
const PAYLOAD =
  'vis://gateway?url=http%3A%2F%2F100.64.0.10%3A7890' +
  '&alt=http%3A%2F%2F192.168.1.24%3A7890%2Chttp%3A%2F%2F172.17.0.1%3A7890' +
  '&token=secret-token';

const LIVE = '192.168.1.24';

beforeEach(() => {
  vi.useFakeTimers();
  vi.stubGlobal(
    'fetch',
    vi.fn((url: string, init?: RequestInit) =>
      String(url).includes(LIVE) ? answers(20) : blackhole(init),
    ),
  );
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

/** Paste the scanned payload and press Pair, the way the QR path lands. */
async function pair(onAdd = vi.fn(async () => {})) {
  const view = render(<AddMachine onAdd={onAdd} />);
  fireEvent.change(screen.getByPlaceholderText(/vis:\/\/gateway/), {
    target: { value: PAYLOAD },
  });
  await act(async () => {
    fireEvent.click(screen.getByRole('button', { name: /^Pair$/ }));
  });
  return { ...view, onAdd };
}

/** Let fake time pass and every repaint it caused land. */
async function tick(ms: number): Promise<void> {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
}

describe('pairing after a scan says what it is doing', () => {
  it('is bounded by its own deadline, not the client request timeout', async () => {
    const { onAdd } = await pair();
    // The live address answered in 20ms. Every remaining candidate is dead, and
    // waiting them out is the reported 30s of silence.
    await tick(12_000);
    expect(onAdd).toHaveBeenCalledTimes(1);
    expect(screen.getByRole('status').textContent).toMatch(/Connected to/);
  });

  it('names the machine, the addresses and the progress while it probes', async () => {
    await pair();
    await tick(50);
    const said = document.body.textContent ?? '';
    // WHAT is happening, and to WHICH machine.
    expect(said).toMatch(/100\.64\.0\.10/);
    // WHAT THE CURRENT STATE IS: how many addresses, how many are settled.
    expect(said).toMatch(/3 addresses/);
    // HOW LONG IT CAN TAKE: an explicit bound, not an open-ended spinner.
    expect(said).toMatch(/\b\d+s\b/);
  });

  it('reports each address by the reach that makes it durable', async () => {
    await pair();
    await tick(50);
    const said = document.body.textContent ?? '';
    expect(said).toMatch(/Tailscale/);
    expect(said).toMatch(/Local network/);
  });

  it('can be given up on, instead of being an unbreakable wait', async () => {
    await pair();
    await tick(50);
    const stop = screen.getByRole('button', { name: /Stop/ });
    await act(async () => {
      fireEvent.click(stop);
    });
    expect(screen.getByRole('status').textContent).toMatch(/Stopped/);
  });
});
