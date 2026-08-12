// @vitest-environment jsdom
import { render, act } from '@testing-library/react';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import type { GatewayConn } from '../lib/types';
import { MachineRows, useFleetHealth } from './Machines';

// Regression, user report ("why are we telling it's green if it's not — I closed
// this laptop and the gateway is not accessible"): a machine row painted the LAST
// remembered probe as if it were live. Re-entering the surface seeded every dot
// from a module-level cache, the sweep kept the old verdict on screen while the
// new probe ran, and the 6s tick was skipped whenever `document.visibilityState`
// said `hidden` — which a resumed iOS webview keeps saying while the reader is
// looking at it. So a laptop that had gone to sleep hours earlier kept a green
// dot and the latency it had answered with when it was still awake.

/** One gateway's health, rendered exactly as the settings column renders it. */
function Fleet({ conns, watch }: { conns: GatewayConn[]; watch?: { url?: string | null; onRecovered?: () => void } }) {
  const health = useFleetHealth(conns, watch);
  return <MachineRows conns={conns} health={health} onPick={() => {}} />;
}

/** What the dot says about a machine right now: its `title` is the verdict. */
function verdict(container: HTMLElement): string {
  const dot = container.querySelector('[title]');
  return dot?.getAttribute('title') ?? '';
}

/** The latency printed beside the address, or '' when the row prints none. */
function latency(container: HTMLElement): string {
  return (
    Array.from(container.querySelectorAll('span'))
      .map((s) => s.textContent ?? '')
      .find((t) => /^\d+ms$/.test(t)) ?? ''
  );
}

/** A gateway that answers `/healthz`, in `ms` of measured round trip. */
function answers(ms = 50): Promise<Response> {
  return new Promise((resolve) => {
    setTimeout(() => resolve(new Response(JSON.stringify({ status: 'ok' }), { status: 200 })), ms);
  });
}

/**
 * A closed laptop: the socket is not refused, it is blackholed, so the request
 * only ends when the probe's own deadline aborts it.
 */
function blackhole(init?: RequestInit): Promise<Response> {
  return new Promise((_resolve, reject) => {
    init?.signal?.addEventListener(
      'abort',
      () => reject(new DOMException('Aborted', 'AbortError')),
      { once: true },
    );
  });
}

let isAwake = true;

beforeEach(() => {
  vi.useFakeTimers();
  isAwake = true;
  vi.stubGlobal(
    'fetch',
    vi.fn((_url: string, init?: RequestInit) => (isAwake ? answers() : blackhole(init))),
  );
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
});

/** Let every probe of one sweep start, answer and repaint. */
async function settle(ms = 200): Promise<void> {
  await act(async () => {
    await vi.advanceTimersByTimeAsync(ms);
  });
}

describe('a machine that stopped answering while nobody was looking', () => {
  it('never shows a remembered verdict as the machine state', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.5:7890', label: 'Macbook' };
    const first = render(<Fleet conns={[conn]} />);
    await settle();
    expect(verdict(first.container)).toBe('Online');
    expect(latency(first.container)).toMatch(/^\d+ms$/);
    first.unmount();

    // The lid closes, and the surface is reopened much later.
    isAwake = false;
    await act(async () => {
      await vi.advanceTimersByTimeAsync(5 * 60_000);
    });
    const again = render(<Fleet conns={[conn]} />);

    // Before any probe has answered, the honest verdict is that nobody knows.
    expect(verdict(again.container)).toBe('Checking\u2026');
    expect(latency(again.container)).toBe('');
    await settle(12_000);
    expect(verdict(again.container)).toBe('Offline');
  });

  it('keeps sweeping while a resumed webview still reports itself hidden', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.6:7890', label: 'RBI' };
    const { container } = render(<Fleet conns={[conn]} />);
    await settle();
    expect(verdict(container)).toBe('Online');

    // iOS hands back a foreground webview that still says `hidden`; the reader
    // is watching this row the whole time.
    Object.defineProperty(document, 'visibilityState', { value: 'hidden', configurable: true });
    isAwake = false;
    await settle(30_000);
    expect(verdict(container)).toBe('Offline');
    Object.defineProperty(document, 'visibilityState', { value: 'visible', configurable: true });
  });

  it('goes on saying Online for a machine that keeps answering', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.7:7890', label: 'tower' };
    const { container } = render(<Fleet conns={[conn]} />);
    await settle();
    // Four sweeps with no gap between verdicts: freshness must never make a live
    // machine flicker through "Checking…".
    for (let i = 0; i < 4; i += 1) {
      await settle(6_000);
      expect(verdict(container)).toBe('Online');
    }
    expect(latency(container)).toMatch(/^\d+ms$/);
  });

  it('lifts the offline gate on a probe, never on the memory of one', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.8:7890', label: 'Macbook' };
    const alive = render(<Fleet conns={[conn]} />);
    await settle();
    alive.unmount();

    // The gate goes up, the gateway is gone, and the screen that put the gate up
    // mounts the same list. A remembered "online" used to call `onRecovered`
    // before a single request left the device.
    isAwake = false;
    const onRecovered = vi.fn();
    render(<Fleet conns={[conn]} watch={{ url: conn.url, onRecovered }} />);
    await settle(12_000);
    expect(onRecovered).not.toHaveBeenCalled();

    // It answers again, and THAT is what lifts the gate — one sweep later,
    // because the request already in flight was made to a machine that was gone.
    isAwake = true;
    await settle(30_000);
    expect(onRecovered).toHaveBeenCalled();
  });
});
