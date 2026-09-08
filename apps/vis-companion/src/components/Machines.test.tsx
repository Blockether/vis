// @vitest-environment jsdom
import { render, act, fireEvent, screen } from '@testing-library/react';
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
  const { health, retry } = useFleetHealth(conns, watch);
  return <MachineRows conns={conns} health={health} onPick={() => {}} onRetry={retry} />;
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
  it('retries one machine immediately while another probe is pending, without duplicates', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.5:7891', label: 'retry target' };
    const slow: GatewayConn = { url: 'http://10.0.0.5:7892', label: 'slow machine' };
    const onRecovered = vi.fn();
    let attempts = 0;
    vi.stubGlobal('fetch', vi.fn((url: string, init?: RequestInit) => {
      if (url.startsWith(slow.url)) return blackhole(init);
      attempts += 1;
      return attempts === 1 ? Promise.reject(new TypeError('Load failed')) : answers();
    }));
    render(<Fleet conns={[conn, slow]} watch={{ url: conn.url, onRecovered }} />);
    await settle();

    fireEvent.click(screen.getByRole('button', { name: 'Retry connection to retry target' }));
    const checking = screen.getByRole('button', { name: 'Checking connection to retry target' });
    expect(checking).toHaveAttribute('aria-busy', 'true');
    expect(checking).toHaveAttribute('aria-disabled', 'true');
    expect(checking.querySelector('.lucide-refresh-cw')).toHaveClass('animate-spin');
    fireEvent.click(checking);
    fireEvent.click(checking);
    expect(attempts).toBe(2);
    expect(fetch).toHaveBeenCalledTimes(3);
    expect(onRecovered).not.toHaveBeenCalled();

    await settle();
    expect(screen.getByRole('button', { name: /retry target/ })).not.toHaveAttribute('aria-busy');
    expect(screen.getByRole('button', { name: 'Checking connection to slow machine' })).toHaveAttribute('aria-busy', 'true');
    expect(onRecovered).toHaveBeenCalledTimes(1);
  });

  it('returns to retry after a failed check', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.5:7893', label: 'unavailable' };
    vi.stubGlobal('fetch', vi.fn(() => Promise.reject(new TypeError('Load failed'))));
    render(<Fleet conns={[conn]} />);
    await settle();
    fireEvent.click(screen.getByRole('button', { name: 'Retry connection to unavailable' }));
    expect(screen.getByRole('button', { name: 'Checking connection to unavailable' })).toHaveAttribute('aria-busy', 'true');
    await settle();
    const retry = screen.getByRole('button', { name: 'Retry connection to unavailable' });
    expect(retry).not.toHaveAttribute('aria-disabled');
    expect(retry).not.toHaveAttribute('aria-expanded');
    expect(fetch).toHaveBeenCalledTimes(2);
  });

  it('uses a background check already in flight when the same row is retried', async () => {
    const conn: GatewayConn = { url: 'http://10.0.0.5:7894', label: 'pending retry' };
    const fetcher = vi.fn<typeof fetch>().mockRejectedValueOnce(new TypeError('Load failed'));
    fetcher.mockImplementation((_url, init) => blackhole(init));
    vi.stubGlobal('fetch', fetcher);
    render(<Fleet conns={[conn]} />);
    await settle();
    await settle(6_000);
    expect(fetcher).toHaveBeenCalledTimes(2);

    fireEvent.click(screen.getByRole('button', { name: 'Retry connection to pending retry' }));
    expect(screen.getByRole('button', { name: 'Checking connection to pending retry' })).toHaveAttribute('aria-busy', 'true');
    expect(fetcher).toHaveBeenCalledTimes(2);
    await settle(9_000);
    expect(screen.getByRole('button', { name: 'Retry connection to pending retry' })).not.toHaveAttribute('aria-busy');
    expect(fetcher).toHaveBeenCalledTimes(2);
  });
});
