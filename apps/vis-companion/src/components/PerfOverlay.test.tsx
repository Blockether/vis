// @vitest-environment jsdom
import { act, fireEvent, render, screen, within } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';

import type { MemoryCell, PerfReport } from '../lib/perf';
import { formatBytes, mountPerfOverlay, PerfOverlay } from './PerfOverlay';

const MB = 1024 * 1024;

function report(overrides: Partial<PerfReport> = {}): PerfReport {
  return {
    at: 0,
    heap: { used: 48 * MB, total: 64 * MB, limit: 4096 * MB },
    domNodes: 1200,
    listeners: {
      live: 10,
      detached: 0,
      detachedByTarget: [],
      groups: [
        { target: 'Window', type: 'resize', site: 'useViewport @ src/lib/viewport.ts:12', live: 6, added: 6 },
        { target: 'AbortSignal', type: 'abort', site: 'fetchJson @ src/lib/gateway.ts:40', live: 4, added: 9 },
      ],
    },
    intervals: { live: 1, sites: [{ site: 'poll @ src/lib/health.ts:8', live: 1 }] },
    timeouts: { pending: 2, sites: [] },
    observers: [{ kind: 'ResizeObserver', observers: 3, targets: 5, detached: 0 }],
    objectUrls: { live: 2, bytes: 2048 },
    cells: [
      { source: 'transcripts', session: 's-big', title: 'Big session', bytes: 30 * MB, entries: 400 },
      { source: 'stream buffer', session: 's-big', title: 'Big session', bytes: 2 * MB, entries: 800 },
      { source: 'transcripts', session: 's-small', title: 'Small session', bytes: 512 * 1024, entries: 20 },
      { source: 'gateway snapshots', bytes: 64 * 1024, entries: 3 },
    ],
    ...overrides,
  };
}

function tableRows(): string[][] {
  const table = screen.getByRole('table', { name: 'Memory by session' });
  return within(table)
    .getAllByRole('row')
    .map((row) => Array.from(row.querySelectorAll('th, td'), (cell) => cell.textContent ?? ''));
}

function figure(label: string): HTMLElement {
  const value = screen.getByText(label).nextElementSibling;
  if (!(value instanceof HTMLElement)) throw new Error(`No value for ${label}`);
  return value;
}

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
  window.location.hash = '';
});

describe('formatBytes', () => {
  it('picks the largest readable unit', () => {
    expect(formatBytes(12)).toBe('12 B');
    expect(formatBytes(1536)).toBe('2 KB');
    expect(formatBytes(1.5 * MB)).toBe('1.5 MB');
    expect(formatBytes(3 * 1024 * MB)).toBe('3.0 GB');
  });
});

describe('memory overlay', () => {
  it('shows the platform counters and what each session holds, heaviest first', () => {
    window.location.hash = '#/s/s-small';
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} />);

    expect(screen.getByRole('region', { name: 'Memory overlay' })).toBeInTheDocument();
    expect(figure('JS heap')).toHaveTextContent('48.0 MB');
    expect(figure('Elements')).toHaveTextContent((1200).toLocaleString());
    expect(figure('Listeners')).toHaveTextContent('10');
    expect(figure('On removed elements')).not.toHaveClass('text-err');
    expect(figure('Observed elements')).toHaveTextContent('5');
    expect(figure('Object URLs')).toHaveTextContent('2 KB');
    expect(tableRows()).toEqual([
      ['Session', 'transcripts', 'stream buffer', 'gateway snapshots', 'Total'],
      ['Big session', '30.0 MB', '2.0 MB', '·', '32.0 MB'],
      ['Small session', '512 KB', '·', '·', '512 KB'],
      ['Machine-wide', '·', '·', '64 KB', '64 KB'],
    ]);
    expect(screen.getByRole('rowheader', { name: 'Small session' }).closest('tr')).toHaveAttribute('aria-current', 'true');
    expect(screen.getByRole('rowheader', { name: 'Big session' }).closest('tr')).not.toHaveAttribute('aria-current');
    expect(screen.getByText(/approximate bytes/)).toBeInTheDocument();
    expect(screen.getByText(/4 · AbortSignal · abort · fetchJson/)).toBeInTheDocument();
    expect(screen.getByText(/1 · poll @ src\/lib\/health\.ts:8/)).toBeInTheDocument();
  });

  it('switches the heatmap between approximate bytes and item counts', () => {
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} />);

    fireEvent.click(screen.getByRole('button', { name: 'Show items' }));

    expect(tableRows()).toEqual([
      ['Session', 'stream buffer', 'transcripts', 'gateway snapshots', 'Total'],
      ['Big session', '800', '400', '·', (1200).toLocaleString()],
      ['Small session', '·', '20', '·', '20'],
      ['Machine-wide', '·', '·', '3', '3'],
    ]);
    expect(screen.getByText(/\(items\)/)).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Show bytes' }));
    expect(tableRows()[1]).toEqual(['Big session', '30.0 MB', '2.0 MB', '·', '32.0 MB']);
  });

  it('folds sessions beyond the sixteen heaviest into the caption', () => {
    const cells: MemoryCell[] = Array.from({ length: 17 }, (_, index) => ({
      source: 'transcripts',
      session: `s-${index}`,
      title: `Session ${index}`,
      bytes: (index + 1) * 1024,
      entries: 1,
    }));
    render(<PerfOverlay startOpen read={() => report({ cells })} refreshMs={60_000} />);

    expect(tableRows()).toHaveLength(17);
    expect(screen.getByText(/1 lighter session not shown/)).toBeInTheDocument();
    expect(screen.queryByText('Session 0')).not.toBeInTheDocument();
  });

  it('shows what grew after the baseline and draws the trends', () => {
    vi.useFakeTimers();
    let current = report();
    render(<PerfOverlay startOpen read={() => current} refreshMs={1_000} />);

    expect(screen.getByText(/Set a baseline/)).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Set baseline' }));
    expect(screen.getByRole('heading', { name: 'Listeners added since the baseline' })).toBeInTheDocument();
    expect(screen.getByText('None')).toBeInTheDocument();

    const base = report();
    current = report({
      heap: { used: 50 * MB, total: 64 * MB, limit: 4096 * MB },
      domNodes: 1500,
      listeners: {
        ...base.listeners,
        live: 14,
        groups: [base.listeners.groups[0], { ...base.listeners.groups[1], live: 8, added: 13 }],
      },
    });
    act(() => {
      vi.advanceTimersByTime(1_000);
    });

    expect(screen.getByText('+4', { selector: 'li span' }).closest('li')).toHaveTextContent(
      '+4 AbortSignal · abort · fetchJson @ src/lib/gateway.ts:40',
    );
    expect(figure('JS heap')).toHaveTextContent('50.0 MB+2.0 MB');
    expect(figure('Elements')).toHaveTextContent(`${(1500).toLocaleString()}+300`);
    expect(figure('Listeners')).toHaveTextContent('14+4');
    expect(screen.queryByRole('img', { name: 'Listener trend' })).not.toBeInTheDocument();

    act(() => {
      vi.advanceTimersByTime(1_000);
    });

    expect(screen.getByRole('img', { name: 'JS heap trend' })).toBeInTheDocument();
    expect(screen.getByRole('img', { name: 'Listener trend' })).toBeInTheDocument();
  });

  it('marks listeners and observed elements left on removed elements', () => {
    const base = report();
    render(
      <PerfOverlay
        startOpen
        read={() =>
          report({
            listeners: { ...base.listeners, detached: 3 },
            observers: [{ kind: 'ResizeObserver', observers: 3, targets: 5, detached: 2 }],
          })
        }
        refreshMs={60_000}
      />,
    );

    expect(figure('On removed elements')).toHaveTextContent('3');
    expect(figure('On removed elements')).toHaveClass('text-err');
    expect(figure('Observed elements')).toHaveTextContent('52 removed');
    expect(figure('Observed elements')).toHaveClass('text-err');
  });

  it('keeps its first reading when the refresh is off', () => {
    vi.useFakeTimers();
    const read = vi.fn(() => report());
    render(<PerfOverlay startOpen read={read} refreshMs={0} />);

    act(() => {
      vi.advanceTimersByTime(60_000);
    });

    expect(read).toHaveBeenCalledTimes(1);
    expect(screen.queryByRole('img', { name: 'Listener trend' })).not.toBeInTheDocument();
  });

  // User report (paraphrased: the memory overlay should be a small dot that shows the
  // figures only after a click): it starts closed, as one dot with the figures as its name.
  it('starts as a dot and opens on a click', () => {
    render(<PerfOverlay read={() => report()} refreshMs={60_000} />);

    expect(screen.queryByRole('region', { name: 'Memory overlay' })).not.toBeInTheDocument();
    const dot = screen.getByRole('button', { name: 'Memory 48.0 MB · 10 listeners' });
    expect(dot).toHaveAttribute('aria-expanded', 'false');
    expect(dot).toHaveAttribute('title', 'Memory 48.0 MB · 10 listeners');
    expect(dot).toHaveTextContent('');
    fireEvent.click(dot);
    expect(screen.getByRole('region', { name: 'Memory overlay' })).toBeInTheDocument();
  });

  // User request: the closed dot sits next to the VIS wordmark in the app bar.
  it('puts the dot just after the wordmark in the app bar', () => {
    const mark = document.createElement('div');
    mark.setAttribute('data-wordmark', '');
    mark.getBoundingClientRect = () => ({ left: 12, right: 96, top: 0, bottom: 48, width: 84, height: 48, x: 12, y: 0, toJSON: () => ({}) });
    document.body.append(mark);
    try {
      render(<PerfOverlay read={() => report()} refreshMs={60_000} />);

      const dot = screen.getByRole('button', { name: 'Memory 48.0 MB · 10 listeners' });
      const box = dot.closest('section')!;
      expect(box).toHaveClass('fixed');
      expect(box.style.left).toBe('96px');
      expect(box.style.top).toBe('24px');
      fireEvent.click(dot);
      expect(screen.getByRole('region', { name: 'Memory overlay' }).style.left).toBe('');
    } finally {
      mark.remove();
    }
  });

  it('keeps the dot at the right edge while the wordmark is away', () => {
    render(<PerfOverlay read={() => report()} refreshMs={60_000} />);

    const box = screen.getByRole('button', { name: 'Memory 48.0 MB · 10 listeners' }).closest('section')!;
    expect(box).not.toHaveClass('fixed');
    expect(box.style.left).toBe('');
  });

  it('minimizes to the dot and opens again', () => {
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} />);

    const toggle = screen.getByRole('button', { name: 'Minimize memory overlay' });
    expect(toggle).toHaveAttribute('aria-expanded', 'true');
    fireEvent.click(toggle);

    expect(screen.queryByRole('region', { name: 'Memory overlay' })).not.toBeInTheDocument();
    const summary = screen.getByRole('button', { name: 'Memory 48.0 MB · 10 listeners' });
    // Keep the control's gesture state so the click after a pointer release is ignored.
    expect(summary).toBe(toggle);
    expect(summary).toHaveAttribute('aria-expanded', 'false');
    fireEvent.click(summary);
    expect(toggle).toHaveAttribute('aria-expanded', 'true');
    expect(screen.getByRole('region', { name: 'Memory overlay' })).toBeInTheDocument();
  });

  it('says when the browser does not report the heap', () => {
    render(<PerfOverlay startOpen read={() => report({ heap: null })} refreshMs={60_000} />);

    expect(figure('JS heap')).toHaveTextContent('Not reported');
    fireEvent.click(screen.getByRole('button', { name: 'Minimize memory overlay' }));
    expect(screen.getByRole('button', { name: 'Memory 10 listeners' })).toBeInTheDocument();
  });

  it('copies the full report as JSON', async () => {
    const writeText = vi.fn().mockResolvedValue(undefined);
    vi.stubGlobal('navigator', { ...navigator, clipboard: { writeText } });
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} />);

    fireEvent.click(screen.getByRole('button', { name: 'Copy report' }));

    expect(writeText).toHaveBeenCalledWith(JSON.stringify(report(), null, 2));
    expect(await screen.findByRole('button', { name: 'Copied' })).toBeInTheDocument();
  });

  it('turns itself off from the open overlay', () => {
    const turnOff = vi.fn(() => true);
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} turnOff={turnOff} />);

    fireEvent.click(screen.getByRole('button', { name: 'Turn off' }));

    expect(turnOff).toHaveBeenCalledTimes(1);
    expect(screen.queryByText('This device did not save the setting.')).not.toBeInTheDocument();
  });

  it('says when this device did not save turning it off', () => {
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} turnOff={() => false} />);

    fireEvent.click(screen.getByRole('button', { name: 'Turn off' }));

    expect(screen.getByText('This device did not save the setting.')).toBeInTheDocument();
  });

  it('has no Turn off control in the perf build', () => {
    render(<PerfOverlay startOpen read={() => report()} refreshMs={60_000} turnOff={null} />);

    expect(screen.queryByRole('button', { name: 'Turn off' })).not.toBeInTheDocument();
  });

  // Regression, user report (paraphrased: the minimized memory badge covered the
  // preferences cog, so the overlay could not be turned off): the host pinned the badge
  // to the top-right corner, over the app bar's controls.
  it('mounts clear of the app bar, at the middle of the right edge', async () => {
    await act(async () => {
      mountPerfOverlay();
    });
    const host = document.getElementById('vis-perf');
    try {
      expect(host).not.toBeNull();
      const classes = host!.className.split(' ');
      expect(classes).toEqual(expect.arrayContaining(['fixed', 'inset-0', 'items-center', 'justify-end']));
      expect(host!.className).not.toMatch(/(^| )(top|right)-/);
      expect(within(host!).getByRole('button', { name: /^Memory .* listeners$/ })).toHaveAttribute('aria-expanded', 'false');
    } finally {
      host?.remove();
    }
  });
});
