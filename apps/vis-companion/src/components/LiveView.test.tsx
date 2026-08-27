// @vitest-environment jsdom
// The panel paints a picture that is being written while it is read, so every
// case here renders the ENGINE's own fixture — the same file
// `gateway/human_input_test.clj` asserts is the engine's projection of a view —
// and reads the document that landed.
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { activityReceiptText, LiveView as LiveViewList, LiveViewPanel, useLiveViews } from './LiveView';
import liveViewSource from './LiveView.tsx?raw';
import fixture from '../lib/live-view.fixture.json';
import activityFixture from '../lib/activity.fixture.json';
import type { GatewayClient } from '../lib/gateway';
import {
  LIVE_NOTE_CHARS,
  LIVE_VIEW_CLOSE_EVENT,
  LIVE_VIEW_PATCH_EVENT,
  liveViewFromWire,
  type LiveNode,
  type LiveView,
} from '../lib/live-view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';

afterEach(cleanup);

function opened(): LiveView {
  const view = liveViewFromWire(fixture);
  if (!view) throw new Error('the engine fixture must be paintable');
  return view;
}

function activityView(): LiveView {
  const view = liveViewFromWire(activityFixture);
  if (!view) throw new Error('the engine Activity fixture must be paintable');
  return view;
}

/** The same view with ONE node replaced, wherever a row put it — a run in a different moment. */
function withNode(view: LiveView, node: LiveNode): LiveView {
  const swap = (nodes: LiveNode[]): LiveNode[] =>
    nodes.map((existing) =>
      existing.id === node.id
        ? node
        : existing.type === 'group'
          ? { ...existing, fields: swap(existing.fields) }
          : existing,
    );
  return { ...view, nodes: swap(view.nodes) };
}

function paint(props: Partial<Parameters<typeof LiveViewPanel>[0]> = {}) {
  const { view = opened(), ...rest } = props;
  render(<LiveViewPanel view={view} {...rest} />);
  return document.body.innerHTML;
}

describe('a live view on the phone', () => {
  it('paints every node the run declared, in the order it declared them', () => {
    const html = paint();
    expect(html).toContain('Fleet scan');
    expect(html).toContain('3 hosts · started 12:04');
    const labels = ['Swept', 'Findings', 'Phases', 'Output', 'Hosts', 'Why', 'Elsewhere'];
    expect(labels.map((label) => html.indexOf(label))).toEqual(
      [...labels.map((label) => html.indexOf(label))].sort((a, b) => a - b),
    );
    expect(labels.every((label) => html.includes(label))).toBe(true);

    expect(html).toContain('Scanning db-2');
    expect(html).toContain('host 2 of 3');
    expect(html).toContain('Critical');
    expect(html).toContain('Collect inventory');
    expect(html).toContain('db-2 · 1 critical (openssl)');
    expect(screen.getAllByRole('row').length).toBe(3);
    expect(screen.getByRole('columnheader', { name: 'Findings' }).className).toContain('text-right');
    expect(screen.getByRole('link', { name: 'The run on GitHub' })).toHaveProperty(
      'href',
      'https://example.com/run/42',
    );
  });

  // A section that only paints cannot be heard. The run reports itself, so the
  // panel is a live region: a screen reader is told when the picture moves.
  it('reports itself as work in progress, not as a question', () => {
    paint();
    const panel = screen.getByRole('status');
    expect(panel.getAttribute('aria-live')).toBe('polite');
    expect(screen.queryByRole('dialog')).toBeNull();
  });

  it('draws the fraction as a bar and states the count beside it', () => {
    paint();
    expect(screen.getByRole('progressbar').getAttribute('aria-valuenow')).toBe('67');
    expect(document.body.innerHTML).toContain('2/3');
  });

  // A bar that never moves reads as a stall. A run whose size is not known yet
  // says so in words instead.
  it('draws no bar for a run with no knowable end', () => {
    const html = paint({ view: withNode(opened(), { id: 'swept', type: 'progress', label: 'Swept' }) });
    expect(screen.queryByRole('progressbar')).toBeNull();
    expect(html).toContain('working');
  });

  it("says what an empty node is waiting for, in the engine's own words", () => {
    const html = paint({
      view: withNode(opened(), {
        id: 'hosts',
        type: 'table',
        label: 'Hosts',
        columns: [{ id: 'host', label: 'Host', align: 'left' }],
        rows: [],
        max_rows: 5000,
        order: 'insertion',
        is_focusable: false,
        focused_ids: [],
      }),
    });
    expect(html).toContain('no rows yet');
    expect(screen.getByRole('columnheader', { name: 'Host' })).toBeTruthy();
  });

  // Caps are for the NAME. A live node names what it is a label OF — the failing
  // job, the runner the timeline belongs to — and a whole line of caps shouts the
  // part that has to be read.
  it('sets the node name in caps and leaves its qualifier in ordinary type', () => {
    paint({
      view: withNode(opened(), {
        id: 'tail',
        type: 'log',
        label: 'Failure · Run native build',
        lines: ['exit 1'],
        window_lines: 2000,
        total_lines: 1,
      }),
    });
    const caps = [...document.querySelectorAll('span.uppercase')].filter((one) =>
      one.textContent?.startsWith('Failure'),
    );
    expect(caps.map((one) => one.textContent)).toEqual(['Failure']);
    expect(caps[0]?.parentElement?.textContent).toBe('Failure · Run native build');
  });
  // A path or an attachment names a place on the MACHINE: dressing it as a link
  // would promise a tap that does nothing under the thumb.
  it('opens what the phone can reach and states what it cannot', () => {
    paint();
    expect(screen.queryByRole('link', { name: 'report.md' })).toBeNull();
    expect(document.body.innerHTML).toContain('/tmp/report.md');
  });
  it('uses the first in-progress task as the collapsed Activity preview', () => {
    const onInterrupt = vi.fn();
    paint({ view: activityView(), onInterrupt });

    expect(screen.getByText('ACTIVITY')).toBeTruthy();
    expect(screen.getByText('Running')).toBeTruthy();
    expect(screen.getByText('RUN_TESTS · suite')).toBeTruthy();
    expect(screen.queryByText(/2 operations|24 passed|truncated/)).toBeNull();
    expect(screen.queryByRole('button', { name: /interrupt/i })).toBeNull();
    expect(screen.queryByRole('list', { name: 'Invocation chronology' })).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    const chronology = screen.getByRole('list', { name: 'Invocation chronology' });
    const chronologyText = chronology.textContent ?? '';
    expect(chronologyText.indexOf('grep · 18 matches')).toBeLessThan(
      chronologyText.indexOf('run_tests · suite'),
    );
    expect(chronologyText).not.toContain('[{query: needle}]');
    expect(chronologyText).not.toContain('24 passed');
    expect(screen.getByRole('button', { name: 'Collapse Activity' })).toBeTruthy();
    expect(onInterrupt).not.toHaveBeenCalled();
  });

  it('matches the compact result-band height without a leading status mark', () => {
    paint({ view: activityView() });
    const receipt = screen.getByLabelText('Activity');
    const header = receipt.querySelector('header');
    const disclosure = screen.getByRole('button', { name: 'Expand Activity' });

    expect(receipt.classList.contains('border')).toBe(false);
    expect(receipt.className).toContain('border-l-2');
    expect(header?.classList.contains('min-h-8')).toBe(true);
    expect(header?.classList.contains('min-h-10')).toBe(false);
    expect(header?.querySelector('.animate-spinner-frame')).toBeNull();
    expect(header?.querySelector('.text-code-duration')).toBeNull();
    expect(disclosure.className).toContain('min-h-8');
    expect(disclosure.className).toContain('motion-reduce:transition-none');
  });

  // Regression, issue td-5b6b08: settled Companion receipts said SUCCEEDED,
  // omitted the operation and elapsed time, and retained "activities run".
  it('matches the settled TUI receipt grammar and durations', () => {
    const view = activityView();
    const settled = {
      ...view,
      created_at: 1_000,
      ended_at: 13_600,
      activity: {
        ...view.activity!,
        state: 'succeeded' as const,
        counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
        rows: view.activity!.rows.map((row, index) => ({
          ...row,
          state: 'succeeded' as const,
          ...(index === 0
            ? { operation: 'shell', summary: 'running: git status', duration_ms: 66 }
            : { duration_ms: 12_500 }),
        })),
      },
    };

    paint({ view: settled, isSettled: true });

    expect(activityReceiptText(settled.activity, 12_600)).toBe(
      'DONE · SHELL and more · 2 activities · 12.6s',
    );
    expect(screen.getByText('Done')).toBeTruthy();
    expect(screen.getByText('SHELL and more')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByText('shell · cmd: git status')).toBeTruthy();
    expect(screen.getByText('66ms')).toBeTruthy();
    expect(screen.getByText('12.5s')).toBeTruthy();
    expect(screen.getByLabelText('Activity').querySelector('header')?.textContent).not.toContain('✓');
    expect(screen.queryByRole('status')).toBeNull();
  });

  it('shows an explicit quiet empty state', () => {
    const view = activityView();
    paint({ view: { ...view, activity: { ...view.activity!, state: 'idle', rows: [] } } });
    expect(screen.getByText('Idle')).toBeTruthy();
    expect(screen.getByText('No operation yet')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Expand Activity' }));
    expect(screen.getByText('No operations yet')).toBeTruthy();
  });
});

describe('focusing a table row', () => {
  const focusableView = (): LiveView => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    return withNode(view, {
      ...hosts,
      is_focusable: true,
      focused_ids: ['db-1', 'db-2'],
    });
  };

  it('shows every default focus and sends a press anywhere in the job row', () => {
    const onFocus = vi.fn();
    paint({ view: focusableView(), onFocus });

    const first = screen.getByRole('button', { name: 'Focus db-1' });
    const second = screen.getByRole('button', { name: 'Focus db-2' });
    expect(first.getAttribute('aria-pressed')).toBe('true');
    expect(second.getAttribute('aria-pressed')).toBe('true');
    fireEvent.click(second);
    expect(onFocus).toHaveBeenCalledWith('hosts', ['db-2']);

    onFocus.mockClear();
    // A middle column appears twice in the DOM on purpose — stacked under the name
    // for a phone, and as its own cell from `sm` — and never both on one screen. The
    // press has to reach the row from the cell that is NOT the button.
    const spelled = screen.getAllByText('critical');
    fireEvent.click(spelled[spelled.length - 1]);
    expect(onFocus).toHaveBeenCalledWith('hosts', ['db-2']);
  });
  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: matrix jobs repeated
  // their whole parent name as flat peers, making the run hard to scan on a phone.
  it('nests matrix jobs under a collapsible parent', () => {
    const view = focusableView();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    hosts.rows = [
      { id: 'ios', cells: ['Release apps / iOS', 'queued'], tone: 'running', branch: 'Release apps' },
      { id: 'android', cells: ['Release apps / Android', 'running'], tone: 'running', branch: 'Release apps' },
      { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok' },
    ];
    hosts.focused_ids = ['android'];
    paint({ view, onFocus: vi.fn() });

    const parent = screen.getByRole('button', { name: 'Release apps' });
    expect(parent.getAttribute('aria-expanded')).toBe('true');
    expect(screen.getByRole('button', { name: 'Focus Release apps / iOS' })).toBeTruthy();
    expect(screen.getByText('Android')).toBeTruthy();

    fireEvent.click(parent);
    expect(parent.getAttribute('aria-expanded')).toBe('false');
    expect(screen.queryByRole('button', { name: 'Focus Release apps / iOS' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Focus Publish docs' })).toBeTruthy();
  });
  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: selecting a job only
  // tinted its first cell, so the table looked like a cell picker instead of a row picker.
  it('paints the selected state across the whole job row', () => {
    paint({ view: focusableView(), onFocus: vi.fn() });

    const selected = screen.getByRole('button', { name: 'Focus db-2' });
    const row = selected.closest('tr') as HTMLTableRowElement;
    expect(row.className).toContain('bg-accent/10');
    expect(row.getAttribute('aria-selected')).toBe('true');
  });

  it('sends the row through the gateway rather than keeping selection in the component', () => {
    const view = focusableView();
    const focusLiveView = vi.fn(async () => ({
      focused_ids: ['db-2'],
      node_id: 'hosts',
      view_id: view.id,
    }));
    const client = { focusLiveView } as unknown as GatewayClient;

    render(<LiveViewList views={[view]} client={client} sid="session-1" />);
    fireEvent.click(screen.getByRole('button', { name: 'Focus db-2' }));

    expect(focusLiveView).toHaveBeenCalledWith('session-1', view.id, 'hosts', ['db-2']);
  });
});

describe('a log the operator walks back through', () => {
  const behind = (): LiveView =>
    withNode(opened(), {
      id: 'tail',
      type: 'log',
      label: 'Output',
      lines: ['db-2 · 1 critical (openssl)'],
      window_lines: 2000,
      total_lines: 500,
    });

  it('offers the earlier lines only when the record still holds some', () => {
    const html = paint({ view: behind(), load: vi.fn() });
    // What a screen reader hears is the PROMISE; what the eye reads is how much
    // of the run is still behind the window.
    expect(screen.getByRole('button', { name: 'Load 200 earlier lines' })).toBeTruthy();
    expect(html).toContain('499 earlier lines');

    cleanup();
    paint({ view: opened(), load: vi.fn() });
    expect(screen.queryByRole('button', { name: /earlier lines/ })).toBeNull();
  });

  it('reads one page out of the record and keeps it above the window', async () => {
    const load = vi.fn().mockResolvedValue({
      node_id: 'tail',
      from: 299,
      lines: ['db-0 · 0 critical'],
      total: 500,
    });
    paint({ view: behind(), load });

    fireEvent.click(screen.getByRole('button', { name: 'Load 200 earlier lines' }));
    await waitFor(() => expect(document.body.innerHTML).toContain('db-0 · 0 critical'));
    expect(load).toHaveBeenCalledWith('tail', 299, 200);

    const html = document.body.innerHTML;
    expect(html.indexOf('db-0 · 0 critical')).toBeLessThan(html.indexOf('db-2 · 1 critical'));
    // 299 read, 200 fetched, 499 in the record before the window: the lines
    // between what was read and what is on screen are NAMED, never skipped over.
    expect(html).toContain('lines scrolled past while you were reading');
  });
});

describe('stopping the run from the phone', () => {
  it('arms the stop, takes the comment, and sends it with the interrupt', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    // Pressing Interrupt STOPS NOTHING yet: it opens the line the reason goes on.
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    expect(onInterrupt).not.toHaveBeenCalled();

    const field = screen.getByRole('textbox', { name: 'Why are you stopping Fleet scan?' });
    expect(field).toHaveProperty('maxLength', LIVE_NOTE_CHARS);
    fireEvent.change(field, { target: { value: '  wrong subnet  ' } });
    fireEvent.submit(field.closest('form') as HTMLFormElement);
    expect(onInterrupt).toHaveBeenCalledWith('wrong subnet');
  });

  it('stops with no comment at all — a stop is never held up by one', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.submit(
      (screen.getByRole('textbox') as HTMLElement).closest('form') as HTMLFormElement,
    );
    expect(onInterrupt).toHaveBeenCalledWith(null);
  });

  it('keeps watching when the human backs out, and forgets what they typed', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.change(screen.getByRole('textbox'), { target: { value: 'never mind' } });
    fireEvent.click(screen.getByRole('button', { name: 'Keep watching' }));
    expect(onInterrupt).not.toHaveBeenCalled();
    expect(screen.queryByRole('textbox')).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    expect(screen.getByRole('textbox')).toHaveProperty('value', '');
  });

  // The key that ARMED the stop is the key that sends it, note and all — the
  // terminal answers Escape the same way, and a comment nobody typed is no
  // comment rather than an empty one.
  it('sends the stop when the human presses Escape over the note', () => {
    const onInterrupt = vi.fn();
    paint({ onInterrupt });
    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.change(screen.getByRole('textbox'), { target: { value: 'wrong subnet' } });
    fireEvent.keyDown(screen.getByRole('textbox'), { key: 'Escape' });
    expect(onInterrupt).toHaveBeenCalledWith('wrong subnet');
    expect(screen.queryByRole('textbox')).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Interrupt' }));
    fireEvent.keyDown(screen.getByRole('textbox'), { key: 'Escape' });
    expect(onInterrupt).toHaveBeenLastCalledWith(null);
  });

  it('says it is working on it, and offers every view a stop', () => {
    cleanup();
    paint({ onInterrupt: vi.fn(), isInterrupting: true });
    const button = screen.getByRole('button', { name: 'Stopping...' });
    expect(button).toHaveProperty('disabled', true);
  });

  it('states a refusal where the press happened', () => {
    const html = paint({ onInterrupt: vi.fn(), error: 'That view would not stop.' });
    expect(html).toContain('That view would not stop.');
    expect(html).toContain('text-err');
  });
});

describe('what a run says about its own layout', () => {
  // A view lays itself out with the FORM's own group: the row is the run's
  // statement, not the screen's guess, and the terminal splits its band on it.
  it('stands the nodes a row holds side by side', () => {
    paint();
    const list = screen.getByRole('status').querySelector('ul') as HTMLElement;
    // Seven top-level nodes, seven rows: `hosts` and `why` share the one the
    // group holds them in instead of taking one each.
    expect(list.children.length).toBe(7);
    const beside = [...list.children].find((row) => row.textContent?.includes('Hosts')) as HTMLElement;
    expect(beside.textContent).toContain('Why');
    expect(beside.innerHTML).toContain('sm:grid-flow-col');
    // …and the sentence beside a table uses the app's naturally spaced prose rule.
    expect(beside.innerHTML).toContain('text-left');
    const alone = [...list.children].find((row) => row.textContent?.includes('Elsewhere')) as HTMLElement;
    expect(alone.innerHTML).not.toContain('grid-flow-col');
  });

  // A phone reads a table DOWN its first column. The row's own line carries the
  // name and, at the right edge, the value it is measured by; every column between
  // them stacks under the name, and they take their cells back only when there is
  // width for them. Forty fenced cells at 8px were a grid to decode before a run
  // could be read — a rule between rows is all the eye needs to keep a row whole.
  it('reads down a phone: no fenced cells, a rule between rows, columns only at sm', () => {
    paint();
    const table = screen.getByRole('table');
    expect(table.className).not.toContain('border border-dialog-edge');
    expect(table.querySelector('tbody')?.className).toContain('divide-y');
    const head = table.querySelector('thead') as HTMLElement;
    expect(head.className).toContain('hidden');
    expect(head.className).toContain('sm:table-header-group');
    const cells = [...table.querySelectorAll('td')];
    expect(cells.some((cell) => cell.className.includes('border border-dialog-edge'))).toBe(false);
    const [name, ...rest] = [...(table.querySelector('tbody tr') as HTMLElement).children];
    expect(name.className).not.toContain('hidden');
    expect(
      rest.every(
        (cell) => cell.className.includes('hidden') && cell.className.includes('sm:table-cell'),
      ),
    ).toBe(true);
  });

  it('keeps the name and its value on one line and stacks the rest beneath', () => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    paint({
      view: withNode(view, {
        ...hosts,
        columns: [
          { id: 'job', label: 'Job', align: 'left' },
          { id: 'now', label: 'Now', align: 'left' },
          { id: 'took', label: 'Took', align: 'right' },
        ],
        rows: [{ ...hosts.rows[0], cells: ['macos-latest', 'Run native build', '44m46s'] }],
      }),
    });
    const face = screen.getAllByRole('cell')[0] as HTMLElement;
    expect(face.textContent).toContain('macos-latest');
    expect(face.textContent).toContain('44m46s');
    expect(face.textContent).toContain('Run native build');
  });

  it('paints the marks a human wrote, and nothing a block would bring', () => {
    const panel = (paint(), screen.getByRole('status'));
    expect([...panel.querySelectorAll('code')].map((mark) => mark.textContent)).toContain('db-2');
    expect([...panel.querySelectorAll('strong')].map((mark) => mark.textContent)).toContain(
      'openssl 3.0.13',
    );
    // A row stays a row: no heading, no quote, no list arrived with the marks.
    expect(panel.querySelector('h1, h2, h3, blockquote')).toBeNull();
  });

  it('marks a table cell too, because a cell is a human string as well', () => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.fields : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    paint({
      view: withNode(view, { ...hosts, rows: [{ ...hosts.rows[0], cells: ['`db-1`', 'clean', '0'] }] }),
    });
    expect(screen.getAllByRole('cell')[0].querySelector('code')?.textContent).toBe('db-1');
  });

  // Regression, reported from the phone: the headline shared one flex line with
  // its detail, so "1 of 2 jobs finished" beside a workflow and a job name was
  // squeezed to one word per line — a column of five words down the panel.
  it('gives the headline the whole width and puts its detail on the line below', () => {
    paint();
    const headline = screen.getByText('Scanning db-2').closest('p') as HTMLElement;
    const detail = screen.getByText('host 2 of 3').closest('p') as HTMLElement;
    // Two elements, not two halves of one line.
    expect(headline).not.toBe(detail);
    expect(headline.contains(detail)).toBe(false);
    // The same grid, the detail on its own row under the headline's column.
    expect(detail.parentElement).toBe(headline.parentElement);
    expect(detail.className).toContain('col-start-2');
    // Nothing may take width from the headline any more.
    expect(headline.className).not.toContain('flex-1');
    expect(headline.parentElement?.className).not.toContain('flex');
  });

  // Regression, user report: Activity patches rendered one-by-one on WKWebView and
  // starved the independent elapsed-time paint, leaving its clock visibly frozen.
  it('coalesces an Activity patch burst into one phone paint', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = activityView();
    const client = { liveViews: () => Promise.resolve([running]) } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;
    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views[0]?.seq ?? 'none'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('0')).toBeTruthy());
    vi.useFakeTimers();
    try {
      for (const seq of [1, 2, 3]) {
        act(() =>
          receive?.({
            type: LIVE_VIEW_PATCH_EVENT,
            view_id: running.id,
            first_seq: seq,
            patch: { view_id: running.id, seq, ops: [] },
          }),
        );
      }

      expect(screen.getByText('0')).toBeTruthy();
      act(() => vi.advanceTimersByTime(100));
      expect(screen.getByText('3')).toBeTruthy();
    } finally {
      vi.useRealTimers();
    }
  });

  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: interrupt
  // removed the live panel, then its one transcript read raced the running block
  // persisting the record, so no artifact row appeared.
  it('re-reads through the handoff from a closed view to its transcript artifact', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const client = { liveViews: () => Promise.resolve([opened()]) } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;
    const onRecordFiled = vi.fn();

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1', onRecordFiled);
      return <span>{views.length}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('1')).toBeTruthy());
    vi.useFakeTimers();
    try {
      act(() =>
        receive?.({
          type: LIVE_VIEW_CLOSE_EVENT,
          view_id: opened().id,
          result: { artifact_id: 'record-1' },
        }),
      );

      expect(onRecordFiled).toHaveBeenCalledTimes(1);
      expect(screen.getByText('0')).toBeTruthy();
      act(() => vi.advanceTimersByTime(8_000));
      expect(onRecordFiled).toHaveBeenCalledTimes(5);
    } finally {
      vi.useRealTimers();
    }
  });

  it('keeps the terminal Activity picture settled while its record is filed', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = activityView();
    const client = { liveViews: () => Promise.resolve([running]) } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      const view = views[0];
      return (
        <span>
          {view?.activity?.state}:{view?.activity?.counts.running}:
          {view?.is_settled ? 'settled' : 'live'}
        </span>
      );
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('running:1:live')).toBeTruthy());
    act(() =>
      receive?.({
        type: LIVE_VIEW_CLOSE_EVENT,
        view_id: running.id,
        ts: 99,
        result: {
          view: {
            title: running.title,
            nodes: running.nodes,
            activity: {
              ...running.activity!,
              state: 'succeeded',
              counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
              rows: running.activity!.rows.map((row) => ({ ...row, state: 'succeeded' })),
            },
          },
        },
      }),
    );
    expect(screen.getByText('succeeded:0:settled')).toBeTruthy();
  });

  it('repairs an Activity sequence gap from the materialized snapshot', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = activityView();
    const repaired = { ...running, seq: 4 };
    const liveViews = vi
      .fn<() => Promise<LiveView[]>>()
      .mockResolvedValueOnce([running])
      .mockResolvedValue([repaired]);
    const client = { liveViews } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: () => () => undefined,
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views[0]?.seq ?? 'none'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('0')).toBeTruthy());
    act(() =>
      receive?.({
        type: LIVE_VIEW_PATCH_EVENT,
        view_id: running.id,
        first_seq: 3,
        patch: { view_id: running.id, seq: 4, ops: [] },
      }),
    );
    await waitFor(() => expect(screen.getByText('4')).toBeTruthy());
    expect(liveViews).toHaveBeenCalledTimes(2);
  });

  it('replaces an Activity with its reconnect snapshot', async () => {
    let reconnect: ((connected: boolean) => void) | null = null;
    const running = activityView();
    const refreshed = { ...running, seq: 2 };
    const liveViews = vi
      .fn<() => Promise<LiveView[]>>()
      .mockResolvedValueOnce([running])
      .mockResolvedValue([refreshed]);
    const client = { liveViews } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: (listener: (connected: boolean) => void) => {
        reconnect = listener;
        return () => undefined;
      },
      subscribeSession: () => () => undefined,
    } as unknown as SessionSubscriptionHub;

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views[0]?.seq ?? 'none'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('0')).toBeTruthy());
    act(() => reconnect?.(true));
    await waitFor(() => expect(screen.getByText('2')).toBeTruthy());
    expect(liveViews).toHaveBeenCalledTimes(2);
  });

  it('does not let an older reconnect snapshot overwrite an Activity close', async () => {
    let reconnect: ((connected: boolean) => void) | null = null;
    let receive: ((event: SseEvent) => void) | null = null;
    let resolveReload: ((views: LiveView[]) => void) | null = null;
    const running = activityView();
    const liveViews = vi
      .fn<() => Promise<LiveView[]>>()
      .mockResolvedValueOnce([running])
      .mockImplementationOnce(
        () => new Promise<LiveView[]>((resolve) => (resolveReload = resolve)),
      );
    const client = { liveViews } as unknown as GatewayClient;
    const subscriptions = {
      subscribeConnection: (listener: (connected: boolean) => void) => {
        reconnect = listener;
        return () => undefined;
      },
      subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
        receive = listener;
        return () => undefined;
      },
    } as unknown as SessionSubscriptionHub;

    function Probe() {
      const views = useLiveViews(client, subscriptions, 'session-1');
      const view = views[0];
      return <span>{view?.is_settled ? view.activity?.state : 'live'}</span>;
    }

    render(<Probe />);
    await waitFor(() => expect(screen.getByText('live')).toBeTruthy());
    act(() => reconnect?.(true));
    act(() =>
      receive?.({
        type: LIVE_VIEW_CLOSE_EVENT,
        view_id: running.id,
        result: {
          view: {
            title: running.title,
            nodes: running.nodes,
            activity: {
              ...running.activity!,
              state: 'succeeded',
              counts: { running: 0, succeeded: 2, failed: 0, cancelled: 0 },
              rows: running.activity!.rows.map((row) => ({ ...row, state: 'succeeded' })),
            },
          },
        },
      }),
    );
    expect(screen.getByText('succeeded')).toBeTruthy();
    await act(async () => resolveReload?.([running]));
    expect(screen.getByText('succeeded')).toBeTruthy();
  });
});

describe('the section is built from the closed vocabulary', () => {
  it("borrows the app's controls and writes no styles of its own", () => {
    expect(liveViewSource).toContain('<Button');
    expect(liveViewSource).toContain('<Meter');
    expect(liveViewSource).toContain('<LoadMore');
    expect(liveViewSource).toContain('<Disclosure');
    // And no spinner: a mark that turns for ninety minutes says only "still here",
    // while one word says whether the run is live and, once it is not, how it ended.
    expect(liveViewSource).not.toContain('<Spinner');
    expect(liveViewSource).not.toContain('<button');
    expect(liveViewSource).not.toContain('style={');
    expect(liveViewSource).not.toContain('style="');
  });
});
