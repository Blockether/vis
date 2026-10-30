// @vitest-environment jsdom
// The panel paints a picture that is being written while it is read, so every
// case here renders the ENGINE's own fixture — the same file
// `gateway/human_input_test.clj` asserts is the engine's projection of a view —
// and reads the document that landed.
import { act, cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { LiveView as LiveViewList, LiveViewPanel, useLiveViews } from './LiveView';
import fixture from '../lib/live-view.fixture.json';
import type { GatewayClient } from '../lib/gateway';
import {
  applyLivePatch,
  LIVE_NOTE_CHARS,
  liveViewFromWire,
  type LiveNode,
  type LiveView,
} from '../lib/live-view';
import type { SessionSubscriptionHub } from '../lib/subscriptions';
import type { SseEvent } from '../lib/types';
import { VIEW_CLOSE_EVENT, VIEW_PATCH_EVENT } from '../lib/view';

afterEach(cleanup);

function opened(): LiveView {
  const view = liveViewFromWire(fixture);
  if (!view) throw new Error('the engine fixture must be paintable');
  return view;
}

/** The same view with ONE node replaced, wherever a row put it — a run in a different moment. */
function withNode(view: LiveView, node: LiveNode): LiveView {
  const swap = (nodes: LiveNode[]): LiveNode[] =>
    nodes.map((existing) =>
      existing.id === node.id
        ? node
        : existing.type === 'group'
          ? { ...existing, children: swap(existing.children) }
          : existing,
    );
  return { ...view, nodes: swap(view.nodes) };
}

function paint(props: Partial<Parameters<typeof LiveViewPanel>[0]> = {}) {
  const { view = opened(), ...rest } = props;
  render(<LiveViewPanel view={view} {...rest} />);
  return document.body.innerHTML;
}

describe('live log disclosures', () => {
  // Regression #189: a scrollable log is not collapsed output.
  it('starts each build log collapsed without deleting its retained output', () => {
    const view: LiveView = {
      ...opened(),
      nodes: [
        {
          id: 'a',
          type: 'log',
          label: 'Build A logs',
          lines: ['A retained'],
          window_lines: 100,
          total_lines: 1,
        },
        {
          id: 'b',
          type: 'log',
          label: 'Build B logs',
          lines: ['B retained'],
          window_lines: 100,
          total_lines: 1,
        },
      ],
    };
    paint({ view });
    expect(screen.queryByText('A retained')).toBeNull();
    expect(screen.queryByText('B retained')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Build A logs' }));
    expect(screen.getByText('A retained')).toBeVisible();
    expect(screen.queryByText('B retained')).toBeNull();
    expect(view.nodes[0]).toHaveProperty('lines', ['A retained']);
  });
});

describe('a live view on the phone', () => {
  it('draws the fraction as a bar and states the count beside it', () => {
    paint();
    expect(screen.getByRole('progressbar').getAttribute('aria-valuenow')).toBe('67');
  });

  // A path or an attachment names a place on the MACHINE: dressing it as a link
  // would promise a tap that does nothing under the thumb.
  it('opens what the phone can reach and states what it cannot', () => {
    paint();
    expect(screen.queryByRole('link', { name: 'report.md' })).toBeNull();
    expect(document.body.innerHTML).toContain('/tmp/report.md');
  });
});

describe('selecting a table row', () => {
  const selectableView = (): LiveView => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.children : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    return withNode(view, {
      ...hosts,
      is_selectable: true,
      selected_ids: ['db-1', 'db-2'],
    });
  };

  it('shows every default selection and sends a press anywhere in the job row', () => {
    const onSelect = vi.fn();
    paint({ view: selectableView(), onSelect });

    const first = screen.getByRole('button', { name: 'Select db-1' });
    const second = screen.getByRole('button', { name: 'Select db-2' });
    expect(first.getAttribute('aria-pressed')).toBe('true');
    expect(second.getAttribute('aria-pressed')).toBe('true');
    fireEvent.click(second);
    expect(onSelect).toHaveBeenCalledWith('hosts', ['db-2']);

    onSelect.mockClear();
    // A middle column appears twice in the DOM on purpose — stacked under the name
    // for a phone, and as its own cell from `sm` — and never both on one screen. The
    // press has to reach the row from the cell that is NOT the button.
    const spelled = screen.getAllByText('critical');
    fireEvent.click(spelled[spelled.length - 1]);
    expect(onSelect).toHaveBeenCalledWith('hosts', ['db-2']);
  });
  // Issue #430: a selected row showed its state only as a tint, unlike the terminal.
  it('marks each selectable row with ● or ○, not with colour alone', () => {
    const view = selectableView();
    const hosts = hostsTable(view);
    hosts.selected_ids = ['db-1'];
    paint({ view, onSelect: vi.fn() });

    const mark = (name: string) =>
      screen.getByRole('button', { name }).querySelector('[data-selection-mark]')?.textContent;
    expect(mark('Select db-1')).toBe('●');
    expect(mark('Select db-2')).toBe('○');
  });
  /** The hosts table of a view, which sits one level down when the view groups it. */
  const hostsTable = (view: LiveView) => {
    const node = view.nodes
      .flatMap((one) => (one.type === 'group' ? one.children : [one]))
      .find((one) => one.id === 'hosts');
    if (!node || node.type !== 'table') throw new Error('the fixture must hold the hosts table');
    return node;
  };

  /** One matrix parent with two variants, beside a job that stands alone. */
  const matrixView = (selectedIds: string[]): LiveView => {
    const view = selectableView();
    const hosts = hostsTable(view);
    hosts.rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'Release apps' },
      { id: 'android', cells: ['Android', 'running'], tone: 'running', parent: 'Release apps' },
      { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok' },
    ];
    hosts.selected_ids = selectedIds;
    return view;
  };

  // Regression, session a64d44c2-8228-455f-926e-b3381f19a93b: matrix jobs repeated
  // their whole parent name as flat peers, making the run hard to scan on a phone.
  it('nests matrix jobs under a parent that starts collapsed', () => {
    paint({ view: matrixView(['android']), onSelect: vi.fn() });

    const parent = screen.getByRole('button', { name: 'Release apps' });
    expect(parent.getAttribute('aria-expanded')).toBe('false');
    // The head counts its own legs; the producer names the parent and nothing else.
    expect(screen.getByText('2 rows')).toBeVisible();
    expect(screen.queryByRole('button', { name: 'Select iOS' })).toBeNull();
    expect(screen.getByRole('button', { name: 'Select Publish docs' })).toBeVisible();

    fireEvent.click(parent);
    expect(parent.getAttribute('aria-expanded')).toBe('true');
    expect(screen.getByRole('button', { name: 'Select iOS' })).toBeVisible();
    expect(screen.getByText('Android')).toBeVisible();
  });
  // Regression, reported with desktop screenshots: the browser sized the columns from the
  // rows on screen, so folding a group moved every column. A settled job moved them again.
  it('keeps every column width when a group folds and when a cell gets shorter', () => {
    const slow = 'waiting for a runner';
    const view = (now: string): LiveView => {
      const next = matrixView([]);
      hostsTable(next).rows = [
        { id: 'ios', cells: ['iOS', now], tone: 'running', parent: 'Release apps' },
        { id: 'android', cells: ['Android', 'running'], tone: 'running', parent: 'Release apps' },
        { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok' },
        { id: 'long', cells: ['Notes', 'ok', 'x'.repeat(80)], tone: 'ok', parent: 'Lint' },
      ];
      return next;
    };
    const widths = () =>
      [...screen.getByRole('table').querySelectorAll('col')].map((col) => col.className);
    const { rerender } = render(<LiveViewPanel view={view(slow)} onSelect={vi.fn()} />);

    // The name column takes the rest, and each other column has a width.
    const folded = widths();
    expect(folded[0]).toBe('');
    expect(folded).toHaveLength(hostsTable(view(slow)).columns.length);
    // A folded leg still counts, and one long cell stops at the cap.
    expect(folded[1]).toBe(`sm:w-[calc(${slow.length}ch+2*var(--live-view-inset))]`);
    expect(folded[2]).toBe('sm:w-[calc(28ch+2*var(--live-view-inset))]');
    fireEvent.click(screen.getByRole('button', { name: 'Lint' }));
    expect(widths()).toEqual(folded);

    rerender(<LiveViewPanel view={view('success')} onSelect={vi.fn()} />);
    expect(widths()).toEqual(folded);
  });
  // Regression, session c6473f43-3b3b-48f0-b309-64b7b37e8a21: every poll re-sent the
  // running jobs as the selection, which sprang open the parent the reader had closed.
  it('keeps a parent closed until the reader opens it, poll after poll', () => {
    const onSelect = vi.fn();
    const { rerender } = render(
      <LiveViewPanel view={matrixView(['android'])} onSelect={onSelect} />,
    );

    const parent = screen.getByRole('button', { name: 'Release apps' });
    fireEvent.click(parent);
    expect(parent.getAttribute('aria-expanded')).toBe('true');
    fireEvent.click(parent);

    rerender(<LiveViewPanel view={matrixView(['ios'])} onSelect={onSelect} />);
    expect(screen.getByRole('button', { name: 'Release apps' }).getAttribute('aria-expanded')).toBe(
      'false',
    );
    expect(screen.queryByRole('button', { name: 'Select iOS' })).toBeNull();
  });
  // Regression, session c6473f43-3b3b-48f0-b309-64b7b37e8a21: GitHub lists a matrix
  // interleaved with the rest of the run, so a parent head showed the first leg it met
  // and the remaining legs stood in the table as rows belonging to no parent.
  it('gathers a parent legs under its own head, however the run interleaves them', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'Release apps' },
      { id: 'docs', cells: ['Publish docs', 'success'], tone: 'ok' },
      { id: 'android', cells: ['Android', 'running'], tone: 'running', parent: 'Release apps' },
    ];
    paint({ view, onSelect: vi.fn() });

    expect(screen.queryByRole('button', { name: 'Select Android' })).toBeNull();

    fireEvent.click(screen.getByRole('button', { name: 'Release apps' }));
    expect(screen.getByRole('button', { name: 'Select iOS' })).toBeVisible();
    expect(screen.getByRole('button', { name: 'Select Android' })).toBeVisible();

    const order = screen.getAllByRole('row').map((row) => row.textContent ?? '');
    const at = (text: string) => order.findIndex((line) => line.includes(text));
    expect(at('Release apps')).toBeLessThan(at('iOS'));
    expect(at('iOS')).toBeLessThan(at('Android'));
    expect(at('Android')).toBeLessThan(at('Publish docs'));
  });
  // A DECLARED group is that same fold given an identity: the producer owns the order
  // the heads paint in, the label each head wears and the tone that says a leg failed
  // before anybody opens the fold.
  it('paints declared groups in the declared order, wearing the label and tone they declared', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [
      { id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'release' },
      { id: 'docs', cells: ['Publish docs', 'failed'], tone: 'error', parent: 'publish' },
    ];
    hosts.groups = [
      { id: 'release', label: 'Release apps', order: 1 },
      { id: 'publish', label: 'Publish', order: 0, tone: 'error' },
    ];
    paint({ view, onSelect: vi.fn() });

    const order = screen.getAllByRole('row').map((row) => row.textContent ?? '');
    const at = (text: string) => order.findIndex((line) => line.includes(text));
    expect(at('Publish')).toBeLessThan(at('Release apps'));
    // The head wears the LABEL; the id its rows point at never reaches the screen.
    expect(screen.queryByText('release')).toBeNull();
  });
  it('opens a group the producer declared open, and lets the reader shut it again', () => {
    const view = matrixView([]);
    const hosts = hostsTable(view);
    hosts.rows = [{ id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'release' }];
    hosts.groups = [{ id: 'release', label: 'Release apps', is_open: true }];
    paint({ view, onSelect: vi.fn() });

    const head = screen.getByRole('button', { name: 'Release apps' });
    expect(head.getAttribute('aria-expanded')).toBe('true');
    fireEvent.click(head);
    expect(
      screen.getByRole('button', { name: 'Release apps' }).getAttribute('aria-expanded'),
    ).toBe('false');
  });
  // Expansion used to be keyed on the label, so renaming a group shut the fold under
  // the reader. It is keyed on the group id, which a rename does not touch.
  it('keeps a group open when the producer renames it mid-run', () => {
    const grouped = (label: string): LiveView => {
      const view = matrixView([]);
      const hosts = hostsTable(view);
      hosts.rows = [{ id: 'ios', cells: ['iOS', 'queued'], tone: 'running', parent: 'release' }];
      hosts.groups = [{ id: 'release', label }];
      return view;
    };
    const { rerender } = render(<LiveViewPanel view={grouped('Release apps')} onSelect={vi.fn()} />);

    fireEvent.click(screen.getByRole('button', { name: 'Release apps' }));
    rerender(<LiveViewPanel view={grouped('Ship it')} onSelect={vi.fn()} />);

    expect(screen.getByRole('button', { name: 'Ship it' }).getAttribute('aria-expanded')).toBe(
      'true',
    );
    expect(screen.getByRole('button', { name: 'Select iOS' })).toBeVisible();
  });

  it('sends the row through the shared View action instead of keeping private selection', () => {
    const view = selectableView();
    const viewAction = vi.fn(async () => ({
      action: 'select' as const,
      is_accepted: true,
      item_ids: ['db-2'],
      node_id: 'hosts',
      view_id: view.id,
    }));
    const client = { viewAction } as unknown as GatewayClient;

    render(<LiveViewList views={[view]} client={client} sid="session-1" />);
    // The list shows the run as one line. Its table is in the picture, one click away.
    expect(screen.queryByRole('button', { name: 'Select db-2' })).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: `Open run ${view.title}` }));
    fireEvent.click(screen.getByRole('button', { name: 'Select db-2' }));

    expect(viewAction).toHaveBeenCalledWith('session-1', view.id, {
      action: 'select',
      node_id: 'hosts',
      item_ids: ['db-2'],
    });
  });
});

// The Companion paints a long table in chunks of rows: the first chunks at once, the
// rest in transition steps. Every row still arrives, and an appended row leaves the
// rows already on screen in place.
describe('a long table', () => {
  const longView = (count: number): LiveView => {
    const view = opened();
    const hosts = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.children : [node]))
      .find((node) => node.id === 'hosts');
    if (!hosts || hosts.type !== 'table') throw new Error('the fixture must hold the hosts table');
    return withNode(view, {
      ...hosts,
      max_rows: 5000,
      rows: Array.from({ length: count }, (_, at) => ({
        id: `host-${at}`,
        cells: hosts.columns.map((_, index) => (index === 0 ? `host-${at}` : `${at}`)),
        tone: 'ok' as const,
      })),
    });
  };

  it('paints every row and keeps the painted rows when one more arrives', async () => {
    const view = longView(1000);
    const mounted = render(<LiveViewPanel view={view} />);
    await waitFor(() => expect(document.querySelectorAll('tbody tr')).toHaveLength(1000));
    const first = screen.getByText('host-0').closest('tr');

    const table = view.nodes
      .flatMap((node) => (node.type === 'group' ? node.children : [node]))
      .find((node) => node.id === 'hosts');
    if (!table || table.type !== 'table') throw new Error('the long view must hold the table');
    const grown = withNode(view, {
      ...table,
      rows: [...table.rows, { id: 'host-new', cells: table.columns.map(() => 'new'), tone: 'ok' }],
    });
    mounted.rerender(<LiveViewPanel view={grown} />);

    await waitFor(() => expect(document.querySelectorAll('tbody tr')).toHaveLength(1001));
    expect(screen.getByText('host-0').closest('tr')).toBe(first);
  });
});

describe('a log the panel shows whole', () => {
  const behind = (total = 500): LiveView =>
    withNode(opened(), {
      id: 'tail',
      type: 'log',
      label: 'Output',
      lines: ['db-2 · 1 critical (openssl)'],
      window_lines: 2000,
      total_lines: total,
      default_expanded: true,
    });

  const record = (total: number) =>
    vi.fn(async (_node: string, from: number, limit: number) => ({
      node_id: 'tail',
      from,
      lines: Array.from({ length: limit }, (_, at) => `line ${from + at}`),
      total,
      matched: total,
      line_numbers: Array.from({ length: limit }, (_, at) => from + at + 1),
    }));

  // A live view shows the LOG, never a tail of it: the panel reads the record back
  // itself and paints it above the window, instead of asking the operator to press
  // for output the view already holds.
  it('reads the record back to its first line on its own', async () => {
    const load = record(500);
    paint({ view: behind(), load });

    await waitFor(() => expect(document.body.innerHTML).toContain('line 0'));
    expect(load).toHaveBeenCalledWith('tail', 0, 499);

    const html = document.body.innerHTML;
    expect(html.indexOf('line 0')).toBeLessThan(html.indexOf('db-2 · 1 critical'));
    expect(screen.queryByRole('button', { name: /earlier lines/ })).toBeNull();
    expect(html).not.toContain('lines scrolled past while you were reading');
  });

  it('keeps reading pages until the whole record is on screen', async () => {
    const load = record(4500);
    paint({ view: behind(4500), load });

    await waitFor(() => expect(document.body.innerHTML).toContain('line 0'));
    expect(load.mock.calls).toEqual([
      ['tail', 2499, 2000],
      ['tail', 499, 2000],
      ['tail', 0, 499],
    ]);
  });

  it('says a failed read and takes the operator at their word to try again', async () => {
    const load = vi
      .fn()
      .mockRejectedValueOnce(new Error('offline'))
      .mockResolvedValue({
        node_id: 'tail',
        from: 0,
        lines: ['line 0'],
        total: 500,
        matched: 500,
        line_numbers: [1],
      });
    paint({ view: behind(), load });

    await waitFor(() => expect(screen.getByRole('alert')).toBeVisible());
    fireEvent.click(screen.getByRole('button', { name: 'Try again' }));
    await waitFor(() => expect(document.body.innerHTML).toContain('line 0'));
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

});

describe('what a run says about its own layout', () => {
  it('paints the marks a human wrote, and nothing a block would bring', () => {
    const panel = (paint(), screen.getByRole('status'));
    expect([...panel.querySelectorAll('code')].map((mark) => mark.textContent)).toContain('db-2');
    expect([...panel.querySelectorAll('strong')].map((mark) => mark.textContent)).toContain(
      'openssl 3.0.13',
    );
    // A row stays a row: no heading, no quote, no list arrived with the marks.
    expect(panel.querySelector('h1, h2, h3, blockquote')).toBeNull();
  });

  // Regression, user report: Activity patches rendered one-by-one on WKWebView and
  // starved the independent elapsed-time paint, leaving its clock visibly frozen.
  it('coalesces a patch burst into one phone paint', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = opened();
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
    await waitFor(() => expect(screen.getByText('0')).toBeVisible());
    vi.useFakeTimers();
    try {
      for (const seq of [1, 2, 3]) {
        act(() =>
          receive?.({
            type: VIEW_PATCH_EVENT,
            kind: 'live',
            view_id: running.id,
            first_seq: seq,
            patch: { view_id: running.id, seq, ops: [] },
          }),
        );
      }

      expect(screen.getByText('0')).toBeVisible();
      act(() => vi.advanceTimersByTime(100));
      expect(screen.getByText('3')).toBeVisible();
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
    await waitFor(() => expect(screen.getByText('1')).toBeVisible());
    vi.useFakeTimers();
    try {
      act(() =>
        receive?.({
          type: VIEW_CLOSE_EVENT,
          kind: 'live',
          view_id: opened().id,
          result: { artifact_id: 'record-1' },
        }),
      );

      expect(onRecordFiled).toHaveBeenCalledTimes(1);
      expect(screen.getByText('0')).toBeVisible();
      act(() => vi.advanceTimersByTime(8_000));
      expect(onRecordFiled).toHaveBeenCalledTimes(5);
    } finally {
      vi.useRealTimers();
    }
  });

  // The settled-Activity-close cases that stood here are gone with the lifecycle
  // they tested: protocol 7 keeps no view alive past its own close, because the
  // form that produced the work carries the terminal snapshot.
  it('repairs a sequence gap from the materialized snapshot', async () => {
    let receive: ((event: SseEvent) => void) | null = null;
    const running = opened();
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
    await waitFor(() => expect(screen.getByText('0')).toBeVisible());
    act(() =>
      receive?.({
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: running.id,
        first_seq: 3,
        patch: { view_id: running.id, seq: 4, ops: [] },
      }),
    );
    await waitFor(() => expect(screen.getByText('4')).toBeVisible());
    expect(liveViews).toHaveBeenCalledTimes(2);
  });

  it('replaces a view with its reconnect snapshot', async () => {
    let reconnect: ((connected: boolean) => void) | null = null;
    const running = opened();
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
    await waitFor(() => expect(screen.getByText('0')).toBeVisible());
    act(() => reconnect?.(true));
    await waitFor(() => expect(screen.getByText('2')).toBeVisible());
    expect(liveViews).toHaveBeenCalledTimes(2);
  });

  // Regression, session c4a1bfb6-58f4-4dd5-ae1d-b1e4d7217ffa: a CI watch patched its
  // view every few seconds, so each reconnect snapshot was thrown away for a newer
  // event, and the patches for a view the panel never mounted were ignored. The run
  // showed no live view until the watch opened its next one.
  describe('snapshot reads that live events overtake', () => {
    function deferredClient(...answers: LiveView[][]) {
      const resolvers: ((views: LiveView[]) => void)[] = [];
      const liveViews = vi.fn(
        () =>
          new Promise<LiveView[]>((resolve) => {
            resolvers.push(resolve);
          }),
      );
      const answer = async (index: number) => {
        await waitFor(() => expect(resolvers.length).toBeGreaterThan(index));
        act(() => resolvers[index](answers[index]));
      };
      return { client: { liveViews } as unknown as GatewayClient, liveViews, answer };
    }

    function hub() {
      let receive: ((event: SseEvent) => void) | null = null;
      const subscriptions = {
        subscribeConnection: () => () => undefined,
        subscribeSession: (_sid: string, listener: (event: SseEvent) => void) => {
          receive = listener;
          return () => undefined;
        },
      } as unknown as SessionSubscriptionHub;
      return { subscriptions, send: (event: SseEvent) => act(() => receive?.(event)) };
    }

    const patchOf = (view: LiveView, seq: number): SseEvent => ({
      type: VIEW_PATCH_EVENT,
      kind: 'live',
      view_id: view.id,
      first_seq: seq,
      patch: { view_id: view.id, seq, ops: [] },
    });

    function Probe({ client, subscriptions }: { client: GatewayClient; subscriptions: SessionSubscriptionHub }) {
      const views = useLiveViews(client, subscriptions, 'session-1');
      return <span>{views.map((view) => `${view.id}@${view.seq}`).join(',') || 'none'}</span>;
    }

    it('keeps a snapshot that a patch overtook and folds the patch over it', async () => {
      const running = opened();
      const { client, answer } = deferredClient([running]);
      const { subscriptions, send } = hub();
      render(<Probe client={client} subscriptions={subscriptions} />);
      send(patchOf(running, 1));
      await answer(0);
      await waitFor(() => expect(screen.getByText(`${running.id}@1`)).toBeVisible());
    });

    it('reads the snapshot again when a patch names a view it never mounted', async () => {
      const running = opened();
      const { client, liveViews, answer } = deferredClient([], [{ ...running, seq: 3 }]);
      const { subscriptions, send } = hub();
      render(<Probe client={client} subscriptions={subscriptions} />);
      await answer(0);
      await waitFor(() => expect(screen.getByText('none')).toBeVisible());
      send(patchOf(running, 4));
      await answer(1);
      await waitFor(() => expect(screen.getByText(`${running.id}@4`)).toBeVisible());
      expect(liveViews).toHaveBeenCalledTimes(2);
    });

    it('never brings back a view that closed while the snapshot was in flight', async () => {
      const running = opened();
      const { client, answer } = deferredClient([running]);
      const { subscriptions, send } = hub();
      render(<Probe client={client} subscriptions={subscriptions} />);
      send({ type: VIEW_CLOSE_EVENT, kind: 'live', view_id: running.id, result: {} });
      await answer(0);
      await waitFor(() => expect(screen.getByText('none')).toBeVisible());
    });
  });
});

describe('live horizontal dividers', () => {
  it('adds and removes a divider without allowing content patches to mutate it', () => {
    let view = liveViewFromWire({ ...fixture, nodes: [{ id: 'break', type: 'divider' }] })!;
    const divider = { id: 'break', type: 'divider' };
    const update = (ops: unknown[]) => {
      const seq = view.seq + 1;
      view = applyLivePatch(view, {
        type: VIEW_PATCH_EVENT,
        kind: 'live',
        view_id: view.id,
        first_seq: seq,
        patch: { view_id: view.id, seq, ops },
      });
    };
    for (const op of ['set', 'append', 'remove', 'clear']) {
      update([{ op, node_id: 'break', label: 'Not a heading', text: 'Not prose' }]);
      expect(view.nodes).toEqual([divider]);
    }
    update([{ op: 'add-node', after: 'break', node_spec: { id: 'second', type: 'divider' } }]);
    expect(view.nodes).toEqual([divider, { id: 'second', type: 'divider' }]);
    update([{ op: 'remove-node', node_id: 'break' }]);
    expect(view.nodes).toEqual([{ id: 'second', type: 'divider' }]);
  });
});
